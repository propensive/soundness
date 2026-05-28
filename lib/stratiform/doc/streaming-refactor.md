# Streaming refactor — design notes

The current `TelParser` is based on `zephyrine.Cursor[Data]` (commit
`002dfe0`) at the entry-point level, but internally it still:

1. Drains the cursor's loader into a single contiguous buffer
   (`drainCursor`) before parsing.
2. Pre-builds a `linesArr: Array[Long]` covering every line of the
   input.
3. Opens **one** outer `cursor.hold:` that lasts for the entire
   `parseDocument` so the parser's `Int` byte offsets stay valid.

That means `Iterator[Data]` inputs work as an API, but we hold the
whole input in memory for the lifetime of the parse. For TEL config
files (typically <10 MB) that's fine; for arbitrarily large inputs we
want the cursor's ring buffer to actually do its job and compact past
consumed bytes between line / token boundaries.

This note captures the architecture for the follow-up that gets us
there.


## Why the global hold blocks compaction

`zephyrine.Cursor.hold` sets `holdStart = pos` when it opens and only
clears it when the **outermost** hold exits. Marks within a hold are
all dropped at outermost-hold exit. So while we're inside the
`parseWithHold:` block, `holdStart` is pinned at 0 and the buffer can
never compact, regardless of how far the parser has advanced.

The fix is to use short-lived holds, not nest a global one. Between
holds, the cursor's `refill()` (triggered by `more`'s slow path) is
free to compact bytes before `pos`. For `Iterator[Data]` inputs this
gives a bounded-memory parse; for pre-filled `Data` inputs no refill
ever triggers, so the buffer just sits there at full size (and that's
fine — the caller already had the whole input in memory).


## Target architecture

Mirror the jacinta substrate (`lib/jacinta/src/core/jacinta.JsonParser.scala`):

### Parser-local snapshot

```scala
private var cursor:    Cursor[Data]       = …
private var heldToken: Cursor.Held | Null = null
private var bytes:     Array[Byte]        = …
private var pos:       Int                = 0
private var bufEnd:    Int                = 0

private inline def syncTo(): Unit =
  cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

private inline def syncFrom(): Unit =
  bytes  = cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]
  pos    = cursor.unsafePos(using Unsafe)
  bufEnd = cursor.unsafeWriteEnd(using Unsafe)

private inline def peek: Int      = bytes(pos) & 0xFF
private inline def advance(): Unit = pos += 1
private inline def more: Boolean   = pos < bufEnd || moreSlow()

private def moreSlow(): Boolean =
  syncTo()
  if cursor.more then { syncFrom(); true }
  else false
```

The hot loops read bytes from the local `bytes` snapshot — the JIT
keeps `pos` and `bytes` in registers across long inner sequences.
`syncTo()` runs before any cursor operation that consults the
cursor's own `pos` (refill, mark, slice, cue); `syncFrom()` runs
after any operation that may have moved the buffer or changed pos
(refill compacts; cue rewinds).

### Per-extraction holds

```scala
private inline def holding[T](inline action: => T): T =
  syncTo()
  cursor.hold:
    heldToken = summon[Cursor.Held]
    try
      syncFrom()
      action
    finally heldToken = null

private def mark(): Cursor.Mark =
  syncTo()
  cursor.mark(using heldToken.nn)

private def slice(start: Cursor.Mark, end: Cursor.Mark): String =
  cursor.slice(start, end): (buf, off, len) =>
    new String(buf.asInstanceOf[Array[Byte]], off, len, StandardCharsets.UTF_8)
```

Every text extraction happens inside its own `holding {…}` block —
roughly one per line of TEL. Between holds, refills can compact;
`bytes` may be reallocated; the next `syncFrom()` refreshes the
snapshot.

### No `linesArr`

The pre-split line array can't survive compaction (Int offsets become
invalid). Replace it with a forward-only line scanner that tracks just:

- `currentLineNumber: Int`
- `currentLineLeadingSpaces: Int`, `currentLineBlank: Boolean`,
  `currentLineSigilLead: Boolean` (whether first non-space byte is the
  sigil — enough to decide comment vs tabulation vs compound)

… and a small previous-line metadata cache for E109:

- `prevLineNumber: Int`
- `prevLineBlank: Boolean`
- `prevLineIsCommentMarker: Boolean`
- `prevLineLeadingSpaces: Int`

E109 only needs the *flags* of the previous line, not its bytes — so
no held bytes carry over between lines.

### Peek + cue-back

Some decisions need to look ahead before committing — `determineMargin`,
`consumeTrailingBlanksFor`, and the schema-aware E107 keyword probe.
The pattern is:

```scala
holding:
  val mk = mark()
  // scan forward, computing what we need
  if shouldKeep then
    () // do nothing — pos stays advanced
  else
    cueTo(mk)  // rewind
```

The hold lifetime covers from the mark through the decision — the
held region is bounded by the longest peek (in practice a few lines
at most, e.g. a long blank run followed by the deciding non-blank).

### Streaming literal-atom payload

`parseLiteralAtom` currently uses `bytesIndexOf(pattern, …)` to find
the `\n<delimiter>\n` closing pattern in the full buffer. In the
streaming version it has to walk the byte stream forward inside a
single hold, accumulating the payload into a `StringBuilder` (or
re-marking and slicing at the close). Because the hold is open for
the whole payload, the buffer holds at most one literal-atom's worth
of bytes at a time — fine unless a single TEL literal is itself
larger than memory, which is exotic.


## Sketch of `parse` flow

```scala
def parseDocument(): Tel.Document raises TelError =
  checkBom()
  val directive = parseInterpreterDirective()  // own hold
  val pragma    = parsePragma()                 // own hold (peek + commit)
  if directive.absent && pragma.absent then determineMargin()
  val children  = parseChildren(parentIndent = -1)
  Tel.Document(directive, pragma, lineEndings, children)

private def parseChildren(parentIndent: Int): IArray[Tel.Block] raises TelError =
  val expected = parentIndent + 1
  val blocks = ArrayBuffer.empty[Tel.Block]
  while peekNextBlockAt(expected) do      // skips blanks, peeks indent
    blocks += parseBlock(expected)
  IArray.from(blocks)

private def parseBlock(indent: Int): Tel.Block raises TelError =
  // Comments — each in its own hold.
  val comments = readComments(indent)
  // Optional tabulation — own hold.
  val tabulation = maybeTabulation(indent)
  // Compound runs — each compound in its own hold; children recurse.
  val compounds = readCompounds(indent)
  // Trailing blanks count attaches per §17.
  Tel.Block(...)
```

`readComments`, `maybeTabulation`, and `readCompounds` each open
short per-line holds, extract their texts via `mark`/`slice`, and
update `prevLine*` before yielding. Recursion into `parseChildren`
inside a compound happens AFTER its hold has closed, so children
process under their own per-line holds.


## What I tried and where it broke

The first attempt at this rewrite (see commit history if it lands as
a WIP branch) hit three classes of issues:

1. **`cursor.slice` returns `addressable.Storage`, not
   `Array[Byte]`** — the lambda receives the abstract storage type;
   the cast inside the `(buf, off, len) =>` lambda must be to
   `Array[Byte]` and the `String` constructor wants `Array[Byte]`
   not `Storage`. Easy fix but the type error confused me at first.
2. **Hard-space-with-embedded-soft-space atom case** — when the
   parser is in hard-space mode and hits a single soft space inside
   an atom, the atom's text needs to include the space. With mark/slice
   we can either keep an "atom-start" mark live across the embedded
   space (works) or accumulate into a `StringBuilder` (also works).
   The mark approach is cleaner.
3. **`consumeTrailingBlanksFor` semantics** — §17 attaches trailing
   blank counts to the preceding block iff the next non-blank line
   is at the same-or-shallower indent (relative to the block). In
   the streaming model this needs a peek-with-cue-back over a blank
   run plus the first non-blank line, all inside one hold.

None of these are structural blockers; they just need patient
implementation with the right hold scope for each operation.


## Suggested next steps

1. Build the snapshot/peek/advance/more/holding/mark/slice
   infrastructure first; verify the current tests still pass with the
   parser's body unchanged (i.e. the infra is unused initially).
2. Migrate `checkBom`, `parseInterpreterDirective`, `parsePragma`,
   `determineMargin` one at a time. Each is a small forward-only
   piece.
3. Migrate `parseCompoundLine` next — it's the most-allocated path
   and the keyword-cache lookup needs to flow through `cursor.slice`'s
   `(buf, off, len) =>` lambda (`internKeywordFromBytes(buf, off,
   len)` is already shaped this way).
4. Migrate `parseChildren` / `parseBlock`, replacing
   `peekNextNonBlankLine` + `lineAt(idx)` with the
   peek-with-cue-back pattern.
5. Migrate `parseLiteralAtom` (streaming payload accumulation) and
   `parseSourceAtom` (per-line holds).
6. Migrate the tabulation pipeline.
7. Drop `linesArr`, `linesLen`, `buildLines`, `drainCursor`, and the
   global `parseWithHold` hold. Verify benchmarks — for
   `Iterator[Data]` inputs the memory profile should now be bounded
   by the live region inside the longest hold.

The change is structurally feasible — no Zephyrine API additions are
needed — but it's a multi-day effort to land correctly across the
445-test corpus. It belongs in its own dedicated PR.
