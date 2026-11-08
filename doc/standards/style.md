# Writing Soundness Documentation

The documents in `doc/modules/` are tutorials, each introducing one feature of
Soundness — a capability that may span several internal modules — to a reader meeting
it for the first time. This guide sets out how to write them: who they address, the
voice they use, and the conventions they follow.

## Audience and scope

Assume a reader with a basic understanding of Scala and of the subject at hand, but
pace the prose for someone meeting Soundness's treatment of it for the first time.

Explain Soundness's own implementations in depth. The concepts Soundness builds on —
JSON, regular expressions, time zones — need not be taught from scratch, but give
enough of each to motivate the design that follows. Link complex or esoteric
background to an external resource, such as Wikipedia, rather than explaining it
inline.

Limit terms and phrases to those familiar to both British and American readers.

## Voice: Classic Style

Write in Classic Style, as described by Steven Pinker: the writer shows the reader
something true or interesting in the world and helps them see it clearly, as a
conversation between equals. The reader is intelligent and capable. Truth can be
perceived and described plainly, and success is measured by whether the reader grasps
the point without effort.

- **Show, don't announce.** Present the idea itself, not your intention to present it.
  Avoid throat-clearing ("In this section…", "It is important to note that…") and
  signposting ("Firstly…", "In conclusion…").
- **Stay out of view.** Avoid first-person pronouns and meta-discussion of the writing,
  the document, or its structure. Do not refer to the document by its genre: never
  justify a choice as one that "suits a tutorial" or is made "for this guide". State
  the choice directly instead — write "the `throwUnsafely` strategy raises an exception
  on failure", not "raising an exception suits a tutorial".
- **Be plain and direct.** Prefer straightforward phrasing over ornate, inflated, or
  needlessly technical wording. Cut clutter, hedging, and unnecessary qualifiers.
- **Project confidence.** State things plainly rather than hedging or apologising;
  avoid academic over-qualification.
- **Ground the abstract.** Reach for concrete imagery and examples to anchor abstract
  ideas.
- **Guide the reader's gaze.** Structure each sentence so attention falls naturally on
  what matters most.
- **Sound like someone who knows the subject.** The voice is calm, knowledgeable, and
  unstrained; the prose feels effortless even when heavily revised.
- **Avoid devices that depend on shared context.** State the point directly rather than
  through rhetorical questions or allusion. Avoid bureaucratic prose, excessive passive
  voice, and pretentious diction.

## Emphasis

Draw attention to the typesafety and compiletime checking Soundness performs, and
explain the design ideas behind each API — especially those that may be unconventional.
Occasionally connect these to Soundness's general design philosophy, so the reader sees
not just how a feature works but why it is shaped as it is.

## Language and spelling

Use American English spelling. Always write "runtime" and "compiletime" as single
words, never as "run time", "run-time", "compile time", or "compile-time".

## Document structure

Begin with a `## Title`, then up to three introductory paragraphs giving an overview of
the subject's scope. Within these, an `### About` section gives the overview and an
`### On <topic>` section motivates the design. After the introduction, cover the
individual topics in their own `###` sections.

Prioritise foundational details and the most commonly used features; deprioritise
implementation details and esoteric features.

The reader already knows the subject is Soundness, so do not make "Soundness" the
reflexive subject of every sentence, least of all the opening one. Lead with the
capability or the thing itself — "Regular expressions are checked as the code compiles",
not "Soundness checks regular expressions as the code compiles". Name the project where
it genuinely marks a contrast — what Soundness does against what other libraries do — and
when you do name it, call it "Soundness", never an internal module name.

## Code samples

Write code in idiomatic Soundness Scala, formatted according to `syntax.md`. Use inline
backticks for code, and for the specific names of terms and types; use triple-backtick
blocks for block-level samples.

Samples should be complete — including imports where necessary, though wildcard imports
are fine — and should follow on from one another: a term with a given name means the
same thing throughout a document, and a single identifier is never assigned more than
once.

Every sample is checked mechanically, twice over, and both checkers follow the fences in
order as one session:

```tabular
column  Command
column  Checks
row  `make fuelcheck`  compiles each tutorial with fuel against `main`'s own test assembly (`.pyrocosm/fuel/config.tel`)
row  `make doccheck`  evaluates each fence through the flame REPL (`etc/doccheck.py`) against the latest release, and checks the names used against the source (`etc/doccheck-names.py`)
row  `make doccheck DOC=json`  the same, for one tutorial
```

A few conventions make that possible:

- A sample must introduce every name it uses, in a fence of the same tutorial, so the fences
  form one continuous session. Where a tutorial needs a value it cannot sensibly construct
  in prose — a path that must exist, a running service — put the binding in
  `doc/fixtures/<tutorial>.scala`, which both checkers compile before the first fence.
- A line that is meant not to compile carries a trailing `// does not compile` comment (with
  an optional explanation after a colon). Both checkers compile it on its own and report it
  if it compiles after all; fuel shows its error as a note on the line.
- A fence that says more than its code does so in a TEL preamble: it opens with
  ` ```scala fuel `, and the preamble ends at a line holding `##`. Its directives are below.

The preamble directives:

```tabular
column  Directive
column  Use it for
column  Meaning
row  `typecheck`  a fence that cannot run in a REPL session: one that opens a browser, binds a socket, reads a terminal, dispatches a JVM  fuel compiles it, and the REPL checker leaves it alone
row  `errors show`  a fence whose point is its error  the error is rendered beneath it
row  `parse`  a sketch that is not meant to compile at all  it is only parsed
row  `feature experimental.captureChecking`  the first fence that needs a language feature  the feature is enabled from there on
row  `restart`  a fence that must not follow the ones before it  the session begins afresh
row  `frame`  an animation  each `frame` has its `code` (and perhaps a `label`); later fences follow the last
row  `alternative`  versions of the same code, one under each tab  each has its `code` (or its `frame`s); later fences follow the first
row  `substitute`  code that must be there to compile but is noise to the reader  the range is shown as `...`, or the text given
row  `focus`  one region of a long fragment, or of one frame, that the prose is about  everything else in the block fades
row  `path`  a fence holding the contents of a file  the file's name is shown above it, and a `run` fence can read it
```

Use an animation only where watching the code change from one frame to the next teaches
something that two fences side by side would not: each frame builds on the last. Where the
versions are instead choices made at the same point — the same program under another policy
import, backend or calendar, or the same thing written two ways — make them alternatives, with
a tab title of a word or two (`Julian`, `RE2`, `Imported`). Only the first alternative joins
the session, so put first the one the rest of the tutorial uses, and nothing after a `##` in
such a fence. `etc/doccheck.py` runs the first alternative too.

A `substitute` names its range as a mark does — a literal, or `start..end`, refined by `line`
and `occurrence` — and is for a `???` that stands for something supplied, a long key or literal
whose content is not the point, or setup repeated in every frame. Don't substitute code the
prose discusses.

## Tables

Fuel does not support Markdown pipe tables. A table is a fence marked `tabular`, written in
TEL: a `column` line per column (with an indented `title` and `align` where needed), a
`row` line per row, and an optional `caption`. Fields are separated by two spaces, so a
cell may hold single spaces but never two in a row. A row with an empty cell takes the long
form, `row` alone with an indented `cell` line per cell, a bare `cell` for the empty one.
Cells are Markdown phrasing, and code containing a backtick needs a double-backtick span.

```tabular
column  Glyph
column  Meaning
column  Example
row  `↗`  superscript  `x↗2` is *x²*
row  `↘`
  cell  subscript
  cell
```

Use a table when the items share attributes: an enumeration, a mapping, a comparison, the
reasons an error can carry, the choices made by an import. Explanation — why, trade-offs,
caveats — stays in prose. Introduce a table with a sentence, as a sample is introduced, and
keep each cell to a phrase. `fuel compile` validates every table.

The format pages share three layouts, so that every format reads the same:

```tabular
column  Layout
column  Columns
row  Optional-field policies  *A field which is* · *Reads as* · *Unless*
row  Codec modes  *Import* · *For a record* · *For any other type*
row  Output forms  *Form* · *Serializes on* · *Hands out*
```
