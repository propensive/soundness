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

Before writing, decide who opens the page and why. A reader comes to a tutorial to get
something done — read a configuration file, call a service, make sense of an error — and reads
with questions the page should answer in the order they arise: what is the least that works,
what happens when it fails, what does it cost, how does it fit with what is already in use. Put
the commonest task first, and answer the worry before the reader has to go looking.

Prefer short sentences that each make one point. A sentence that needs two semicolons or a
parenthesis inside a dash is two or three sentences.

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
  if it compiles after all; fuel shows its error as a note on the line. New samples use an
  `errors show` fence instead, for the reason given under "Showing, not telling".
- A fence that says more than its code does so in a TEL preamble: it opens with
  ` ```scala fuel `, and the preamble ends at a line holding `##`. Its directives are below.

The preamble directives:

```tabular
column  Directive
column  Use it for
column  Meaning
row  `typecheck`  a fence that cannot run in a REPL session: one that opens a browser, binds a socket, reads a terminal, dispatches a JVM  fuel compiles it, and the REPL checker leaves it alone
row  `errors show`  a fence whose point is its error  the error is rendered beneath it
row  `run`  a fence whose point is what it prints, or the error it throws  fuel compiles it and runs it, and shows its output beneath it; the REPL checker leaves it alone
row  `highlight`  the part of a fence the prose is about  the range is underlined, with a caption beside it
row  `error`, `caution`  a part of a fence that is wrong, or risky, by design  the range is marked as the compiler would mark it, with the caption given
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

### Showing, not telling

A sample shows what the code does; it does not describe it. A comment in a sample is the writer
talking over the code, so a sample has none. Each thing
a comment used to say has a better place:

```tabular
column  Instead of a comment saying
column  Write
row  what an expression evaluates to  a `run` fence which prints it, so the real output appears beneath
row  what a line is for  a `highlight` with a caption, or a sentence before the fence
row  why code does not compile  an `errors show` fence, so the compiler's own message is shown
row  what a program throws  a `run` fence with `errors show`
row  that something is risky or discouraged  a `caution` mark, or a sentence before the fence
```

A fence that prints needs a `Stdio` in scope, so a tutorial with `run` fences imports one once,
with its first imports, and says in a clause what the line is for:

```scala fuel
parse
##
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

A `run` fence prints with `Out.println`. Print a value with `show` where it has a `Showable`
instance, since that is the form a user would display, and with `inspect` otherwise — a
`Boolean`, a case class, a nested structure. Print the values a reader would want to see and no
more: one fence with three lines of output teaches more than three fences with one.

Prefer the compiler's words to a paraphrase of them. A reader who meets an error in their own
code searches for its text, so a sample that fails shows the message they will see. A message of
one line appears as a note on the line; a longer one appears as a card beneath the fence. A
failing sample is a fence of its own, holding only the code that fails:

```tabular
column  Form
column  Use it when
row  `compile` and `errors show`  the code does not compile
row  `run` and `errors show`  the code compiles, and fails when it runs
```

Keep a failing expression minimal: the one call that is wrong, without the calls that would
follow it. Avoid the `// does not compile` marker in new samples. A fence that holds one never
joins the session, and every marker line and `run` fence after it then fails for want of that
fence, which fuel counts as the failure it was told to expect — so a line which compiles
perfectly well can pass as one which does not.

Check what a sample shows as well as that it passes. Read the captured output and each error
message in the rendered page: a sample can compile, run, and still print something other than
what the prose around it says.

The session has a few rules of its own, which a tutorial's fences must respect:

- A definition which later fences use goes in a plain fence, and a `run` fence only prints. The
  REPL checker skips every fence which names a mode, so a definition inside one is lost to it.
- A name defined in a tutorial — an enumeration's cases, a companion's members — is not carried
  into a later fence by an `import` of it. Write `Color.Red` in later fences, or repeat the
  import in the fence that needs it.
- Name a given whose type starts with a type defined in the tutorial:
  `given personParsable: (Person is Json.Parsable)`. An anonymous `given Person is …` hides
  `Person` from every later fence.
- A given which must be found while another is derived is declared in the fence that uses it,
  inside a `locally` block, as an import of a different policy is.
- A fence whose code depends on a macro or an inline given being expanded needs `compile`, since
  `typecheck`, the default, stops before expansion.
- An import which cannot be resolved breaks every fence after it, because a failed fence still
  leaves its imports behind. Fix the first error in a tutorial before reading the others.

`inspect` writes a present optional value between corner brackets and an absent one as `○`, and
a floating-point number has no `show` without a `Decimalizer` in scope, so it is printed with
`inspect` too. Say what the notation means where a tutorial first shows it.

A `run` fence which touches the world outside the program — the disk, a subprocess — creates
what it needs within itself, and does not rely on what an earlier `run` fence left behind. Fuel
runs a fence again only when the fence changes, so on a partial rebuild an earlier fence's
effects may be absent, or belong to a different temporary directory. Give such a tutorial one
fixed working location, write to it with flags which replace what is there, and print results,
never paths: an absolute path in captured output, or in a captured error message, is the path
on whichever machine built the documentation. For the same reason, do not `run` a fence which
prints the real environment, user name or host name; give it a fixed environment instead.

A fence that cannot run where the documentation is built — one that needs the network, a
terminal, a browser or a running service — stays `typecheck`, and the prose says what it would
do. Never write the output of such a fence by hand as though it had been captured.

## Links

A tutorial is rarely read alone. The reader arrives with something to get done, and the thing
they need next is often on another page, so link to it at the point they would want it:

- a concept another tutorial teaches, at its first use in each section, with the section's anchor
  where the target is one section of a long page: `[strategy](errors.md#choosing-a-strategy)`;
- a combination of two features, from both pages. Where an instance joins one module's type to
  another's typeclass — a date that is encoded in JSON, a quantity that is shown in a table, a
  path that is a command-line parameter — each tutorial gives the combination a short section or
  a sentence, with a sample, and links to the other;
- a design idea, to its essay in `doc/philosophy/`, once, where the design is being explained;
- an API entity, by naming it in inline code with its sigil (`` `#Json` `` for a type,
  `` `.fetch` `` for a term), which fuel resolves and links.

Link text is the name of the thing linked to, never "here" or "this page". `fuel compile` checks
that every link has a target.

## Glossary

A word with a precise meaning in Soundness — *tactic*, *strategy*, *capability*, *choice
package* — is defined in `doc/glossary.tel`, and written in brackets where a reader may not yet
know it: `[tactic]`, or `[strategies][strategy]` for an inflected form. Fuel shows the definition
when the term is pointed at, and links it to the page named in its entry.

- Mark a term at its first use in a section, not at every use.
- A definition is one to three plain sentences that would make sense to a reader who has read
  nothing else, and it names the page that explains the term in full as its `link`.
- Add a term when a tutorial relies on a word whose everyday meaning would mislead, or which
  Soundness uses more narrowly than Scala does. Ordinary Scala vocabulary a reader is assumed to
  know — *trait*, *case class* — is not defined.
- Fuel does not report a bracketed term with no definition; it is shown as plain bracketed
  text, so search the tutorial for bracketed words before committing.

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
