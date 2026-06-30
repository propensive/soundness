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
  the document, or its structure.
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
implementation details and esoteric features. In the prose, refer to the project as
"Soundness", never by an internal module name.

## Code samples

Write code in idiomatic Soundness Scala, formatted according to `syntax.md`. Use inline
backticks for code, and for the specific names of terms and types; use triple-backtick
blocks for block-level samples.

Samples should be complete — including imports where necessary, though wildcard imports
are fine — and should follow on from one another: a term with a given name means the
same thing throughout a document, and a single identifier is never assigned more than
once.
