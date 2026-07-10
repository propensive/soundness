# Naming

Names in Soundness are chosen with care, because a well-named thing reads correctly and
a badly-named one obscures every line that uses it. Methods, types, and contextual
values are named so that the expressions they form read as natural language, and
consistent conventions mean a name carries the same meaning wherever it appears. Good
naming is inseparable from making code read like [elegant prose](elegant-prose.md): the
right name in the right place removes the need for a comment to explain it.

Names are also unique across the whole of Soundness. Every public type has one meaning
in the `soundness` namespace: there is one `Path`, one `Message`, one `Error`, and a
module needing a distinct concept coins a distinct name — `Teletype`, not another
`String`; `GitHash`, not another `Hash`. Uniqueness is what makes the umbrella import
possible: `import soundness.*` never forces a choice between two things called the same,
a bare name in documentation or an error message is unambiguous, and moving code between
modules never silently changes which `Path` it means. Where the same word genuinely
suits two things, one of them is renamed rather than qualified forever.

Descriptiveness extends to the smallest names. Type parameters are words, not letters:
a method is generic over `element`, `format`, `duration` or `plane` — never `A`, `B`,
`T` — so a signature reads as a sentence about the kinds of thing involved, and its
constraint clauses (`element: Encodable in Json`) read as statements about them. The
convention costs a few characters per declaration and repays them at every reading,
which is the trade naming should always make.
