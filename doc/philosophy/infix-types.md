# Infix Types

Soundness uses infix types throughout, so that a type reads as a phrase rather than a
tangle of brackets. Written as prepositions and verbs — `Path on Linux`, `Html of
"br"`, `Text is Decodable in Json`, `Quantity is Multiplicable by Duration`, a method
that `raises HttpError` or `logs ExecEvent` — they let a signature say in plain words
what a value is and what an operation needs, which is part of making code read like
[elegant prose](elegant-prose.md). Their real power is that they
[compose](composability.md): each adds one more clause to a type without nesting, so
`Element of "ul" over "li" in Whatwg` builds up exactly as an English description
would, and the same `of`, `in`, `by`, or `over` carries the same meaning wherever it
appears. A reader who has understood one such type can read the next by analogy, and a
writer can describe a precise type by naming its parts in turn rather than assembling a
deeply parameterised one.
