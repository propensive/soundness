# Direct Style

Soundness is written in direct style: code reads as a straightforward sequence of
steps, not as a chain of combinators threading a value through a monad. Effects such
as failure, asynchrony, and context are carried by capabilities and by types rather
than by wrapper types that have to be mapped and flat-mapped together. The result
composes as ordinary code composes — with calls, blocks, and local values — so the
shape of a program follows the shape of the problem instead of the shape of an
abstraction laid over it.
