# Impossible States

The first of Soundness's two golden rules: the types of a program should admit only
states that are valid, so that an impossible state cannot even be written down.
Rather than constructing a value and then checking whether it is legal, the type makes
an illegal value impossible to construct, moving the error from runtime to compiletime.
A type that can hold only meaningful values needs no defensive checks downstream,
because the compiler has already ruled out everything else. This pairs with the
companion rule that transitions between states should be [total](total-transitions.md).
