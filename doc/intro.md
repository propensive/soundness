Dissonance implements Eugene Myers' diff algorithm in Scala as a pure function
on immutable data structures. Using it is as simple as calling
`diff(left, right)`, where `left` and `right` are sequences of like-typed data
to be compared; the result is an instance of `Diff`, a sequence of additions,
deletions and no-change nodes representing each item in the left and right
sequence.


