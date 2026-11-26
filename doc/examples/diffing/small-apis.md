# A difference of anything

`diff` is not a function of text. It compares two sequences of any element, and the edits it
gives hold elements of that type, so characters, lines and records are compared by one method.

```scala fuel
compile
alternative Characters
  highlight diff  between two sequences of `Char`
  code
      val spelling = diff
        ( Sequence('h', 'o', 'n', 'o', 'r'),
          Sequence('h', 'o', 'n', 'o', 'u', 'r') )
alternative Lines
  highlight diff  between two sequences of `Text`
  code
      val revision = diff
        ( Sequence(t"To be, or not to be,", t"that is the question"),
          Sequence(t"To be, or not to be,", t"ay, there's the point") )
alternative Records
  highlight diff  between two sequences of `Role`
  code
      case class Role(name: Text, lines: Int)

      val recasting = diff
        ( Sequence(Role(t"Hamlet", 1506), Role(t"Yorick", 0)),
          Sequence(Role(t"Hamlet", 1506)) )
##
```

[Computing a diff](../../soundness/modules/diffing.md#computing-a-diff) describes the edits,
and [small APIs](../../philosophy/small-apis.md) why one name serves.
