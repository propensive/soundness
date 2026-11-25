# The first element of a list which has one

`head` throws on an empty list in the standard library. Here a list written out with its
elements is typed as populated, and its `head` is an element. A list of unknown length gives
an optional one, and nothing throws.

```scala fuel
compile
frame
  label A list whose length is known
  highlight Text  not optional: the list is known to be populated
    line 2
  code
      val cast = List(t"Lear", t"Kent", t"Fool")
      val lead: Text = cast.head
frame
  label A list which may be empty
  highlight Optional[Text]  the list may be empty
  code
      def lead(cast: List[Text]): Optional[Text] = cast.head
frame
  label An answer for the empty one
  highlight or  the answer for an empty list
  code
      def lead(cast: List[Text]): Text = cast.head.or(t"nobody")
##
```

[The types](../../soundness/modules/collections.md#the-types) introduces populated lists, and
[total transitions](../../philosophy/total-transitions.md) the principle.
