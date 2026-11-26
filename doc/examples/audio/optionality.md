# A machine with no microphone

The inputs of a machine are a list, and the list may be empty. Its first element is therefore
an optional value, and code which records from it must say what happens when there is none.

```scala fuel
compile
frame
  label The first input, used as though there were one
  errors show
  code
      def microphone: Text = Audio.Feed.list.head.name
frame
  label Its name, where there is one
  highlight let  applies only to a value which is present
  code
      def microphone: Optional[Text] = Audio.Feed.list.head.let(_.name)
frame
  label And a name for none
  highlight or(t"silence")  what absence means here
  code
      def microphone: Text = Audio.Feed.list.head.let(_.name).or(t"silence")
##
```

The example is compiled, and is not run, since it asks for the devices of the machine.
[Recording](../../soundness/modules/audio.md#recording) records from a feed, and
[optionality](../../philosophy/optionality.md) describes `let` and `or`.
