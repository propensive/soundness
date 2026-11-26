# A deadline which gives an absent value

`await` may be given a duration, after which it stops waiting and raises an error. Under
`safely` that error is an absent value, so a task which is too slow is an `Optional`, with a
default chosen where it is read.

```scala fuel
compile
frame
  label Waiting for as long as it takes
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def answer(): Text = supervise:
        val oracle = async(t"Beware the ides of March")
        oracle.await()
frame
  label Waiting for a tenth of a second
  highlight safely
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def answer(): Optional[Text] = supervise:
        val oracle = async(t"Beware the ides of March")
        safely(oracle.await(0.1*Second))
frame
  label What silence means
  highlight or(t"The..silent")  the default, chosen where it is read
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def answer(): Text = supervise:
        val oracle = async(t"Beware the ides of March")
        safely(oracle.await(0.1*Second)).or(t"The oracle is silent")
##
```

The example is compiled, and is not run, since it waits against a clock.
[Timeouts](../../soundness/modules/concurrency.md#timeouts) describes the deadline, and
[optionality](../../philosophy/optionality.md) the operations on an absent value.
