# Every result of an upgrade has a name

The launcher which verifies an upgrade does not report success or failure. It records one of
seven results, and the application which reads the record answers each of them.

```scala fuel
compile
frame
  label Applied, or not
  code
      import Upgrade.Outcome.Result

      def advice(result: Result): Text = result match
        case Result.Applied => t"upgraded"
        case other          => t"not upgraded"
frame
  label Each way of not being applied
  highlight Result.BadSignature  a different answer from a release which is merely old
  code
      import Upgrade.Outcome.Result

      def advice(result: Result): Text = result match
        case Result.Applied                            => t"upgraded"
        case Result.NotNewer                           => t"already up to date"
        case Result.BadSignature                       => t"refused: the signature does not verify"
        case Result.WrongApplication | Result.NoRecord => t"refused: not a release of this program"
        case Result.Disabled | Result.SwapFailed       => t"this installation cannot be upgraded"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(advice(Result.NotNewer))
Out.println(advice(Result.BadSignature))
```

[Staging an upgrade](../../soundness/modules/daemons.md#staging-an-upgrade) says what each
result means, and [total transitions](../../philosophy/total-transitions.md) why none of them
is an exception.
