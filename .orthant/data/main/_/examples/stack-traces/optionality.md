# The cause of a failure, if it has one

An exception may have been caused by another, or by nothing. The `cause` of a `StackTrace` is
an `Optional[StackTrace]`, so it cannot be read as though it were always there, and there is no
`null` to forget to check.

```scala fuel
compile
frame
  label A cause is taken for granted
  errors show
  code
      val murder = Exception("murder most foul")
      val trace: StackTrace = Exception("the time is out of joint", murder).stackTrace

      val reason: Text = trace.cause.message.text
frame
  label Absence is given a meaning
  highlight let  only where there is a cause
  highlight or(t"no..cause")  what an exception without one shows
  code
      val murder = Exception("murder most foul")
      val trace: StackTrace = Exception("the time is out of joint", murder).stackTrace

      val reason: Text = trace.cause.let(_.message.text).or(t"no cause")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(reason)
Out.println(murder.stackTrace.cause.let(_.message.text).or(t"no cause"))
```

[Capturing a trace](../../soundness/modules/stack-traces.md#capturing-a-trace) describes the
rest of the value, and [optionality](../../philosophy/optionality.md) the union behind
`Optional`.
