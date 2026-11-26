# A hint which is not a repository

The repackager is told where to look for downloads by hints of the form `owner/repo`. A hint
comes from the command line, so it may be anything, and the method which reads one says in its
type that it may refuse.

```scala fuel
compile
frame
  label A hint, taken on trust
  errors show
  code
      def hint(text: Text): GitHub.Repository = GitHub.Repository.parse(text)
frame
  label The refusal, declared
  highlight raises  a malformed hint is the user's mistake
  code
      def hint(text: Text): GitHub.Repository raises Repackager.UserError =
        GitHub.Repository.parse(text)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(safely(hint(t"globe/hamlet")).let(_.show).inspect)
Out.println(safely(hint(t"globe")).let(_.show).inspect)
```

[Errors](../../soundness/modules/packaging.md#errors) lists what else a `UserError` reports,
and [error handling](../../philosophy/error-handling.md) explains the declaration.
