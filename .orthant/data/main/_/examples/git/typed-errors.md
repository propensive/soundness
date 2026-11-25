# A branch name which Git would refuse

Git has rules for the name of a branch, such as that it may not contain `..`. They are checked
when a `Git.Branch` is parsed from text, and the method which parses says in its type that the
name may be refused.

```scala fuel
compile
frame
  label A name from outside, taken on trust
  errors show
  code
      def branch(name: Text): Git.Branch = Git.Branch.parse(name)
frame
  label The refusal, declared
  highlight raises..RefError
  code
      def branch(name: Text): Git.Branch raises Git.RefError = Git.Branch.parse(name)
frame
  label Or turned into an absent value
  highlight safely
  code
      def branch(name: Text): Optional[Git.Branch] = safely(Git.Branch.parse(name))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(branch(t"second-quarto").let(_.show).inspect)
Out.println(branch(t"second..quarto").let(_.show).inspect)
```

[References](../../soundness/modules/git.md#references) has the rules, and
[error handling](../../philosophy/error-handling.md) the reasons for naming the error.
