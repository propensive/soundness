# A path literal, checked as it is compiled

A path written as `p"…"` is read by the compiler, which decides from its shape whether it is
a POSIX path or a Windows one. Text which is neither is a compile error.

```scala fuel
compile
frame
  label Neither a root nor a drive
  errors show
  code
      val letters = p"home/viola/letters"
frame
  label A POSIX path
  highlight /home  a root makes it a POSIX path
  code
      val letters = p"/home/viola/letters"
frame
  label Its names are in its type
  highlight Path..Posix  the leaf comes first
  code
      val letters: Path of ("letters", "viola", "home") on Posix =
        p"/home/viola/letters"
##
```

[Building a path](../../soundness/modules/paths.md#building-a-path) has the literal, and
[safety by construction](../../philosophy/safety-by-construction.md) the principle.
