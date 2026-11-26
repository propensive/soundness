# An edit which keeps the comments

A parsed TEL document holds its comments, blank lines and layout, and never changes. An edit
gives a new document in which one compound differs, and everything a person wrote around it is
still there when it is written back.

```scala fuel
compile
frame
  label A file with a comment and a blank line
  code
      import strategies.throwUnsafely, codepages.utf8Codepage, dynamicAccess.dynamicTel

      val account = t"# The account holder\nname Hamlet\n\nage 30\n".read[Tel]
frame
  label One field replaced
  highlight modify  a new document
  code
      import strategies.throwUnsafely, codepages.utf8Codepage, dynamicAccess.dynamicTel

      val account = t"# The account holder\nname Hamlet\n\nage 30\n".read[Tel]
      val edited = account.modify("name", Tel.scalar(t"Horatio"))
frame
  label Every keyword changed
  highlight transform  applied to every compound
  code
      import strategies.throwUnsafely, codepages.utf8Codepage, dynamicAccess.dynamicTel

      val account = t"# The account holder\nname Hamlet\n\nage 30\n".read[Tel]
      val edited = account.modify("name", Tel.scalar(t"Horatio"))
      val shouted = edited.transform: compound =>
        compound.copy(keyword = compound.keyword.upper)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(shouted.show)
Out.println(account.show)
```

[Editing without destroying the file](../../soundness/modules/tel.md#editing-without-destroying-the-file)
has finer edits, which change a single atom of a line.
