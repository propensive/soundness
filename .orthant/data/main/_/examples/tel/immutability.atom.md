# One atom of one line, changed

A mutation names the place it changes by a pointer, and changes that and nothing else. It
gives a new document in which one atom of one line differs, and the comment above the line and
the blank line below it are as they were.

```scala fuel
compile
frame
  label A file as a person wrote it
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      val account = t"# The account holder\nname Hamlet\n\nage 30\n".read[Tel]
frame
  label A pointer, and an edit at it
  highlight UpdateAtom(holder..t"Horatio")  the first atom of that line
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import Tel.Mutation.Op.UpdateAtom

      val account = t"# The account holder\nname Hamlet\n\nage 30\n".read[Tel]
      val holder = Tel.Pointer.of(t"name")
      val edited = Tel.Mutation(account, List(UpdateAtom(holder, 0, t"Horatio")))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(edited.show)
```

[Editing without destroying the file](../../soundness/modules/tel.md#editing-without-destroying-the-file)
lists what else a mutation may do.
