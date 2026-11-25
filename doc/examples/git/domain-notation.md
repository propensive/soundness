# A reference is a path

A fully-qualified reference such as `refs/heads/main` is a path, and is written as one, from
the root `Git.Refs`. A commit relative to `HEAD` is written as Git writes it, by how far back
it is.

```scala fuel
compile
frame
  label A reference
  highlight Git.Refs  the root, refs/
  code
      import strategies.throwUnsafely

      val branch: Git.Ref = Git.Refs / t"heads" / t"second-quarto"
frame
  label A revision, counted back from HEAD
  highlight head(1)  HEAD~1
  code
      import strategies.throwUnsafely

      val branch: Git.Ref = Git.Refs / t"heads" / t"second-quarto"
      val parent: Git.Revision = Git.Revision.head(1)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(branch.encode)
```

[References](../../soundness/modules/git.md#references) lists the kinds of reference, and
[paths](../../soundness/modules/paths.md) the notation which a `Git.Ref` shares.
