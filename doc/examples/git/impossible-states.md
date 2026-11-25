# A tag is not a branch

A branch, a tag and a commit hash are different types, though each is written as a short piece
of text. An operation which moves `HEAD` to a branch takes a `Git.Branch`, and is not given a
tag by mistake.

```scala fuel
compile
frame
  label Switching to a tag
  errors show
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def rehearse(folio: Path on Local): Unit =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.switch(Git.Tag(t"first-folio"), worktree.head())
frame
  label Switching to a branch
  highlight Git.Branch(t"second-quarto")  the only kind of reference which switch takes
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def rehearse(folio: Path on Local): Unit =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.switch(Git.Branch(t"second-quarto"), worktree.head())
##
```

The example is compiled, and is not run, since it needs a repository.
[References](../../soundness/modules/git.md#references) lists the types, and
[impossible states](../../philosophy/impossible-states.md) explains why they are kept apart.
