# A file in a worktree is a file

An open worktree is also a directory. A path formed from it is written with the same
`overwrite` as any other file, under the grants the worktree was opened with, and the same
path is what `add` stages.

```scala fuel
compile
frame
  label A file, written into the worktree
  highlight overwrite  a filesystem operation
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def record(folio: Path on Local): Unit =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          (worktree / "prologue.txt").overwrite(t"Two households, both alike in dignity")
frame
  label Staged and committed
  highlight worktree.add  a Git operation, on the same path
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def record(folio: Path on Local): Git.Hash =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          val head = worktree.head()
          (worktree / "prologue.txt").overwrite(t"Two households, both alike in dignity")
          worktree.add(worktree / "prologue.txt")
          worktree.commit(t"Add the prologue", head)
##
```

The example is compiled, and is not run, since it needs a repository.
[Making a commit](../../soundness/modules/git.md#making-a-commit) describes the commit, and
[filesystem](../../soundness/modules/filesystem.md) the operations on the file.
