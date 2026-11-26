# A repository opened for reading is not written to

A worktree is opened for a block, and the mode it is opened with names what the block may do.
A handle which was granted `Read` has no method which stages a file, so a block which only
inspects a repository cannot change it.

```scala fuel
compile
frame
  label Staging a file through a handle which may only read
  errors show
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def stage(folio: Path on Local): Unit =
        folio.open[Git.Worktree](Read): worktree ?=>
          worktree.add(worktree / "prologue.txt")
frame
  label The grant, asked for
  highlight Write  the block may now change the repository
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def stage(folio: Path on Local): Unit =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.add(worktree / "prologue.txt")
##
```

The example is compiled, and is not run, since it needs a repository.
[Opening a repository](../../soundness/modules/git.md#opening-a-repository) describes the two
forms of handle, and [delimited scopes](../../philosophy/delimited-scopes.md) the idea of a
block which holds a grant.
