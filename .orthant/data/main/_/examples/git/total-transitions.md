# A commit is conditional on what was read

Other processes write to a repository, so the tip of a branch may move between a read and a
write. `commit` takes the reading of `HEAD` it was based on, and fails if `HEAD` has moved
since, so a commit made without having looked cannot be written.

```scala fuel
compile
frame
  label A commit, on top of whatever is there
  errors show
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def record(folio: Path on Local): Git.Hash =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.add(worktree / "prologue.txt")
          worktree.commit(t"Add the prologue")
frame
  label A commit, on top of what was seen
  highlight Stale[Git.Head]  true when it was read
  highlight head)  the commit fails if HEAD has moved
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def record(folio: Path on Local): Git.Hash =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          val head: Stale[Git.Head] = worktree.head()
          worktree.add(worktree / "prologue.txt")
          worktree.commit(t"Add the prologue", head)
##
```

The example is compiled, and is not run, since it needs a repository.
[Observing a repository that others change](../../soundness/modules/git.md#observing-a-repository-that-others-change)
describes `Stale` values, and [total transitions](../../philosophy/total-transitions.md) why
the write asks for one.
