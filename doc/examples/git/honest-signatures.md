# A reading which says it may be out of date

Other processes write to a repository, so the history of a branch is true only at the moment
it is read. `log` returns it as a `Stale` value, and code which wants the list must take it out
with `value`, in sight of the fact.

```scala fuel
compile
frame
  label The history, as though it could not change
  errors show
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem


      def history(folio: Path on Local): List[Commit] =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.log()
frame
  label The history as it was when it was read
  highlight .value  what was true at the read
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem


      def history(folio: Path on Local): List[Commit] =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.log().value
##
```

The example is compiled, and is not run, since it needs a repository.
[Inspecting history](../../soundness/modules/git.md#inspecting-history) says which reads
are stale and which are not, and [honest signatures](../../philosophy/honest-signatures.md) why
the type admits it.
