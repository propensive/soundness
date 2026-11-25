# A conflict is an outcome

A merge can move a branch forward, make a merge commit, find nothing to do, or stop on a
conflict. None of these is an error. `merge` returns which happened as a value, and a match on
it accounts for all four.

```scala fuel
compile
frame
  label The outcome of a merge
  highlight Git.Merge.Outcome
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def join(folio: Path on Local): Git.Merge.Outcome =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.merge(Git.Branch(t"quarto"), worktree.head(), ff = FastForward.Only)
frame
  label Each of its cases
  highlight Stopped(operation)  carries the paths in conflict, and the ways to go on
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      import Git.Merge.Outcome.*

      def join(folio: Path on Local): Text =
        folio.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.merge(Git.Branch(t"quarto"), worktree.head(), ff = FastForward.Only) match
            case FastForwarded(head) => t"moved forward to $head"
            case Merged(head)        => t"merged as $head"
            case UpToDate            => t"nothing to merge"
            case Stopped(operation)  => t"stopped on a conflict"
##
```

The example is compiled, and is not run, since it needs a repository.
[Merging, cherry-picking, reverting and rebasing](../../soundness/modules/git.md#merging-cherry-picking-reverting-and-rebasing)
says how a stopped operation is continued or abandoned.
