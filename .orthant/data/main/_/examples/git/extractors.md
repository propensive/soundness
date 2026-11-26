# Where HEAD is, taken apart

`HEAD` is either attached to a branch or detached at a commit. A reading of it is matched as
one or the other, and each pattern binds what that case has: a branch and perhaps a tip, or a
hash.

```scala fuel
compile
frame
  label A reading of HEAD
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem


      def position(folio: Path on Local): Git.Head =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.head().value
frame
  label Its two cases
  highlight Git.Head.Attached(branch,  binds a Git.Branch, not text
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem


      def position(folio: Path on Local): Text =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.head().value match
            case Git.Head.Attached(branch, tip) => t"on $branch"
            case Git.Head.Detached(at)          => t"detached at $at"
##
```

The example is compiled, and is not run, since it needs a repository.
[Observing a repository that others change](../../soundness/modules/git.md#observing-a-repository-that-others-change)
describes the reading.
