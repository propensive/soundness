# A branch with no commits has no tip

In a repository which has just been made, `HEAD` is attached to a branch on which nothing has
been committed. The commit it points to is therefore optional, and the code which wants a hash
says what to use where there is none.

```scala fuel
compile
frame
  label The commit at HEAD, if there is one
  highlight Optional[Git.Hash]  absent in a new repository
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem


      def tip(folio: Path on Local): Optional[Git.Hash] =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.head().value.commit
frame
  label Or the hash which means none
  highlight or(Git.Hash.zero)  forty zeros, as Git writes it
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem


      def tip(folio: Path on Local): Git.Hash =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.head().value.commit.or(Git.Hash.zero)
##
```

The example is compiled, and is not run, since it needs a repository.
[Opening a repository](../../soundness/modules/git.md#opening-a-repository) shows such a
repository, and [optionality](../../philosophy/optionality.md) describes `or`.
