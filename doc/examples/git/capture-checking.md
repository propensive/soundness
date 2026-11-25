# A worktree's handle belongs to the block which opened it

A Git repository is opened like a file: the handle exists for a block, with the grants which were
asked for. A function which would query the repository later would do so through a handle which
is no longer open, so it cannot be returned. The commit it found is a value, and can.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A query kept for later
  errors show
  highlight worktree.head().value  would ask after the repository had closed
  code
      import gitBackends.commandlineGit, gitCommands.searchpathGitCommand, logging.silentLogging
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem, strategies.throwUnsafely
      import temporaryDirectories.javaBaseTemporaryDirectory, filesystemBackends.javaBaseFilesystem
      import workingDirectories.javaBaseWorkingDirectory, environments.javaBaseEnvironment
      import codepages.utf8Codepage

      val folio = temporaryDirectory[Path on Local] / "folio"

      def head() = folio.open[Git.Worktree](): worktree ?=>
        () => worktree.head().value
frame
  label The query, made while the repository is open
  highlight worktree.head().value
  code
      import gitBackends.commandlineGit, gitCommands.searchpathGitCommand, logging.silentLogging
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem, strategies.throwUnsafely
      import temporaryDirectories.javaBaseTemporaryDirectory, filesystemBackends.javaBaseFilesystem
      import workingDirectories.javaBaseWorkingDirectory, environments.javaBaseEnvironment
      import codepages.utf8Codepage

      val folio = temporaryDirectory[Path on Local] / "folio"

      def head() = folio.open[Git.Worktree](): worktree ?=>
        worktree.head().value
##
```

The example is compiled, and is not run. [Git](../../soundness/modules/git.md) describes what a
worktree's handle offers.
