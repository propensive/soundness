# The same history, with or without git

The operations on a repository are carried out by a backend, and the backend is an import. One
runs the `git` command, with everything that brings. The other reads the files of the
repository itself, and needs no `git` to be installed.

```scala fuel
compile
alternative The native backend
  highlight soundnessGit  reads the object database directly
  code
      import gitBackends.soundnessGit, strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def history(folio: Path on Local): List[Text] =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.log().value.map(_.message)
alternative The git command
  highlight commandlineGit  runs git, found on the search path
  code
      import gitBackends.commandlineGit, gitCommands.searchpathGitCommand, logging.silentLogging
      import workingDirectories.javaBaseWorkingDirectory, environments.javaBaseEnvironment
      import strategies.throwUnsafely, codepages.utf8Codepage
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem

      def history(folio: Path on Local): List[Text] =
        folio.open[Git.Worktree](): worktree ?=>
          worktree.log().value.map(_.message)
##
```

The example is compiled, and is not run, since it needs a repository.
[Choosing a backend](../../soundness/modules/git.md#choosing-a-backend) compares the two, and
[declarative context](../../philosophy/declarative-context.md) explains why the choice is made
by import.
