# Standard directories, by name

The conventional directories of a system are nested objects, named as the directories are.
`~/.local/bin` is `Home.Local.Bin`, and a directory which is misspelled is a member which
does not exist.

```scala fuel
compile
frame
  label A directory which is not there
  errors show
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem
      import pathInterfaces.pathOnLocal, strategies.throwUnsafely

      def cache(): Path on Local = Home.Cashe[Path on Local]()
frame
  label The cache
  highlight Home.Cache  ~/.cache, unless the user has moved it
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem
      import pathInterfaces.pathOnLocal, strategies.throwUnsafely

      def cache(): Path on Local = Home.Cache[Path on Local]()
frame
  label Deeper
  highlight Home.Local.Bin  ~/.local/bin
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem
      import pathInterfaces.pathOnLocal, strategies.throwUnsafely

      def cache(): Path on Local = Home.Cache[Path on Local]()
      def programs(): Path on Local = Home.Local.Bin[Path on Local]()
##
```

The example is compiled, and is not run, since its results are paths on the machine it runs
on. [Standard directories](../../soundness/modules/filesystem.md#standard-directories) lists
them all.
