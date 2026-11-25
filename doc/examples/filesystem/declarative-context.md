# Whether a move may overwrite

Moving a file onto a path which is taken either destroys what is there or refuses to, and
neither is right for every program. `moveTo` therefore has no default: it does not compile
until an import answers the question.

```scala fuel
compile
frame
  label A move, with the question left open
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import filesystemOptions.{createParents, dereferenceSymlinks, moveAtomically}

      def publish(draft: Path on Local, script: Path on Local): Path on Local =
        draft.moveTo(script)
frame
  label It may not
  highlight failOnPreexisting  a collision is an error
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import filesystemOptions.{createParents, dereferenceSymlinks, moveAtomically}
      import filesystemOptions.failOnPreexisting

      def publish(draft: Path on Local, script: Path on Local): Path on Local =
        draft.moveTo(script)
frame
  label It may
  highlight overwritePreexisting  what is there is deleted first
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import filesystemOptions.{createParents, dereferenceSymlinks, moveAtomically}
      import filesystemOptions.{overwritePreexisting, deleteRecursively}

      def publish(draft: Path on Local, script: Path on Local): Path on Local =
        draft.moveTo(script)
##
```

The example is compiled, and is not run, since it would move a file.
[Copying, moving and deleting](../../soundness/modules/filesystem.md#copying-moving-and-deleting)
lists the five questions and their answers, and
[declarative context](../../philosophy/declarative-context.md) explains why they are imports.
