# Whether a delete may empty a directory

Deleting a directory which has children either removes the whole tree or refuses to. Neither
is a safe default, so `delete` has none: it does not compile until an import says which.

```scala fuel
compile
frame
  label A delete, with the question left open
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely

      def strike(set: Path on Local): Path on Local = set.delete()
frame
  label Only what is empty
  highlight deleteOnlyEmpty  a directory with children is an error
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import filesystemOptions.deleteOnlyEmpty

      def strike(set: Path on Local): Path on Local = set.delete()
frame
  label The whole tree
  highlight deleteRecursively  stated, and not inherited
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import filesystemOptions.deleteRecursively

      def strike(set: Path on Local): Path on Local = set.delete()
##
```

The example is compiled, and is not run, since it would delete a directory.
[Copying, moving and deleting](../../soundness/modules/filesystem.md#copying-moving-and-deleting)
has each policy, and [declarative context](../../philosophy/declarative-context.md) the
reasons for an import.
