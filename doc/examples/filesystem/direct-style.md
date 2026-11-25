# Four operations on a disk, one after another

Writing a file, copying it, moving it and reading it back can each fail. They are written as
four plain lines, and the one way they fail is in the method's type.

```scala fuel
compile
frame
  label Write
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import filesystemOptions.{createParents, dereferenceSymlinks, moveAtomically}
      import filesystemOptions.{overwritePreexisting, deleteRecursively}

      def revise(folio: Path on Local): Unit raises Io.Error =
        val draft = folio / "draft.txt"
        draft.write(t"Enter Viola")
frame
  label Copy
  highlight copyTo  may fail, and says so above
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import filesystemOptions.{createParents, dereferenceSymlinks, moveAtomically}
      import filesystemOptions.{overwritePreexisting, deleteRecursively}

      def revise(folio: Path on Local): Unit raises Io.Error =
        val draft = folio / "draft.txt"
        draft.write(t"Enter Viola")
        draft.copyTo(folio / "backup.txt")
frame
  label Move, and read
  highlight moveTo  gives the new path
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import filesystemOptions.{createParents, dereferenceSymlinks, moveAtomically}
      import filesystemOptions.{overwritePreexisting, deleteRecursively}

      def revise(folio: Path on Local): Text raises Io.Error =
        val draft = folio / "draft.txt"
        draft.write(t"Enter Viola")
        draft.copyTo(folio / "backup.txt")
        draft.moveTo(folio / "script.txt").read[Text]
##
```

The example is compiled, and is not run, since it would write to the disk.
[Copying, moving and deleting](../../soundness/modules/filesystem.md#copying-moving-and-deleting)
has the operations, and [direct style](../../philosophy/direct-style.md) says what became of
the effects.
