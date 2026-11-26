# The permissions of every file created in a scope

A file is created under a mask, which withholds permissions from it. The mask is a given, so
one line makes every file which is created beneath it private, and each file is created with
its final permissions, never with wider ones which are narrowed afterwards.

```scala fuel
compile
frame
  label Under the mask of the process
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage

      def confide(letter: Path on Local): Unit =
        letter.create[File](): handle ?=>
          handle.write(Chain(t"I am not what I am".in[Data]))
frame
  label Under a mask of its own
  highlight Umask(0x3f)  077 in octal, so the owner alone
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage

      def confide(letter: Path on Local): Unit =
        given umask: Umask = Umask(0x3f)

        letter.create[File](): handle ?=>
          handle.write(Chain(t"I am not what I am".in[Data]))
##
```

The example is compiled, and is not run, since it would create a file.
[Creation masks](../../soundness/modules/filesystem.md#creation-masks) explains why a daemon
needs this, and [declarative context](../../philosophy/declarative-context.md) the principle.
