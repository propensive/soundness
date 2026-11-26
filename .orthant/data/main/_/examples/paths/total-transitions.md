# The parent of a path which may be the root

Every path has a parent but the root, and a path which arrives at runtime may be the root.
`parent` is therefore an `Optional` path, and the code which calls it says what the absent
case means.

```scala fuel
compile
frame
  label A parent, taken for granted
  errors show
  code
      def above(path: Path on Linux): Path on Linux = path.parent
frame
  label A parent which may be absent
  highlight Optional  the root has no parent
  code
      def above(path: Path on Linux): Optional[Path on Linux] = path.parent
frame
  label The root, where there is none
  highlight or(path)  what absence means here
  code
      def above(path: Path on Linux): Path on Linux = path.parent.or(path)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import strategies.throwUnsafely

Out.println(above(t"/home/viola".as[Path on Linux]).encode)
Out.println(above(t"/".as[Path on Linux]).encode)
```

The last frame is what a shell does: the parent of the root is the root.
[Between two paths](../../soundness/modules/paths.md#between-two-paths) has more arithmetic
on paths, and [total transitions](../../philosophy/total-transitions.md) explains the
principle.
