# A Windows path literal

The `p"…"` literal reads a Windows path as readily as a POSIX one, and tells them apart by
their shape. A drive letter and a backslash make a `Path on Windows`, and a drive letter with
no root makes a compile error.

```scala fuel
compile
frame
  label A drive, and no root
  errors show
  code
      val system = p"""C:Windows"""
frame
  label A drive and a root
  highlight C:\Windows\System32  read by the compiler
  code
      val system = p"""C:\Windows\System32"""
frame
  label Its platform is in its type
  highlight on..Windows  decided by the shape of the literal
  code
      val system: Path on Windows = p"""C:\Windows\System32"""
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(system.encode)
Out.println(system.name)
```

[Building a path](../../soundness/modules/paths.md#building-a-path) has the literal, and
[safety by construction](../../philosophy/safety-by-construction.md) the principle.
