# A platform, matched as a value

The operating system and the processor are read from the `System` in scope as typed values,
not as the text of a property. A release is therefore chosen by a pattern, in which the width
of the processor is a type and an unknown system binds its name.

```scala fuel
compile
frame
  label An operating system
  code
      def release()(using System): Optional[Text] = System.os match
        case Os.Linux => t"linux"
        case Os.MacOs => t"macos"
        case _        => Unset
frame
  label And a processor, of a width
  highlight Architecture.Arm(_:..64)  the width, as a type
  code
      def release()(using System): Optional[Text] =
        (System.os, System.architecture) match
          case (Os.Linux, Architecture.X86(_: 64)) => t"linux-x64"
          case (Os.Linux, Architecture.Arm(_: 64)) => t"linux-arm64"
          case (Os.MacOs, Architecture.Arm(_: 64)) => t"macos-arm64"
          case _                                   => Unset
frame
  label A system with no case of its own
  highlight Os.Other(name)  binds what the system calls itself
  code
      def release()(using System): Optional[Text] =
        (System.os, System.architecture) match
          case (Os.Linux, Architecture.X86(_: 64)) => t"linux-x64"
          case (Os.Linux, Architecture.Arm(_: 64)) => t"linux-arm64"
          case (Os.MacOs, Architecture.Arm(_: 64)) => t"macos-arm64"
          case (Os.Other(name), _)                 => t"$name-unknown"
          case _                                   => Unset
##
```

The example is compiled, and is not run, since its result depends on the machine.
[The machine](../../soundness/modules/environment.md#the-machine) lists the cases.
