# A default only where a type has one

`presume` replaces an absent value with its type's default: empty text, zero, an empty list.
It is defined for the types which declare a default, and a new type gains it with one line.

```scala fuel
compile
frame
  label A type with no default
  errors show
  code
      case class Volume(level: Int)

      val setting: Optional[Volume] = Unset
      val volume: Volume = setting.presume
frame
  label A default, declared
  highlight Default[Volume]  what presume falls back to
  code
      case class Volume(level: Int)
      given Default[Volume] = () => Volume(0)

      val setting: Optional[Volume] = Unset
      val volume: Volume = setting.presume
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(volume.level.show)
```

[Defaults](../../soundness/modules/optional-values.md#defaults) says when a default is
computed.
