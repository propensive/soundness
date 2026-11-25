# A debugging form for any case class

`inspect` writes a value for a programmer. For a case class or an enumeration its form is
derived from the fields, each inspected in turn, so nothing is written for the type.

```scala fuel
compile
frame
  label A case class
  code
      case class Player(name: Text, age: Int)

      val player = Player(t"Bottom", 35)
frame
  label Nested in an enumeration
  highlight Role  no instance is written for it
  code
      case class Player(name: Text, age: Int)

      enum Role:
        case Lead(player: Player, understudy: Optional[Player])
        case Chorus

      val pyramus: Role = Role.Lead(Player(t"Bottom", 35), Unset)
      val wall: Role = Role.Chorus
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(pyramus.inspect)
Out.println(wall.inspect)
```

Text is written as it would be in source, an absent value is a circle, and a case with no
fields is its bare name. [Inspecting](../../soundness/modules/display.md#inspecting) describes
each form.
