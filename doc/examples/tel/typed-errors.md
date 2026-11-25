# A document which may lack a keyword

A TEL file is written by a person, and may not have what a case class needs. Decoding one
raises a `Tel.Error`, and a method which decodes must handle the error or name it in its type.

```scala fuel
compile
frame
  label A failure which is not accounted for
  errors show
  code
      case class Player(name: Text, age: Int)

      def player(tel: Tel): Player = tel.as[Player]
frame
  label The failure, declared
  highlight raises..Error  part of the type
  code
      case class Player(name: Text, age: Int)

      def player(tel: Tel): Player raises Tel.Error = tel.as[Player]
frame
  label The failure, as an absent value
  highlight safely  a failure becomes Unset
  code
      case class Player(name: Text, age: Int)

      def player(tel: Tel): Optional[Player] = safely(tel.as[Player])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(player(tel"name Ophelia").inspect)
```

[Parsing and decoding](../../soundness/modules/tel.md#parsing-and-decoding) shows the errors
themselves.
