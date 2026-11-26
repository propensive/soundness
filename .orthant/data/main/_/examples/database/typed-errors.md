# A value which was never stored

`store` cannot fail, since it adds what is missing. `reference` only looks, and a value which
the database does not hold has no reference, so it raises a `Database.Error`, which something
must handle.

```scala fuel
compile
frame
  label Nothing handles the error
  errors show
  code
      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      val alpha = Box(t"alpha").reference()
frame
  label A method declares it
  highlight raises..Error
  code
      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      def known(name: Text): Boolean raises Database.Error =
        Box(name).reference() == Box(name).store()
frame
  label Its caller turns it into an absent value
  highlight safely
  code
      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      def known(name: Text): Boolean raises Database.Error =
        Box(name).reference() == Box(name).store()

      val alpha: Optional[Boolean] = safely(known(t"alpha"))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(alpha.inspect)
```

[Relating](../../soundness/modules/database.md#relating) describes the lookup, and
[Errors](../../soundness/modules/errors.md#choosing-a-strategy) the ways to meet the error.
