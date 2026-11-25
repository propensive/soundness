# A keyword the type does not have

`verify` checks a TEL document against the shape of a case class, and gives it that shape as
its type. Each step into it is then checked by the compiler, and a keyword which the case
class lacks cannot be asked for.

```scala fuel
compile
frame
  label A path through a verified document
  highlight verify[Residence]  checked once, as it runs
  code
      import strategies.throwUnsafely

      case class Castle(name: Text, city: Text)
      case class Residence(prince: Text, castle: Castle)

      def city(tel: Tel): Text = tel.verify[Residence].castle.city.as[Text]
frame
  label A step which the type does not allow
  errors show
  code
      import strategies.throwUnsafely

      case class Castle(name: Text, city: Text)
      case class Residence(prince: Text, castle: Castle)

      def town(tel: Tel): Text = tel.verify[Residence].castle.town.as[Text]
##
```

[Verified navigation](../../soundness/modules/tel.md#verified-navigation) contrasts this with
dynamic access, which checks nothing.
