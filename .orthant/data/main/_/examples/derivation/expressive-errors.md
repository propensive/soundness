# A derivation which explains its refusal

A derivation can ask, as the code compiles, whether every case of an enumeration is a
singleton. One which makes sense only for such enumerations then rejects the rest with a
message which its author wrote.

```scala fuel
compile
frame
  label An enumeration of names
  focus enum..Comedy)
  code
      import wisteria.contextual

      trait Naming[value]:
        def name(value: value): Text

      object Naming extends Derivation[Naming]:
        inline def conjunction[derivation <: Product: ProductReflection]: Naming[derivation] =
          value => typeName[derivation]

        inline def disjunction[derivation: SumReflection]: Naming[derivation] = value =>
          inline if choice[derivation] then
            variant(value):
              [variant <: derivation] => arm => t"${typeName[derivation]}.${contextual.name(arm)}"
          else scala.compiletime.error("cannot derive Naming for a sum whose variants carry data")

      enum Genre:
        case Comedy, History, Tragedy

      val name: Text = Naming.derived[Genre].name(Genre.Comedy)
frame
  label An enumeration whose cases hold data
  errors show
  code
      import wisteria.contextual

      trait Naming[value]:
        def name(value: value): Text

      object Naming extends Derivation[Naming]:
        inline def conjunction[derivation <: Product: ProductReflection]: Naming[derivation] =
          value => typeName[derivation]

        inline def disjunction[derivation: SumReflection]: Naming[derivation] = value =>
          inline if choice[derivation] then
            variant(value):
              [variant <: derivation] => arm => t"${typeName[derivation]}.${contextual.name(arm)}"
          else scala.compiletime.error("cannot derive Naming for a sum whose variants carry data")

      enum Genre:
        case Comedy, History
        case Tragedy(deaths: Int)

      val name: Text = Naming.derived[Genre].name(Genre.Comedy)
##
```

[Enumerations of singletons](../../soundness/modules/derivation.md#enumerations-of-singletons)
describes `choice` and its companions.
