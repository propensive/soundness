# What a null means for an optional field

A YAML file may give a key the value `null`. Whether that is the same as leaving the key out
is a policy, and an import chooses it for every decoder in its scope.

```scala fuel
compile
alternative A null is an absent value
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Optional[Int])

      val fool: Optional[Player] = safely(t"name: Fool\nage: null".read[Yaml].as[Player])
alternative A null is a fault
  highlight strictYamlNulls  a null goes to the decoder of Int
  code
      import strategies.throwUnsafely
      import optionalityOptions.strictYamlNulls

      case class Player(name: Text, age: Optional[Int])

      val fool: Optional[Player] = safely(t"name: Fool\nage: null".read[Yaml].as[Player])
##
```

The first gives a player whose age is `Unset`, and the second gives `Unset` for the player,
since `null` is not an `Int`. [Optional fields](../../soundness/modules/yaml.md#optional-fields)
has the two other policies.
