# Validation is the field's type

A form has no rules of its own for what a field accepts. A field of type `EmailAddress` is
decoded as every email address is, so a submission which holds anything else has no value of
the form's type, and the failure names the field.

```scala fuel
compile
frame
  label An address which is any text
  code
      import strategies.throwUnsafely

      case class Player(name: Text, email: Text)

      def player(post: Text): Player = post.as[Query].as[Player]
frame
  label An address which must be one
  highlight EmailAddress  the only validation there is
  code
      import strategies.throwUnsafely

      case class Player(name: Text, email: EmailAddress)

      def player(post: Text): Player = post.as[Query].as[Player]
##
```

```scala fuel
run
errors show
##
player(t"name=Quince&email=nobody")
```

[Submissions](../../soundness/modules/forms.md#submissions) describes how a failure points at
its field.
