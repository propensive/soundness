# The one field which carries an annotation

When exactly one field of a type carries an annotation, the summoned instance knows its name.
Whether the match was unique is recorded in the type of the instance, so `field` does not
exist where two fields are marked.

```scala fuel
compile
frame
  label One field is the identifier
  highlight field  exists because the match is unique
    line 5
  code
      final case class ident() extends StaticAnnotation

      case class Person(name: Text, @ident email: Text)

      val identifier: Text = summon[Person is Annotated by ident].field
frame
  label Two fields claim to be
  errors show
  code
      final case class ident() extends StaticAnnotation

      case class Person(@ident name: Text, @ident email: Text)

      val identifier: Text = summon[Person is Annotated by ident].field
##
```

[Finding the annotated field](../../soundness/modules/annotations.md#finding-the-annotated-field)
explains what the instance offers.
