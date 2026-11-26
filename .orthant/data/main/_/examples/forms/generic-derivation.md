# A form derived from a case class

A form is derived from the type it collects. Each field becomes a labeled widget chosen by its
type, a nested case class becomes a fieldset, and a field added to the class is a field added
to the form.

```scala fuel
compile
frame
  label A form with one field
  code
      import formulations.postFormulation

      case class Troupe(name: Text)

      val form = elicit[Troupe](validation = Validation(), submit = t"Save")
frame
  label A nested case class, which becomes a fieldset
  highlight leader:..Person  the form follows the type
  code
      import formulations.postFormulation

      case class Person(name: Text, email: EmailAddress)
      case class Troupe(leader: Person, name: Text)

      val form = elicit[Troupe](validation = Validation(), submit = t"Save")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(form.show)
```

[Rendering a form](../../soundness/modules/forms.md#rendering-a-form) lists the widget for each
type of field.
