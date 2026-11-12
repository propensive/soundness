## Forms

### About

An HTML form is a user interface to a data type, and here one is generated from the type
itself. A case class produces a form, with a labeled input for each field, and a widget
chosen by the field's type. A submission is decoded back into the case class, and any
failures of validation are attached to the fields which caused them. Nested case classes
become nested fieldsets, so the form mirrors the structure of the data.

### On forms

Forms are written twice in most web applications: once as HTML inputs, and again as the
parsing and validation of what comes back. The two halves are kept consistent by hand. Every
change to the data, such as a new field, a renamed one or a stricter type, must be made in
both, or the form drifts from the data it claims to collect.

[Deriving][derivation] both halves from one type removes the duplication. The type says what
fields exist, and what each accepts. The widgets follow from the types of the fields.
Decoding a submission applies the same validated types which govern the rest of the program,
so a malformed email address fails at the form, with a message which points at the email
field. That is [correctness](../philosophy/correctness.md) through a single source of truth.

Everything comes from the `soundness` package. The last import lets the samples print their
results:

```scala
import soundness.*
import formulations.postFormulation
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Rendering a form

`elicit` renders the form for a type as [HTML](html.md), ready to serve. `edit` renders it
filled in from an existing value:

```scala
case class Person(name: Text, email: EmailAddress)
case class Troupe(leader: Person, name: Text)

val form = elicit[Troupe](validation = Validation(), submit = t"Save")
```

```scala fuel
run
##
Out.println(form.show)
```

Each field appears with a label derived from its name, so `leader` becomes "Leader". A nested
case class becomes a `<fieldset>` with a legend, so the structure of the form explains
itself.

The widget for each field is chosen by its type:

```tabular
column  Field type
column  Widget
column  Rendered as
row  `Boolean`  `Checkbox`  a checkbox
row  an enumeration  `Combobox`  a text input with a `<datalist>` of the cases
row  `Reference[entity]`  `Dropdown`  a `<select>` of the listed entities
row  any other type encodable as `Text`  `Field`  a text input
row  a case class  none  a `<fieldset>` of its fields
```

### Submissions

A submission arrives as a `Query`: the pairs of keys and values of a form post, in which
nested fields are dotted, as in `leader.name`. `as` decodes it to the type:

```scala
val query = t"leader.name=Quince&leader.email=quince%40example.com&name=Mechanicals".as[Query]
```

```scala fuel
run
##
Out.println(query.as[Troupe].inspect)
```

The fields are decoded through the same types which are used everywhere. An
[`EmailAddress`](network-addresses.md#email-addresses) must parse, and a
[`Name`](names.md) must satisfy its rules. The validation of a form is therefore not a
separate set of rules. It is the type system doing its usual work at the boundary, and a
failure carries a pointer to the field at fault:

```scala fuel
run
errors show
##
t"leader.name=Quince&leader.email=nobody&name=Mechanicals".as[Query].as[Troupe]
```

[Decoding](decoding.md#requiring-a-value) explains how a field's type decides whether the
field is required.

### The form cycle

A form is a loop: render, submit, and render again with errors, until the value is complete.
`elicit` takes the submitted `Query` and the state of validation, so the form which is
rendered again shows what was entered, with the faults attached to their fields.

A [server](http-server.md#responses) handler runs the loop, by decoding on success and
rendering again on failure. It therefore deals in values, not in requests. The
[HTTP client](http-client.md#posting-a-body) posts a `Query` as a form submission.

### Customizing appearance

How a form and its rows are rendered is a `Formulation`: the frame around the widgets, and
the placement of labels and error messages. `postFormulation` gives a plain, unstyled
rendering. An application supplies its own, to match its design, without touching how fields
are derived. [CSS](css.md) styles the result.
