## Forms

### About

An HTML form is a user interface to a data type, and Soundness generates one from the type
itself. A case class produces a form — a labeled input per field, the widget chosen by the
field's type — and a submission decodes back into the case class, with validation failures
attached to the fields that caused them. Nested case classes become nested fieldsets, so the form
mirrors the data's structure.

### On forms

Forms are written twice in most web applications: once as HTML inputs, and again as the parsing
and validation of what comes back. The two halves are kept consistent by hand, and every change to
the data — a new field, a renamed one, a stricter type — must be made in both, or the form drifts
from the data it claims to collect.

Deriving a form and its decoder from one type keeps them in agreement, which is [correctness](../philosophy/correctness.md) through a single source of truth.

Deriving both halves from one type removes the duplication. The type says what fields exist and
what each accepts; the widgets follow from the field types — a `Boolean` is a checkbox, an
enumeration a selection, text a field; and decoding a submission applies the same validated types
that govern the rest of the program, so a malformed email address fails at the form boundary with
a message pointing at the email field. Everything comes from the `soundness` package:

```scala
import soundness.*
import formulations.postFormulation
import strategies.throwUnsafely
```

### Rendering a form

`elicit` renders the form for a type as [HTML](html.md), ready to serve; `edit` renders it
pre-filled from an existing value:

```scala
case class Person(name: Text, email: EmailAddress)
case class Organization(leader: Person, name: Text)

elicit[Organization](validation = Validation(), submit = t"Save")
```

Each field appears with a label derived from its name — `leader` becomes "Leader" — and a nested
case class becomes a `<fieldset>` with a legend, so the form's structure explains itself.

### Submissions

A submission arrives as a `Query` — the key–value pairs of a form post, with nested fields dotted
as `leader.name`. It decodes to the type with `as`, and a failure carries a pointer to the field at
fault:

```scala
val query = t"leader.name=Ada&leader.email=ada%40example.com&name=Acme".as[Query]

query.as[Organization]   // Organization(Person(t"Ada", email"ada@example.com"), t"Acme")
```

Because the fields decode through the same types used everywhere — an `EmailAddress` must parse, a
`Name` must satisfy its rules — form validation is not a separate rule set but the type system
doing its usual work at the boundary.

### The form cycle

A form is a loop: render, submit, re-render with errors, until the value is complete. `elicit`
takes the submitted `Query` and the validation state, so the re-rendered form shows what was
entered with the faults attached to their fields; a [server](http-server.md) handler runs the
loop by decoding on success and re-rendering on failure, and so deals in values, not requests.

### Customizing appearance

How a form and its rows render is a `Formulation` — the frame around the widgets, the placement of
labels and error messages. `postFormulation` gives a plain, unstyled rendering; an application
supplies its own to match its design, without touching how fields are derived.
