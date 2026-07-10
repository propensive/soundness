## Forms

### About

An HTML form is a user interface to a data type, and Soundness generates one from the type
itself. A case class produces a form — a labelled input per field, the widget chosen by the
field's type — and a submission decodes back into the case class, with validation failures
attached to the fields that caused them. Nested case classes become nested fieldsets, so the form
mirrors the data's structure.

### On forms

Forms are written twice in most web applications: once as HTML inputs, and again as the parsing
and validation of what comes back. The two halves are kept consistent by hand, and every change to
the data — a new field, a renamed one, a stricter type — must be made in both, or the form drifts
from the data it claims to collect.

Deriving both halves from one type removes the duplication. The type says what fields exist and
what each accepts; the widgets follow from the field types — a `Boolean` is a checkbox, an
enumeration a selection, text a field; and decoding a submission applies the same validated types
that govern the rest of the program, so a malformed email address fails at the form boundary with
a message pointing at the email field. Everything comes from the `soundness` package:

```scala
import soundness.*
import formulations.defaultFormulation
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
query.as[Organization]   // an Organization, or typed errors per field
```

Because the fields decode through the same types used everywhere — an `EmailAddress` must parse, a
`Name` must satisfy its rules — form validation is not a separate rule set but the type system
doing its usual work at the boundary.

### The form cycle

A form is a loop: render, submit, re-render with errors, until the value is complete. The
[HTTP server](http-server.md) integration runs that loop with `orchestrate`, delivering a complete
typed value when validation passes and the re-rendered form, faults attached, when it does not —
so a handler deals in values, not requests.

### Customising appearance

How a form and its rows render is a `Formulation` — the frame around the widgets, the placement of
labels and error messages. `defaultFormulation` gives a plain, unstyled rendering; an application
supplies its own to match its design, without touching how fields are derived.
