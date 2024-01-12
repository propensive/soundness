### Constructing HTML nodes

Simple HTML nodes can be constructed by applying other nodes (or strings) to `Tag` instances, for example:
```scala
val table = Table(
  Tbody(
    Tr(Th("Name"), Th("Age")),
    Tr(Td("Andrew"), Td("18"))
  )
)
```

Note that the two `Tr` nodes are separated by a comma (`,`) since they are applied as repeated arguments.

This will produce HTML nodes representing the HTML code:
```html
<table>
  <tbody>
    <tr>
      <th>Name</th>
      <th>Age</th>
    </tr>
    <tr>
      <th>Andrew</th>
      <th>18</th>
    </tr>
  </tbody>
</table>
```

The example shows,
- a `Tbody` node nested inside a `Table`
- `Tr` nodes nested inside a `Tbody`
- `Th` and `Td` nodes nested inside `Tr`s

These are all permitted in HTML 5, so they are permitted in Honeycomb. But while it would be
possible to write the HTML,
```html
<tr>
  <tbody>
    <td>Name</td>
  </tbody>
</tr>
```
and a web browser may even render it in some way, the code `Tr(Tbody(Td("Name")))` is not valid,
and will not compile, because a `Td` cannot be a direct child of a `Tbody` node, and a `Tbody` node
may not be nested inide a `Tr` node.

`Tag` objects for every HTML node exist, and rules about which nodes are permitted as children of
each one are encoded in their types. This makes it possible to write HTML code which conforms to the
HTML 5 specification.

`Tag`s have the same name as their HTML counterparts, written with a capital letter.

There are some limitations to this in cases where HTML's rules are defined in terms of more than a
simple nesting relationship, but work is ongoing to encode as many constraints as possible. In all
cases where HTML 5 rules are not fully implemented, Honeycomb is more permissive.

### Attributes

HTML nodes may also have attributes. These can be applied to an additional parameter block _before_
the child nodes, in the style of named parameters, for example,
```scala
Td(colspan = 2, id = "cell_1")("Data")
```

Only attributes that are valid for a particular `Tag` type may be used on that tag. Also note that the
attribute values have different types: `colspan` takes an `Int` and `id` takes a `String`. By default,
attributes are configured to accept the most suitable types for describing their values, without
being overly permissive.

Many such types are not defined in Honeycomb, since their representation is best handled by other
libraries. Other libraries may nevertheless make their types usable by Honeycomb without adding a
hard dependency on Honeycomb. This facility is provided through typeclasses defined in
[Anticipation](https://github.com/propensive/anticipation), which becomes a necessary dependency
of both libraries, but is tiny, so does not impose any significant burden.

For example, if including [Gesticulate](https://github.com/propensive/gesticulate/) to represent
Media types, it becomes possible use a Gesticulate media type for an attribute such as `type` on
a `Style` tag, like so,
```scala
import gesticulate.*
val styles = Style(htype = media"text/css")(css)
```
without any additional imports.

A contextual instance of `anticipation.HtmlAttribute` is all that is required to make this possible.



