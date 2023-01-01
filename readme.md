[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/honeycomb/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/honeycomb/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/honeycomb-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/honeycomb-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Honeycomb

_Honeycomb_ provides a typesafe representation of HTML5 which goes as far as possible in the scope of
Scala's type system to ensure that the HTML specification is correctly modeled, for example by
enforcing nesting rules between different tags.

## Features

- simple method-application style for constructing HTML nodes
- nodes and sequences of nodes may be embedded and mixed inside HTML nodes
- the type of a node defines the valid types of its child nodes, enforcing the HTML5 specification through the type system
- values of other types may be embedded in HTML, if an appropriate typeclass instance exists
- a simple named-parameter style is provided for HTML attributes
- HTML attributes are strongly-typed, and the types accepted by each are defined by typeclasses


## Availability

The current latest release of Honeycomb is __0.4.0__.

## Getting Started

## Constructing HTML nodes

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

## Attributes

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


## Related Projects

The following _Scala One_ libraries are dependencies of _Honeycomb_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp;

The following _Scala One_ libraries are dependents of _Honeycomb_:

[![Punctuation](https://github.com/propensive/punctuation/raw/main/doc/images/128x128.png)](https://github.com/propensive/punctuation/) &nbsp; [![Tarantula](https://github.com/propensive/tarantula/raw/main/doc/images/128x128.png)](https://github.com/propensive/tarantula/) &nbsp;

## Status

Honeycomb is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Honeycomb is designed to be _small_. Its entire source code currently consists of 713 lines of code.

## Building

Honeycomb can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Honeycomb are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/honeycomb/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Honeycomb easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Honeycomb was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Honeycomb is named after the hexagonal prismic cells in a bees' nest, where they store their honey and their larvæ; the most innovative feature of Honeycomb is its provision of safe nesting of HTML nodes.

## License

Honeycomb is copyright &copy; 2018-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
