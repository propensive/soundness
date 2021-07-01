[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/cataract/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/cataract/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/maven-central/v/com.propensive/cataract-core_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/cataract-core_2.12)
[<img src="https://vent.dev/badge/propensive/cataract" height="24">](https://vent.dev/)

<img src="/doc/images/github.png" valign="middle">

# Cataract

__Cataract__ provides a typesafe representation of CSS, including properties, selectors, rules and stylesheets.

## Features

TBC

## Getting Started

Cataract provides several types for modeling CSS.

## Selectors

CSS selectors are currently unparsed, and represented by the `Selector` type, which is just a
wrapper for a `String`. The easiest way to construct a new `Selector` is with the `sel`
interpolator, for example:
```scala
val selector: Selector = sel".form input[type=radio]"
```

In a later version, these will be parsed at compiletime to check they are well-formed.

## Specifying attributes

CSS attributes may be added to a `Selector` to create a `Rule` by applying named parameters to the
`Selector`. Each named parameter takes its name from the exact CSS attribute, where CSS's dashed
notation (for example, `animation-fill-mode`) should be translated into camelcase (for example,
`animationFillMode`).

Every CSS attribute may be transformed in this way, without exception. Only valid CSS attributes
(after transformation) may be used as named parameters, and any attempt to use a nonexistent CSS
attribute will produce a compile error.

For example,
```scala
val rule: Rule = sel".form input[type=radio]"(fontWeight = 600, textAlign = TextAlign.Center)
```

Additionally, a set of styles (not associated with a selector) may be constructed by applying them
to the `Css` object, for example,
```scala
Css(margin = (1.px, 2px, 1px), cursor = Cursor.Pointer)
```

## Stylesheets

A number of `Rule`s may be combined into a single `Stylesheet`. The `Stylesheet` constructor can
take repeated `Rule` arguments, for example:
```scala
val styles = Stylesheet(
  sel"form"(backgroundColor = colors.White),
  sel".form input"(display = Display.InlineBlock),
  rule1
)
```

## Other directives

In addition to `Rule`s, a `Stylesheet` may contain a number of other directives, such as imports and
media queries.

These include:
- `Keyframes`
- `Import`

Others will be added in due course.

## Attribute values

As much as possible, attribute values are strongly typed, which means arbitrary Scala expressions
may be used for attribute values, provided they conform to a suitable type.

The process of finding the most suitable types for each attribute is an ongoing process, and
different CSS attributes are gradually being migrated from using `String`s to more precise types.

Additionally, arithmetic expressions involving dimensional values will use CSS's `calc` function
where necessary.

### Dimensional values

Lengths and other values use instances of the `Length` enumeration. This provides representations of
all CSS unit types (`px`, `pt`, `in`, `pc`, `cm`, `mm`, `em`, `ex`, `ch`, `rem`, `vw`, `vh`, `vmin`
and `vmax`). These are made available as extension methods on `Double` values.

### Colors

Colours are provided by [Iridescence](https://github.com/propensive/iridescence/) which can
represent colors in a variety of different models. Color models which are natively supported by CSS
will embed those colors using that same model, while other colors will be converted to SRGB.

### Enumerated values

Many CSS attributes take one of a selection of enumerated possibilities. These are generally
represented by an `enum` whose name is taken from the capitalized, camelcase name of that attribute,
and whose members' names are similarly transformed.

For example, the CSS value, `color-dodge` for the attribute, `mix-blend-mode` is called,
`MixBlendMode.ColorDodge`.

### Multi-part values

Some CSS attributes, such as `border`, can accept multiple arguments. While these would be
separated by spaces in CSS, they should be embedded in a tuple using Cataract.

## Status

Cataract is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Cataract&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/cataract`.
```
fury layer clone -i propensive/cataract
```
or imported into an existing layer with,
```
fury layer import -i propensive/cataract
```

## Contributing

Contributors to Cataract are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/cataract/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Cataract easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Cataract was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Cataract is copyright &copy; 2017-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
