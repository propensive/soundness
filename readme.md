[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/tarantula/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/tarantula/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Tarantula

__Drive a web browser using the WebDriver protocol__

_Tarantula_ makes it possible to interact with a web browser through a programmatic interface. It
provides an immutable API for controlling the web browser from Scala, through the WebDriver
protocol.

## Features

- simulate keypresses and mouse clicks in a web browser
- automatically launch Chrome or Firefox programmatically
- uses the standard WebDriver protocol
- intuitive, but typesafe syntax


## Availability

Tarantula has not yet been published as a binary.

## Getting Started

### Browser Sessions

_Tarantula_ makes it possible to control a web browser programmatically from Scala. Currently [Firefox](https://www.mozilla.org/en-GB/firefox/new/) and [Chrome](https://www.google.com/chrome/) are supported.

All browser operations take place in a _session_, which may be started by calling the `session` method, specifying a port number, on
a `Browser` object; either `Chrome` or `Firefox`. For example:
```scala
Firefox.session(8120):
  // Browser actions are carried out in this scope
```

### Simple navigation

Within the session body, the `browser` object, may be accessed and used to control the newly-launched browser.

The `browser` object is an instance of `WebDriver#Session`, and includes several navigational methods:
- `navigateTo(url)` - to send the browser to a particular URL
- `refresh()` - to refresh the page
- `back()` - to go back to the previous page
- `forward()` - to go forward (assuming we have already gone back at least once)

The `title()` method will also return the page title, and `url()` the current URL, as `Text` instances.

### Acessing Elements

Within a particular page, it's possible to access an element with the `element` method, which takes, as a parameter, a
way of locating that element, of which several different types are valid:
- `Text`: finds an element by its link text
- `Selector`: type from [Cataclysm](https://github.com/propensive/cataclysm), which find a value by CSS selection
- `TagType`, `DomId`, `Cls`: types from [Honeycomb](https://github.com/propensive/honeycomb), which finds a value by an HTML tag, DOM ID or CSS class

For example, the link containing the text `"here"` could be selected with, `browser.element(t"here")` or the element which is an
instance of an `<img>` HTML tag could be found with `browser.element(Img)`, where the `Img` value is defined in Honeycomb. In both
cases an `Element` instance will be returned, or an exception will be thrown if no matching element exists on the page.

HTML has a tree-based structure, so it's possible to select one element within another with repeated applications of the
`Element#element` method, for example,
```scala
val link = browser.element(Nav).element(id"menu").element(t"About")
```
would find the link containing the text `About` in the element with ID `menu` which is inside a `<nav>` HTML element.

### Accessing multiple elements

Often it's useful to find all elements on a page, which is served by the `/` method of `browser`, `Element` and as an
extension on `List[Element]`. Although the method is defined on three types, it always returns a `List[Element]`, and since
this is one of the types defining `/`, it is easy to progressively filter a selection of elements to a single one, with
repeated applications. The infix syntax is particularly intuitive. For example,
```scala
for elem <- browser / id"menu" / Li / cls"checkbox" do elem.click()
```
would simulate a click on every element with the `checkbox` CSS class inside an `<li>` tag in the element with ID `menu`.



## Status

Tarantula is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Tarantula is designed to be _small_. Its entire source code currently consists
of 132 lines of code.

## Building

Tarantula can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Tarantula are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/tarantula/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Tarantula easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Tarantula was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

__Tarantulas__ are spiders, known for making webs, and Tarantula is a library for the WebDriver protocol.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo represents the eight legs of a tarantula.

## License

Tarantula is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
