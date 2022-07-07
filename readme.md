[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/tarantula/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/tarantula/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/tarantula-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/tarantula-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Tarantula

_Tarantula_ makes it possible to interact with a web browser through a programmatic interface. It
provides an immutable API for controlling the web browser from Scala, through the WebDriver
protocol.

## Features

- simulate keypresses and mouse clicks in a web browser
- automatically launch Chrome or Firefox programmatically
- uses the standard WebDriver protocol
- intuitive, but typesafe syntax


## Availability

The current latest release of Tarantula is __0.4.0__.

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
- `Selector`: type from [Cataract](https://github.com/propensive/cataract), which find a value by CSS selection
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


## Related Projects

The following _Scala One_ libraries are dependencies of _Tarantula_:

[![Cataract](https://github.com/propensive/cataract/raw/main/doc/images/128x128.png)](https://github.com/propensive/cataract/) &nbsp; [![Euphemism](https://github.com/propensive/euphemism/raw/main/doc/images/128x128.png)](https://github.com/propensive/euphemism/) &nbsp; [![Guillotine](https://github.com/propensive/guillotine/raw/main/doc/images/128x128.png)](https://github.com/propensive/guillotine/) &nbsp; [![Honeycomb](https://github.com/propensive/honeycomb/raw/main/doc/images/128x128.png)](https://github.com/propensive/honeycomb/) &nbsp; [![Scintillate](https://github.com/propensive/scintillate/raw/main/doc/images/128x128.png)](https://github.com/propensive/scintillate/) &nbsp;

No other _Scala One_ libraries are dependents of _Tarantula_.

## Status

Tarantula is classified as __embryonic__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Tarantula is designed to be _small_. Its entire source code currently consists of 125 lines of code.

## Building

Tarantula can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Tarantula are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/tarantula/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Tarantula easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Tarantula was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

__Tarantulas__ are spiders, known for making webs, and Tarantula is a library for the WebDriver protocol.

## License

Tarantula is copyright &copy; 2021-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
