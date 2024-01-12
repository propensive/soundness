__Anticipation__ defines several typeclass interfaces to provide interoperability
between different libraries, without requiring a hard dependency between them.

### The Problem

Consider a library _J_ which defines a JSON datatype, and a second library _H_
which can return a different datatypes as HTTP responses. Neither library
fundamentally requires the other, and users should be able to choose whether to
depend on J or H or both.

But it would nevertheless be useful for users of both J and H to be able to
return JSON HTTP responses. The JSON HTTP typeclass instance must depend on
both J and H, which means that either,
1. the typeclass instance is defined in J, and J depends on H
2. the typeclass instance is defined in H, and H depends on J
3. a new module which depends on both J and H should be created

Options 1 and 2 require a dependency—in some direction—between J and H. And
option 3 has the disadvantage that it creates another dependency for users to
manage, both at the source-code and the build level.

The typeclass instance will have a type which looks something akin to
`Http[Json]`, where `Json` is defined in J and `Http` is defined in H. That
furthermore precludes the possibility that the given instance could be defined
in the companion objects of either of those types.

Ideally, users should be able to add both J and H (independently) to their
build, and import no more than the packages of J and H, and automatically get
access to the integration typeclass instance without any further work.

This is what Anticipation provides.

### The Solution

The issues above can be circumvented by predefining a set of minimalistic
typeclass interfaces for each integration point necessary, and having each
library independently depend upon them.

As much as possible, the typeclass interfaces should not require any additional
datatypes to be defined; they should depend only on types in the standard
library. Whilst this may compromise the utility of these typeclasses, they are
intended to be used only by the integration libraries; not by end-users.

Continuing the earlier example, the libraries J and H could both depend on
Anticipation. J would then define a typeclass instance for its JSON type in its
companion object, and H would define a typeclass converter from Anticipation's
typeclass to its own user-facing typeclass interface in its companion object.

Consequently, users of both H and J could depend on both libraries, import both
packages (and nothing more) and automatically be able to use the integration
between them.

### Anticipation Typeclasses

#### `GenericCssSelection` in the `css` module

Used primarily by [Cataclysm](https://github.com/propensive/cataclysm/), this
allows different types to be used in CSS selectors. Implementations are
provided in [Honeycomb](https://github.com/propensive/honeycomb/) for its
representations of CSS class names (`honeycomb.Cls`), DOM IDs
(`honeycomb.DomId`), and HTML tags (`honeycomb.TagType`).

#### Paths, Files and Directories in the `file` module

Six typeclass interfaces provide the ability to read and write representations
of paths, files and directories. These are used by projects such as
[Surveillance](https://github.com/propensive/surveillance/),
[Oubliette](https://github.com/propensive/oubliette/),
[Rudiments](https://github.com/propensive/rudiments/),
[Imperial](https://github.com/propensive/imperial/) and
[Guillotine](https://github.com/propensive/guillotine/) to support interaction
with any library which provides path-like types.
[Galilei](https://github.com/propensive/galilei/) provides instances for its
`galilei.Path`, `galilei.File` and `galilei.Directory` types, and
[Diuretic](https://github.com/propensive/diuretic/) provides instances for
types in the Java standard library, such as `java.io.File` and
`java.nio.file.Path`.

The typeclasses are called,
- `GenericPathReader`
- `GenericFileReader`
- `GenericDirectoryReader`
- `GenericPathMaker`
- `GenericFileMaker`
- `GenericDirectoryMaker`

In a later version of Anticipation, each `Reader` and `Maker` pair may be
combined into a single typeclass.

#### `GenericHtmlAttribute` in the `html` module

An HTML library, most notably
[Honeycomb](https://github.com/propensive/honeycomb/), can support attributes
having typed values (rather than just strings), provided that type is suitable
for the given attribute. The `GenericHtmlAttribute` typeclass, parameterised on
the singleton literal type of the attribute name, and the value type, allows
libraries to make their types usable in such HTML libraries.
[Gesticulate](https://github.com/propensive/gesticulate/) makes
`gesticulate.MediaType`s usable for `formenctype`, `enctype`, `media` and
`type` attributes. [Scintillate](https://github.com/propensive/scintillate/)
allows `scintillate.RequestParam` instances to be used for the `name` parameter
of inputs in a form. [Cataclysm](https://github.com/propensive/cataclysm/)
supports `cataclysm.CssStyle` values to be used on an HTML tag's `style`
attribute. And [Serpentine](https://github.com/propensive/serpentine/) and
[Telekinesis](https://github.com/propensive/telekinesis/) allow
`serpentine.Relative`, `serpentine.GenericPath`, `telekinesis.HttpMethod` and
`telekinesis.Url` instances to be used in the variety of attributes which
support these types, such as `src`.

#### `GenericHttpResponseStream` and `GenericHttpReader` in the `http` module

These typeclasses provide support for working with various types over HTTP. Any
type which can be streamed to HTTP, including an appropriate media type, can be
furnished with a `GenericHttpResponseStream` instance. Libraries which provide
these types include:

- [Caesura](https://github.com/propensive/caesura/), for `caesura.Csv` and `caesura.Tsv` types
- [Cataclysm](https://github.com/propensive/cataclysm/), for `cataclysm.CssStylesheet`s
- [Honeycomb](https://github.com/propensive/honeycomb/), for `honeycomb.HtmlDoc`s
- [Jacinta](https://github.com/propensive/jacinta/), for `Json` values
- [Xylophone](https://github.com/propensive/xylophone/), for `Xml` values

A `GenericHttpReader` instance is provided in
[Jacinta](https://github.com/propensive/jacinta/) for reading a `Json` value
directly from an HTTP response.

#### `GenericInstant` and `GenericDuration` in the `time` module

Any library which needs to work with time values, as instantaneous points in
time or as time durations, should use `GenericInstant` or `GenericDuration`
respectively.  [Parasite](https://github.com/propensive/parasite/) and
[Turbulence](https://github.com/propensive/turbulence/) use these typeclasses
for generic time-related operations, and instances are provided for Java types
in [Diuretic](https://github.com/propensive/diuretic/) and
[Aviation](https://github.com/propensive/aviation/) for `aviation.Instant` and
`aviation.Duration` types.

#### `GenericUrl` in the `url` module

Many libraries need to work with URLs, and the `GenericUrl` provides a generic
interface that's use by [Tarantula](https://github.com/propensive/tarantula/)
and [Telekinesis](https://github.com/propensive/telekinesis/).
[Diuretic](https://github.com/propensive/diuretic/) provides instances for
`java.net.URL` and Telekinesis provides an instance for its own
`telekinesis.Url` type.




