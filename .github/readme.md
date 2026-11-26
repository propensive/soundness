![Latest version](https://img.shields.io/github/v/tag/propensive/soundness?label=LATEST+VERSION&style=for-the-badge&labelColor=444444&color=ea8433)

![Soundness](/.github/logo.svg#gh-dark-mode-only)
![Soundness](/.github/logo2.svg#gh-light-mode-only)

Soundness is an ecosystem of libraries for writing direct-style code with Scala
3, in a number of broad domains:

- `cli`, for building commandline applications
- `data`, for working with data in various forms
- `sci`, for scientific and mathematical operations
- `test`, for testing
- `tool`, for building developer tools
- `web`, for developing web applications

The [Soundness website](https://soundness.dev/) includes more details about
developing software using Soundness, and should be the primary source of
documentation about Soundness for _users_.

## Using Soundness

Soundness is composed of over one hundred modules. Each module has its own
unique name and purpose, and may be used alone (with its dependencies) or in
combination with other modules in the ecosystem. Most modules have a `core`
component, but many have additional components for optional functionality.
Modules are grouped into bundles, one for each of the domains above.

### Binary dependencies

Releases are published on GitHub, not on Maven Central: every component is a jar
of its own, attached to the release. [Crux](https://github.com/propensive/crux)
is the build plugin, for Mill and for sbt, which resolves them from there. It
resolves [Proscala](https://github.com/propensive/proscala) too, the compiler
Soundness is built with. A dependency is specified as follows:

 - To include _everything in Soundness_, use
   `dev.propensive:soundness:<version>`. This covers every bundle except the two
   opt-in ones, `staged` and `android`, whose dependencies (the staging compiler
   and R8) are heavyweight enough that they should only ever be asked for by
   name.

 - To include a bundle of modules, use
   `dev.propensive:soundness-<bundle>:<version>` where _`<bundle>`_ is one of:
   - `base`, the core abstractions every other bundle builds on
   - `cli`, `data`, `sci`, `test`, `tool` or `web`, for the domains above
   - `wasi`, the WASI backends for the platform-abstraction modules
   - `staged`, the expansion-time variants of the data-format modules
   - `android`, the Android dexing and packaging stages

   for example, `dev.propensive:soundness-sci:0.80.0`.

 - To include a single component, use
   `dev.propensive:<module>-<component>:<version>`, for example
   `dev.propensive:jacinta-core:0.80.0`. Whatever it depends on is included too.

 - The compiler plugins are used with `-Xplugin:` rather than on the classpath,
   and are in no bundle: `dev.propensive:larceny-plugin`,
   `dev.propensive:umbrageous-plugin` and `dev.propensive:beneficence-plugin`.

A bundle has no classes of its own: it depends on each of its components, so a
build may name a bundle and a component together. For Scala.js and Scala Native,
a bundle and a component have the platform's suffix, as
`soundness-web_sjs1_3` and `soundness_native0.5_3`.

Version numbers are synchronized across all bundles, and the latest release
version is shown at the top of this page. Binary compatibility is not guaranteed
between modules with different version numbers.


### Releases

New versions of Soundness are usually released weekly.


## Building Soundness

Soundness is currently built using [Mill](https://mill-build.org/), with
[GNU Make](https://www.gnu.org/software/make/) providing some convenient shortcuts.

- `make test` will compile everything and run the full test suite with [fume](https://github.com/propensive/fume)
- `make test.<module>` will compile and run the tests for the module _`<module>`_
- `make dev` will compile all source files continuously, watching for changes
- `make publishLocal` will publish a version of Soundness locally


### Requirements

Soundness requires Scala 3.7. Java 17 should be assumed as a minimum requirement
for most modules, however _Mandible_ (which uses the new classfile API) requires
Java 24 or later.
