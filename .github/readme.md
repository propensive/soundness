![Latest version](https://img.shields.io/github/v/tag/propensive/soundness?label=LATEST+VERSION&style=for-the-badge&labelColor=444444&color=ea8433)

![Soundness](/doc/logo.svg#gh-dark-mode-only)
![Soundness](/doc/logo2.svg#gh-light-mode-only)

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
Bundles of modules for the six domains above are also provided.

### Binary dependencies

Releases are published on Maven Central, and can be specified as follows:

 - To include a bundle of modules, use
   `dev.soundness:soundness-𝒷𝓊𝓃𝒹𝓁ℯ:𝓋ℯ𝓇𝓈𝒾ℴ𝓃` where _`𝒷𝓊𝓃𝒹𝓁ℯ`_ is `cli`,
   `data`, `sci`, `test`, `tool` or `web`, for example,
   `dev.soundness:soundness-sci:0.48.0`.
   
 - To include one specific module, use
   `dev.soundness:𝓂ℴ𝒹𝓊𝓁ℯ-𝒸ℴ𝓂𝓅ℴ𝓃ℯ𝓃𝓉:𝓋ℯ𝓇𝓈𝒾ℴ𝓃`, where _`𝓂ℴ𝒹𝓊𝓁ℯ`_ is one of
   the modules from the
   [lib](https://github.com/propensive/soundness/tree/main/lib) directory, and
   _`𝒸ℴ𝓂𝓅ℴ𝓃ℯ𝓃𝓉`_ is typically `core`, but may be something else for modules
   with optional components, for example `dev.soundness:rudiments-core:0.48.0`
   or `dev.soundness:punctuation-html:0.48.0`.
   
 - To include _everything in Soundness_, use
   `dev.soundness:soundness-all:𝓋ℯ𝓇𝓈𝒾ℴ𝓃`.

Version numbers are synchronized across all modules, and the latest release
version is shown at the top of this page. Binary compatibility is not guaranteed
between modules with different version numbers.


### Releases

New versions of Soundness are usually released weekly.


## Building Soundness

Soundness is currently built using [Mill](https://mill-build.org/), with
[GNU Make](https://www.gnu.org/software/make/) providing some convenient shortcuts.

- `make test` will compile everything and run the full test suite
- `make test.𝓂ℴ𝒹𝓊𝓁ℯ` will compile and run the tests for the module _`𝓂ℴ𝒹𝓊𝓁ℯ`_
- `make dev` will compile all source files continuously, watching for changes
- `make publishLocal` will publish a version of Soundness locally


### Requirements

Soundness requires Scala 3.7. Java 17 should be assumed as a minimum requirement
for most modules, however _Mandible_ (which uses the new classfile API) requires
Java 24 or later.
