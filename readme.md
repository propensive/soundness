[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
<img src="/doc/images/github.png" valign="middle">

# Scala One

**Take Scala to Another Level!**

_Scala One_ is a loose collection of open-source libraries designed to take full advantage of new
features of Scala 3 to write web and command-line applications, with a focus on lifting more
operations to the type-level with safe and expressive syntax.

_Scala One_ includes libraries for working with [HTML](https://github.com/propensive/honeycomb/),
[CSS](https://github.com/propensive/cataclysm/), [JSON](https://github.com/propensive/jacinta/),
[XML](https://github.com/propensive/xylophone/), [CSV](https://github.com/propensive/caesura/),
[typesafe strings](https://github.com/propensive/gossamer/),
[ANSI-escaped strings](https://github.com/propensive/escapade/) and
[Markdown](https://github.com/propensive/punctuation/), both
[calling HTTP](https://github.com/propensive/telekinesis/) and
[serving HTTP](https://github.com/propensive/scintillate/) or the
[command line](https://github.com/propensive/exoskeleton/). Additionally, support for
[disk I/O](https://github.com/propensive/galilei/),
[filewatching](https://github.com/propensive/surveillance/) and the standard
[UNIX directory layout](https://github.com/propensive/imperial),
[environment variables](https://github.com/propensive/ambience),
[shell processes](https://github.com/propensive/guillotine/) (including native
[keyboard interactivity](https://github.com/propensive/profanity/)),
[cryptographic functions](https://github.com/propensive/gastronomy/),
[tabular output](https://github.com/propensive/escritoire/) and
[regular expressions](https://github.com/propensive/kaleidoscope/) is provided, and includes
representations of [colors](https://github.com/propensive/iridescence/),
[directed acyclic graphs](https://github.com/propensive/acyclicity/),
[multilingual strings](https://github.com/propensive/cosmopolite/),
[MIME types](https://github.com/propensive/gesticulate/) and
[generalized paths](https://github.com/propensive/serpentine/). Fundamental to these projects are
utilities for [generic derivation](https://github.com/propensive/wisteria/),
[checked interpolated strings](https://github.com/propensive/contextual/),
[annotations](https://github.com/propensive/adversaria/),
[streaming operations](https://github.com/propensive/turbulence),
[type providers](https://github.com/propensive/polyvinyl/)
library [decoupling](https://github.com/propensive/anticipation/) and
[unit testing](https://github.com/propensive/probably/) with a
[WebDriver API](https://github.com/propensive/tarantula/) for browser testing. A module also
exists for [Scala syntax highlighting](https://github.com/propensive/harlequin/). Everything
builds upon minimal set of [common tools](https://github.com/propensive/rudiments/).

## Principles

_Scala One_ embraces two core principles:
1. Impossible states should be unrepresentable
2. Transitions between states should be total

Together, these two principles eliminate an entire class of bugs.

More specifically, each library under the _Scala One_ umbrella adheres to the following principles:
- typesafe—taking full advantage of the Scala 3 typesystem
- checked exceptions—but only if you choose to use them
- safe literals—interpolated strings, checked at compiletime with
  [Contextual](https://github.com/propensive/contextual/)
- forbidden `null`—no method should ever return `null`, and no value should ever be `null`, guaranteed
  by the Scala typesystem
- typeclasses—extensibility provided through
  [Wisteria](https://github.com/propensive/wisteria/)'s generic derivation
- immutability—mutation of state is avoided, even when working with streams
- decoupled—modules use [Anticipation](https://github.com/propensive/anticipation/) to minimise
  unnecessary dependencies
- functional programming—embracing the fundamentals of FP, but avoiding the complexity
- small APIs—above all, code should be legible, natural and elegant

### Typesafe

Scala 3's typesystem offers a rich variety of types for representing and combining constraints on
values. This presents an opportunity to encode a value's invariants—facts about the value which we
know will always be true—precisely in its type. These invariants then give us certainty that
operations involving the value are safe. Or, that they're simply impossible. In general, this
reduces the amount of branching, including exception handling, that's required in code. Extensive
use of immutable datatypes adds further guarantees.

In particular, the type `Text`, an opaque type alias of `String`, is used in place of `String` at
every opportunity. `Text` has a variety of useful methods, all of which are total functions or
declare their exceptions. A `String` may always be converted to a `Text` with the `show` extension
method, and a `Text` converted to a `String` by calling its method `s`.

### Checked Exceptions

The latest release of Scala 3 introduces opt-in exception checking, and every _Scala One_ method declares
the exceptions it may throw in its signature. This makes it easy to write prototype code with
a "let it fail" attitude and exception-checking off, and to migrate to production-quality code just
by turning exception-checking on, and having the compiler require handlers for each exception—but
without needing to transform types or switch to a monadic coding style.

Effectively, this transforms every partial function into a total function; when combined with the
wise philosophy of making impossible states unrepresentable, exceptions become even more
exceptional.

### Forbidden `null`

The introduction of `null` into ALGOL was described by Tony Hoare as his "billion-dollar mistake",
though it has persisted in numerous programming languages since. Scala 3 introduces new checks to
help avoid `null` references, and _Scala One_ projects take full advantage of these.

### Safe Literals

When constructing a value, such as a JSON object or a URL, from a string literal, all the
information is available at compiletime to check the validity of the string's contents. So,
whenever possible, this is provided with interpolated strings, such as `url"https://github.com/"`,
using [Contextual](https://github.com/propensive/contextual/). Since checks are performed at
compiletime, there is no risk of runtime exceptions arising from these values.

### Typeclasses

The typeclass pattern, provided through contextual values (`given`s) is used extensively by
_Scala One_ libraries to provide ad-hoc polymorphism (in preference to subtype polymorphism). This not
only allows user-defined types to participate naturally in all kinds of _Scala One_ APIs, but also
facilitates interaction between _Scala One_ libraries and third-party libraries.

### Decoupled

When working with libraries in different domains, it is common to need integration between them.
For example, an HTTP server should be able to serve a XML value with the correct MIME type, without
too much boilerplate. That is easily achieved by making one library a dependency of the other. But
the user of a XML library should not need to include an HTTP server (nor should an HTTP server
require a XML library). The solution is to make use of minimal typeclasses provided by
[Anticipation](https://github.com/propensive/anticipation/) to maximally decouple independent
libraries.

### Small APIs

Every API introduced by a _Scala One_ library should fit on one side of a business card. It should never
be difficult to learn, and composition of APIs should be preferred over specialized solutions. Names
should be meaningful and appropriately unique: that is to say, sharing a name with an existing
concept or entity if they represent that entity, but introducing new nomenclature if they represent
something new. Short names are preferred, but arbitrarily-abbreviated names are not. Objects are
primarily composed through selection (the `.` operator) and application, rather than monadic mapping
and flat-mapping, since exceptional cases may be handled with checking. The amount of code in each
library should also be small.

### Other common features

- extensive use of the immutable `IArray[Byte]` type for random-access byte-data
- streaming provided using `LazyList`s
- use of the `T | Unset` union type for optional parameters, without the need to wrap `T` in `Some` or use `null`

## Future features

### Capture Checking

_Scala One_ projects already use Scala 3's enhanced exception checking, and in the future, streaming APIs
built on `LazyList`s will be enhanced to use the experimental capture-checking functionality that is
expected to be introduced in Scala 3 soon to provide better safety.

## _Scala One_ Modules

[![Acyclicity](https://github.com/propensive/acyclicity/raw/main/doc/images/128x128.png)](https://github.com/propensive/acyclicity/)&nbsp;
[![Adversaria](https://github.com/propensive/adversaria/raw/main/doc/images/128x128.png)](https://github.com/propensive/adversaria/)&nbsp;
[![Ambience](https://github.com/propensive/ambience/raw/main/doc/images/128x128.png)](https://github.com/propensive/ambience/)&nbsp;
[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/)&nbsp;
[![Aviation](https://github.com/propensive/aviation/raw/main/doc/images/128x128.png)](https://github.com/propensive/aviation/)&nbsp;
[![Baroque](https://github.com/propensive/baroque/raw/main/doc/images/128x128.png)](https://github.com/propensive/baroque/)&nbsp;
[![Caesura](https://github.com/propensive/caesura/raw/main/doc/images/128x128.png)](https://github.com/propensive/caesura/)&nbsp;
[![Cardinality](https://github.com/propensive/cardinality/raw/main/doc/images/128x128.png)](https://github.com/propensive/cardinality/)&nbsp;
[![Cataclysm](https://github.com/propensive/cataclysm/raw/main/doc/images/128x128.png)](https://github.com/propensive/cataclysm/)&nbsp;
[![Cellulose](https://github.com/propensive/cellulose/raw/main/doc/images/128x128.png)](https://github.com/propensive/cellulose/)&nbsp;
[![Charisma](https://github.com/propensive/charisma/raw/main/doc/images/128x128.png)](https://github.com/propensive/charisma/)&nbsp;
[![Chiaroscuro](https://github.com/propensive/chiaroscuro/raw/main/doc/images/128x128.png)](https://github.com/propensive/chiaroscuro/)&nbsp;
[![Contextual](https://github.com/propensive/contextual/raw/main/doc/images/128x128.png)](https://github.com/propensive/contextual/)&nbsp;
[![Cosmopolite](https://github.com/propensive/cosmopolite/raw/main/doc/images/128x128.png)](https://github.com/propensive/cosmopolite/)&nbsp;
[![Dendrology](https://github.com/propensive/dendrology/raw/main/doc/images/128x128.png)](https://github.com/propensive/dendrology/)&nbsp;
[![Digression](https://github.com/propensive/digression/raw/main/doc/images/128x128.png)](https://github.com/propensive/digression/)&nbsp;
[![Dissonance](https://github.com/propensive/dissonance/raw/main/doc/images/128x128.png)](https://github.com/propensive/dissonance/)&nbsp;
[![Diuretic](https://github.com/propensive/diuretic/raw/main/doc/images/128x128.png)](https://github.com/propensive/diuretic/)&nbsp;
[![Escapade](https://github.com/propensive/escapade/raw/main/doc/images/128x128.png)](https://github.com/propensive/escapade/)&nbsp;
[![Escritoire](https://github.com/propensive/escritoire/raw/main/doc/images/128x128.png)](https://github.com/propensive/escritoire/)&nbsp;
[![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/)&nbsp;
[![Exoskeleton](https://github.com/propensive/exoskeleton/raw/main/doc/images/128x128.png)](https://github.com/propensive/exoskeleton/)&nbsp;
[![Fulminate](https://github.com/propensive/fulminate/raw/main/doc/images/128x128.png)](https://github.com/propensive/fulminate/)&nbsp;
[![Galilei](https://github.com/propensive/galilei/raw/main/doc/images/128x128.png)](https://github.com/propensive/galilei/)&nbsp;
[![Gastronomy](https://github.com/propensive/gastronomy/raw/main/doc/images/128x128.png)](https://github.com/propensive/gastronomy/)&nbsp;
[![Gesticulate](https://github.com/propensive/gesticulate/raw/main/doc/images/128x128.png)](https://github.com/propensive/gesticulate/)&nbsp;
[![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/)&nbsp;
[![Guillotine](https://github.com/propensive/guillotine/raw/main/doc/images/128x128.png)](https://github.com/propensive/guillotine/)&nbsp;
[![Harlequin](https://github.com/propensive/harlequin/raw/main/doc/images/128x128.png)](https://github.com/propensive/harlequin/)&nbsp;
[![Hieroglyph](https://github.com/propensive/hieroglyph/raw/main/doc/images/128x128.png)](https://github.com/propensive/hieroglyph/)&nbsp;
[![Honeycomb](https://github.com/propensive/honeycomb/raw/main/doc/images/128x128.png)](https://github.com/propensive/honeycomb/)&nbsp;
[![Hyperbole](https://github.com/propensive/hyperbole/raw/main/doc/images/128x128.png)](https://github.com/propensive/hyperbole/)&nbsp;
[![Imperial](https://github.com/propensive/imperial/raw/main/doc/images/128x128.png)](https://github.com/propensive/imperial/)&nbsp;
[![Inimitable](https://github.com/propensive/inimitable/raw/main/doc/images/128x128.png)](https://github.com/propensive/inimitable/)&nbsp;
[![Iridescence](https://github.com/propensive/iridescence/raw/main/doc/images/128x128.png)](https://github.com/propensive/iridescence/)&nbsp;
[![Jacinta](https://github.com/propensive/jacinta/raw/main/doc/images/128x128.png)](https://github.com/propensive/jacinta/)&nbsp;
[![Kaleidoscope](https://github.com/propensive/kaleidoscope/raw/main/doc/images/128x128.png)](https://github.com/propensive/kaleidoscope/)&nbsp;
[![Larceny](https://github.com/propensive/larceny/raw/main/doc/images/128x128.png)](https://github.com/propensive/larceny/)&nbsp;
[![Mercator](https://github.com/propensive/mercator/raw/main/doc/images/128x128.png)](https://github.com/propensive/mercator/)&nbsp;
[![Merino](https://github.com/propensive/merino/raw/main/doc/images/128x128.png)](https://github.com/propensive/merino/)&nbsp;
[![Mosquito](https://github.com/propensive/mosquito/raw/main/doc/images/128x128.png)](https://github.com/propensive/mosquito/)&nbsp;
[![Nectary](https://github.com/propensive/nectary/raw/main/doc/images/128x128.png)](https://github.com/propensive/nectary/)&nbsp;
[![Nettlesome](https://github.com/propensive/nettlesome/raw/main/doc/images/128x128.png)](https://github.com/propensive/nettlesome/)&nbsp;
[![Octagenarian](https://github.com/propensive/octogenarian/raw/main/doc/images/128x128.png)](https://github.com/propensive/octogenarian/)&nbsp;
[![Oubliette](https://github.com/propensive/oubliette/raw/main/doc/images/128x128.png)](https://github.com/propensive/oubliette/)&nbsp;
[![Panopticon](https://github.com/propensive/panopticon/raw/main/doc/images/128x128.png)](https://github.com/propensive/panopticon/)&nbsp;
[![Parasite](https://github.com/propensive/parasite/raw/main/doc/images/128x128.png)](https://github.com/propensive/parasite/)&nbsp;
[![Contingency](https://github.com/propensive/contingency/raw/main/doc/images/128x128.png)](https://github.com/propensive/contingency/)&nbsp;
[![Polyvinyl](https://github.com/propensive/polyvinyl/raw/main/doc/images/128x128.png)](https://github.com/propensive/polyvinyl/)&nbsp;
[![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/)&nbsp;
[![Profanity](https://github.com/propensive/profanity/raw/main/doc/images/128x128.png)](https://github.com/propensive/profanity/)&nbsp;
[![Punctuation](https://github.com/propensive/punctuation/raw/main/doc/images/128x128.png)](https://github.com/propensive/punctuation/)&nbsp;
[![Quantitative](https://github.com/propensive/quantitative/raw/main/doc/images/128x128.png)](https://github.com/propensive/quantitative/)&nbsp;
[![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/)&nbsp;
[![Savagery](https://github.com/propensive/savagery/raw/main/doc/images/128x128.png)](https://github.com/propensive/savagery/)&nbsp;
[![Scintillate](https://github.com/propensive/scintillate/raw/main/doc/images/128x128.png)](https://github.com/propensive/scintillate/)&nbsp;
[![Serpentine](https://github.com/propensive/serpentine/raw/main/doc/images/128x128.png)](https://github.com/propensive/serpentine/)&nbsp;
[![Spectacular](https://github.com/propensive/spectacular/raw/main/doc/images/128x128.png)](https://github.com/propensive/spectacular/)&nbsp;
[![Surveillance](https://github.com/propensive/surveillance/raw/main/doc/images/128x128.png)](https://github.com/propensive/surveillance/)&nbsp;
[![Symbolism](https://github.com/propensive/symbolism/raw/main/doc/images/128x128.png)](https://github.com/propensive/symbolism/)&nbsp;
[![Tarantula](https://github.com/propensive/tarantula/raw/main/doc/images/128x128.png)](https://github.com/propensive/tarantula/)&nbsp;
[![Telekinesis](https://github.com/propensive/telekinesis/raw/main/doc/images/128x128.png)](https://github.com/propensive/telekinesis/)&nbsp;
[![Turbulence](https://github.com/propensive/turbulence/raw/main/doc/images/128x128.png)](https://github.com/propensive/turbulence/)&nbsp;
[![Ulysses](https://github.com/propensive/ulysses/raw/main/doc/images/128x128.png)](https://github.com/propensive/ulysses/)&nbsp;
[![Umbrageous](https://github.com/propensive/umbrageous/raw/main/doc/images/128x128.png)](https://github.com/propensive/umbrageous/)&nbsp;
[![Villainy](https://github.com/propensive/villainy/raw/main/doc/images/128x128.png)](https://github.com/propensive/villainy/)&nbsp;
[![Wisteria](https://github.com/propensive/wisteria/raw/main/doc/images/128x128.png)](https://github.com/propensive/wisteria/)&nbsp;
[![Xylophone](https://github.com/propensive/xylophone/raw/main/doc/images/128x128.png)](https://github.com/propensive/xylophone/)
[![Zeppelin](https://github.com/propensive/zeppelin/raw/main/doc/images/128x128.png)](https://github.com/propensive/zeppelin/)
