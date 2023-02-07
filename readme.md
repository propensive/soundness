[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/dissonance/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/dissonance/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Dissonance

Dissonance implements Eugene Myers' diff algorithm in Scala as a pure function
on immutable data structures. Using it is as simple as calling
`diff(left, right)`, where `left` and `right` are sequences of like-typed data
to be comparied; the result is an instance of `Diff`, a sequence of additions,
deletions and no-change nodes representing each item in the left and right
sequence.

## Features

- implements Myers' diff algorithm
- can be used with any data type, not just strings
- specify a custom comparison function enabling fine-grained merges on "similar" data
- diffs are simple immutable structures of `Del`, `Ins` and `Keep` nodes


## Availability

Dissonance has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

Dissonance provides only a single method, `diff`, which takes a `left` and
`right` parameter, both sequences of the same type, and generates a sequence of
the edits required to transform the left sequence into the right sequence. Each
element of the resultant sequence is either a `Keep` value, corresponding to a
value in both the left and right sequences, an `Ins` value (for _insertions_)
which exists only in the right sequence, or a `Del` value (for _deletions_)
which exists only in the left sequence.

The naming of these enumeration cases corresponds to a translation of the left
sequence into the right sequence, but could describe a translation from the
right sequence to the left if the roles are reversed. The `Diff#flip` method
can automatically reverse the translation.

Each of the three possible `Change` cases, `Ins`, `Del` and `Keep` includes the
relevant value, as well as the indices of that value in each sequence it exists
in: for `Ins`, the right; for `Del`, the left, and for `Keep` both a `left` and
`right` index.

### Custom equality

By default, elements of the left and right sequences will be considered _the
same_ (producing `Keep` values) if they are equal according to Java's universal
equality method, `AnyRef#equals`. However, other forms of equality (or
similarity) may exist, and it may be useful to consider two elements to be _the
same_, even if they are not equal. A common example would be if they had the
same ID, even if their content is different. That would introduce the
possibility to do deeper comparisons on those values.

The `diff` method takes an optional third parameter, `cmp`, of type `(T, T) ->
Boolean` which determines whether two elements are _the same_ for the purposes
of the diff.



## Related Projects

The following _Scala One_ libraries are dependencies of _Dissonance_:

[![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

The following _Scala One_ libraries are dependents of _Dissonance_:

[![Cellulose](https://github.com/propensive/cellulose/raw/main/doc/images/128x128.png)](https://github.com/propensive/cellulose/) &nbsp; [![Chiaroscuro](https://github.com/propensive/chiaroscuro/raw/main/doc/images/128x128.png)](https://github.com/propensive/chiaroscuro/) &nbsp;

## Status

Dissonance is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Dissonance is designed to be _small_. Its entire source code currently consists
of 230 lines of code.

## Building

Dissonance can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Dissonance are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/dissonance/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Dissonance easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Dissonance was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Dissonance_'s purpose is to detect differences—or dissonance, the places where they are not in agreement—between different objects.

## License

Dissonance is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
