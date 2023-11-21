[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/dissonance/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/dissonance/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Dissonance

__Myers' diff algorithm in Scala__

Dissonance implements Eugene Myers' diff algorithm in Scala as a pure function
on immutable data structures. Using it is as simple as calling
`diff(left, right)`, where `left` and `right` are sequences of like-typed data
to be compared; the result is an instance of `Diff`, a sequence of additions,
deletions and no-change nodes representing each item in the left and right
sequence.

## Features

- implements Myers' diff algorithm
- can be used with any data type, not just strings
- specify a custom comparison function enabling fine-grained merges on "similar" data
- diffs are simple immutable structures of `Del`, `Ins` and `Par` nodes


## Availability

Dissonance has not yet been published as a binary.

## Getting Started

Dissonance provides only a single method, `diff`, which takes a `left` and
`right` parameter, both sequences of the same type, and generates a sequence of
the edits required to transform the left sequence into the right sequence, a
result of type `Diff`. Each element of the resultant sequence is either a `Par`
value (for _parity_), corresponding to a value in both the left and right
sequences, an `Ins` value (for _insertions_) which exists only in the right
sequence, or a `Del` value (for _deletions_) which exists only in the left
sequence.

The naming of these enumeration cases corresponds to a translation of the left
sequence into the right sequence, but could describe a translation from the
right sequence to the left if the roles are reversed. The `Diff#flip` method
can automatically reverse the translation.

Each of the three possible `Edit` cases, `Ins`, `Del` and `Par` includes the
relevant value, as well as the indices of that value in each sequence it exists
in: for `Ins`, the right; for `Del`, the left, and for `Par` both a `left` and
`right` index.

### Custom equality

By default, elements of the left and right sequences will be considered _the
same_ (producing `Par` values) if they are equal according to Java's universal
equality method, `AnyRef#equals`. However, other forms of equality (or
similarity) may exist, and it may be useful to consider two elements to be _the
same_, even if they are not equal according to `AnyRef#equals`. A common example
would be if they had the same ID, even if their content is different.

The `diff` method takes an optional third parameter, `compare`, of type
`(ElemType, ElemType) -> Boolean` which determines whether two elements are
_the same_ for the purposes of the diff.

### Collation

Given a `Diff` instance, created from two sequences, its `collate` method can be
used to group subsequences of changes together into changed and unchanged
regions. For any given _diff_ result, this collation can be done unambiguously:
runs of adjacent `Par`s are grouped together into an `Unchanged` instance, and
between each run will be one or more `Ins` and/or `Del` values which are grouped
into a `Changed` instance, consisting of a sequence of deletions and a sequence of
insertions.

Conventionally, the diff algorithm will output deletions before insertions, but in
terms of correctness, the order in which deletions and insertions are applied does
not matter. The result of collation will always be an alternating sequence of
`Changed` and `Unchanged` sections.

### Deeper Diff

`Diff` also provides a method, `rdiff`, which will use these collated changes and
perform a further diff on each changed section which contains at least one insertion
and at least one deletion, since some of these may be more usefully viewed as
_substitutions_ or replacements, rather than unrelated insertions and deletions. The
`rdiff` method requires a _similarity_ function, `(ElemType, ElemType) -> Boolean`,
which can identify values in the left and right sequences which are similar (but
which have already been identified as non-equal). These are thus presented as `Sub`
nodes.

Since the order in which each deletion or insertion is applied within a changed
section does not affect correctness, each nested diff may decide to identify certain
insertion/deletion pairs as "substitutes" and fit the remaining insertions and
deletions around them.

The result is an instance of `RDiff` containing a (now flattened) sequence of
`Ins`s, `Del`s, `Par`s and `Sub`s. Note that all but `Sub` have the supertype,
`Edit`, while all have the supertype, `Change`.

Furthermore, `rdiff` has a second, optional, parameter, `subSize`, which may be used
to automatically convert short, changed sections with an equal number of insertions
and deletions, into a sequence of `Sub`s, provided they are less than `subSize`. The
value defualts to `1`. So, as isolated insertion/deletion region of length `subSize`
would become a series of `Sub`s, regardless of whether the similarity function returns
`true` or `false`.

### Applying a Diff

A `Diff` instance, say `diff`,  may be applied to a sequence, `seq`, with,
```scala
diff.applyTo(seq)
```
to produce a new sequence. This is performed lazily, so the result is a `LazyList`.

By default, a `Par` "edit" will leave the element in the original sequence unchanged.
However, `applyTo` has an optional second parameter, `update`, which can be provided
to specify how `Par` edits should be handled. Of course, if the edit is a `Par`, the
original and diff versions of element _should_ be equal anyway, but may not be if,
for example, the diff was constructed with a different "compare" function or the
diff is being incorrectly applied to the wrong input.

Three reasonable alternatives for `update` are:
- to ignore the value in the diff, and keep the original (the default),
- to ignore the original value and replace it with the value in the diff
- to check that the original and diff elements are equal, and fail if they are not

But more involved possibilities are available which could "merge" the original and
diff elements, or prefer one or the other.




## Status

Dissonance is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Dissonance is designed to be _small_. Its entire source code currently consists
of 461 lines of code.

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

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows three sets of concentric circles, offset from each other, and creating an interference pattern, alluding to dissonance between them.

## License

Dissonance is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
