# Soundness error codes

Every error Soundness numbers carries a stable identifier, its
[bolt code](https://github.com/propensive/bolt), of the form `↯/s.m.n` or
`↯/s.m.n.v`, where:

- `s` is the letter by which bolt codes name Soundness;
- `m` is the number of the module the error is in, as [the list of
  modules](modules.md) and `bolt.tel` give it: modules are
  numbered in an order in which each comes after those it depends on, as
  they stood when each was numbered, and a new module takes the next number;
- `n` is the number of the error within its module, the next free one when
  the error is written;
- `v`, when present, identifies a specific variant of the error (typically
  the case of a `Reason` enum).

An error which is reported on account of another project's carries that
error's code after its own: Stratiform's `Tel.Error` for a document the TEL
Specification rejects with `↯/t.1.7` is `↯/s.118.1/t.1.7`.

The rendered prefix appears in compile output as `[↯/s.m.n.v]`, coloured if
the compiler's `-color` setting is on. In the source, the numbers are the
first arguments of `fulminate.Error` and of `halt`: `Error(118, 1,
reason.number)`.

`bolt.tel`, beside this file, is the record of every code: the
number and the name of each module, error and variant, in the `bolt` schema.
The index of bolt codes names it as Soundness's, and holds no copy, so an
error which is added, renamed or retired is recorded there, in the same
change. A name is in kebab-case, and spells the long form of a code:
`↯/s.87.2.1` is `↯/soundness.legerdemain.query.missing`. An error type
`Query.Error` is named `query`, and a `Reason` such as `BadSignature`,
`bad-signature`.

Each error is documented in a markdown file at `<m>.<n>.md`. When an
error has variants, every `↯/s.m.n.v` is documented in the same file,
under `## Variants`: as a row of a `tabular` table, with the
columns `Code`, `Reason`, and whichever of `Meaning`, `Message` (an
example), `Resolution` and `Wire code` its variants have; or, when
there are five or fewer and each needs an explanation and a resolution
of its own, as a `### ↯/s.m.n.v` sub-section, with `**Example.**` and
`**Resolution.**` paragraphs. A variant described only by its example
message is always a row.

A code is never reused. When an error is removed, its page stays, marked
`(retired)`, saying what became of it, and its number is marked retired in
`bolt.tel`: a reference to the code from an older release still resolves, and
the number cannot be allocated again by accident. A retired page says what
replaced the code in a `Was` and `Now` table, with a row for the code and
for each of its variants that has a successor, even when there is only one.

Every page names the source file its error is raised from. It does not
give a line number, since a line number is invalidated by any edit above
it and nothing checks that it is still right.

## The `SN-` codes

Until Soundness adopted bolt codes, an error was `SN-ddd`, a three-digit
number unique across every module. [`sn.md`](sn.md) gives the bolt code of
each `SN-` code which was live then. The pages named with three digits
(`013.md`) are those of the `SN-` codes retired before then, which never
had a bolt code; they are kept so that a reference from an older release
still resolves.

## Families

Some errors are raised by one check against different limits, so each
has a page of its own, but they are easiest to compare together.

The integer literals of Hypotenuse are checked against the range of
their type:

```tabular
column  Type
column  Least
column  Greatest
column  Below the range
column  Above the range
row  `S8`  -128  127  [`↯/s.27.6`](27.6.md)  [`↯/s.27.13`](27.13.md)
row  `S16`  -32768  32767  [`↯/s.27.1`](27.1.md)  [`↯/s.27.7`](27.7.md)
row  `U8`  0  255  [`↯/s.27.8`](27.8.md)  [`↯/s.27.9`](27.9.md)
row  `U16`  0  65535  [`↯/s.27.12`](27.12.md)  [`↯/s.27.14`](27.14.md)
```

The binary and hexadecimal literals of Hypotenuse:

```tabular
column  Code
column  Literal
column  Rejects
row  [`↯/s.27.2`](27.2.md)  `bin"…"`  a character other than `0` or `1`
row  [`↯/s.27.5`](27.5.md)  `bin"…"`  a width other than 8, 16, 32 or 64 bits
row  [`↯/s.27.3`](27.3.md)  `hex"…"`  an odd number of digits
row  [`↯/s.27.4`](27.4.md)  `hex"…"`  a character that is not a hexadecimal digit
```

The time literals checked by Aviation's `validTime` macro:

```tabular
column  Code
column  Rejects
row  [`↯/s.73.8`](73.8.md)  an argument which is not a literal `Double`
row  [`↯/s.73.11`](73.11.md)  a negative time
row  [`↯/s.73.1`](73.1.md)  an hour above 12
row  [`↯/s.73.2`](73.2.md)  a minute value above 59
row  [`↯/s.73.7`](73.7.md)  minutes not written as exactly two digits
```
