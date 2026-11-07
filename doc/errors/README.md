# Soundness error codes

Every compile error raised by a Soundness macro carries a stable
identifier of the form `SN-d` or `SN-d.e`, where:

- `d` is a globally-unique error number, zero-padded to three digits.
- `e`, when present, identifies a specific variant of the error
  (typically the case of a `Reason` enum).

The rendered prefix appears in compile output as `[↯SN-d.e]`,
coloured if the compiler's `-color` setting is on.

Each error is documented in a markdown file at `<d>.md`. When an
error has variants, every `SN-d.e` is documented in the same `<d>.md`
file, under `## Variants`: as a row of a `tabular` table, with the
columns `Code`, `Reason`, and whichever of `Meaning`, `Message` (an
example), `Resolution` and `Wire code` its variants have; or, when
there are five or fewer and each needs an explanation and a resolution
of its own, as a `### SN-d.e` sub-section, with `**Example.**` and
`**Resolution.**` paragraphs. A variant described only by its example
message is always a row.

A code is never reused. When an error is removed, its page stays, marked
`(retired)`, saying what became of it: a reference to the code from an
older release still resolves, and the number cannot be allocated again
by accident. A retired page says what replaced the code in a `Was` and
`Now` table, with a row for the code and for each of its variants that
has a successor, even when there is only one.

Every page names the source file its error is raised from. It does not
give a line number, since a line number is invalidated by any edit above
it and nothing checks that it is still right.

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
row  `S8`  -128  127  [`SN-287`](287.md)  [`SN-768`](768.md)
row  `S16`  -32768  32767  [`SN-046`](046.md)  [`SN-461`](461.md)
row  `U8`  0  255  [`SN-689`](689.md)  [`SN-709`](709.md)
row  `U16`  0  65535  [`SN-764`](764.md)  [`SN-772`](772.md)
```

The binary and hexadecimal literals of Hypotenuse:

```tabular
column  Code
column  Literal
column  Rejects
row  [`SN-118`](118.md)  `bin"…"`  a character other than `0` or `1`
row  [`SN-167`](167.md)  `bin"…"`  a width other than 8, 16, 32 or 64 bits
row  [`SN-137`](137.md)  `hex"…"`  an odd number of digits
row  [`SN-148`](148.md)  `hex"…"`  a character that is not a hexadecimal digit
```

The time literals checked by Aviation's `validTime` macro:

```tabular
column  Code
column  Rejects
row  [`SN-735`](735.md)  an argument which is not a literal `Double`
row  [`SN-955`](955.md)  a negative time
row  [`SN-081`](081.md)  an hour above 12
row  [`SN-093`](093.md)  a minute value above 59
row  [`SN-582`](582.md)  minutes not written as exactly two digits
```
