# Syntax and Formatting

This standard has moved. The syntactic and whitespace conventions Soundness
follows are now **Consequent Style**, maintained in its own repository
together with the compiler plugin that checks them:

- [The standard](https://github.com/propensive/consequent/blob/main/doc/consequent-style.md)
- [Rule reference](https://github.com/propensive/consequent/tree/main/doc/rules)

Nothing about the conventions changed in the move; the document was
generalised so that it governs any project that adopts it, rather than
Soundness specifically. A project fixes two parameters — the exact text of
its licence header (and hence its length in lines) and, optionally, the name
of its umbrella re-export package — and every other convention applies
unchanged. Both are supplied to the checker as options; see the
[Consequent README](https://github.com/propensive/consequent#options).

## Rule identifiers

Rules were renumbered at the same time. Where this project's docs and history
refer to a rule as `SN-nnn`, the equivalent Consequent rule has an identifier
of the form `<principle><number>` — the letter naming the principle it derives
from, then its number within that principle:

| Was | Now |
| --- | --- |
| `SN-799` | `F1` — licence-header block comment |
| `SN-230` | `F4` — line length |
| `SN-302` | `L1` — top-level import rules |
| `SN-742` | `L4` — umbrella re-exports |

The full table is in the
[rule reference](https://github.com/propensive/consequent/blob/main/doc/rules/README.md#migrating-from-the-sn--identifiers).
Each retired `SN-nnn` page under [`doc/errors/`](../errors/) also names its
replacement, so an older reference still resolves.

## The checker

The plugin that enforces this standard was `lib/decorum` in this repository.
It is now published separately as `dev.propensive:consequent_<scala-version>`
and applied through `consequentToolchain` in `build.mill`.
