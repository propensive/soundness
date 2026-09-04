# Migration notes

Soundness moves quickly and renames, moves and reshapes its API between releases. Downstream
code is expected to be upgraded by an LLM agent rather than by hand, so from the 0.64.0
release onwards every release ships a machine-oriented record of what changed.

## How it works

- `doc/migration/pending.md` accumulates the changes since the last release. Every PR that
  makes a material change to the public API or observable behaviour adds an entry to it.
- When a release is cut, `pending.md` is renamed to `doc/migration/<version>.md`
  (for example `doc/migration/0.65.0.md` for Soundness 0.65.0) and a fresh, empty
  `pending.md` is started. Released files are not edited afterwards except to correct
  mistakes.
- The result is one file per release, each describing exactly the changes between the
  previous release and that one.

## Upgrading downstream code

To upgrade a project from version `A` to version `B`, instruct an LLM agent to read every file
`doc/migration/<v>.md` with `A < v <= B`, in ascending version order, and to apply the changes
each one describes to the project's code. For example, moving from 0.64.0 to 0.67.0 means
reading `0.65.0.md`, `0.66.0.md` and `0.67.0.md`, in that order.

## What the files contain

The files are written for an intelligent LLM, not for people. They are precise, dense and
complete: every rename, move, signature change, removal, changed default and changed
behaviour that a consumer of a Soundness library could observe. They say *what* changed,
with fully-qualified old and new names and exact semantics, and leave working out *how* to
adapt a particular codebase to the agent reading them, except where the adaptation is not
deducible from the change itself. `AGENTS.md` at the repository root specifies the format
and the rules for maintaining them.
