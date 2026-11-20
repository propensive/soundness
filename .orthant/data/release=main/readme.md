# Soundness's documentation

The documentation is kept in an [Orthant](https://github.com/propensive/orthant) store, in
`.orthant/` at the top of the repository, and not as files in git. The store holds every
document in each language (`en`, and `fr` beneath it) at each release (0.76.0, 0.77.0 and so on,
then `main` for what is not yet released). It stores only the lines that differ from the point
above. This directory is the store's working tree. git ignores everything in it but this file,
so the directory holds only this file until a point is checked out:

```
orthant checkout en main            # the English as it is on main: soundness/, philosophy/, …
orthant checkout fr main --indirect   # the French, each line beside the English it translates
```

A value is given for each axis: without `main`, the release is the one last checked out, or the
oldest. The first checkout in a fresh clone needs `--force`, because Orthant takes this file,
which it has not yet checked out, for an edit.

This file is the one document kept twice, in git and in the store at `main`, so that a fresh
clone has it to read. A change to it is checked in like any other, and the file is committed as
well.

## How an edit travels

A point is one value on each axis, such as `en` at `main` or `fr` at 0.76.0. Each point sits
below the point with the parent value on either axis: `main` below 0.77.0 below 0.76.0, and `fr`
below `en`. An edit checked in at a point is seen at every point below it which has not changed
the same lines. A correction to the English at 0.76.0 therefore appears in the English of every
later release, and marks its French translation as needing a look at each.

Every kind of change below comes down to two choices: the point at which the edit is stored,
and how far below that point it reaches.

| The change                                         | Where to make it                               |
|----------------------------------------------------|------------------------------------------------|
| New or changed text for the next release           | `en` at `main`, plain `checkin`                |
| A correction to text that was already released     | `en` at `main`, `checkin --origin release`     |
| A correction to one release, and no later one      | `en` at that release, `checkin --only release` |
| Translating, or revising a translation             | `orthant todo fr --checkout`, then `checkin`   |
| An English edit which leaves the translation right | `checkin`, then `orthant confirm` in `fr`      |
| A new language                                     | `orthant axis value language de en`            |
| A dialect                                          | `orthant axis value language en/US en`         |

`orthant status` says what is not checked in, `orthant diff` shows it, and `orthant checkin
--dry-run` names the points an edit would change without storing anything. Run the dry run first
whenever the reach of an edit is in doubt.

## Changing what is not yet released

This is the usual case: a new tutorial section, an entry in the migration notes, a reworded
paragraph.

```
orthant checkout en main      # the English, as plain documents
$EDITOR doc/standards/logging.md
orthant diff                  # the edits, before they are stored
orthant checkin               # store them at en/main, and so at fr/main
fuel compile                  # check the documents, as the store now holds them
git add .orthant && git commit
```

Fuel compiles only what is checked in, never the working tree. The check-in reports the work it
leaves for translators:

```
  stored  modified  standards/logging.md
  leaves  language=fr release=main with 1 more line to translate
```

That work does not block the change. The French shows the English line until it is translated.

## Adding, removing and renaming documents

A new file in the working tree is a new document at the point checked out and every point below
it. `orthant checkin` stores it.

A deleted file is not removed by a plain check-in, which reports it as `missing` and skips it.
`orthant checkin --remove` deletes it at this point and below. The document stays in the
releases above: removed at `main`, it is still part of 0.77.0.

Documents are known by their paths, so a rename is a removal and an addition: check in with
`--remove`. The translation of the old document does not follow to the new one. Copy it across
in the translation's own checkout.

## Correcting what was already released

A released document is changed only to correct an error in it: a wrong signature, a broken
example, a misspelt name. The correction belongs at the release where the error first appeared,
so that every later release receives it.

The simplest way is to make the correction at `main`, where the documents are already checked
out, and let Orthant send each changed line back to the release which introduced it:

```
orthant checkout en main
$EDITOR doc/soundness/modules/sockets.md
orthant checkin --origin release --dry-run   # the points it would change
orthant checkin --origin release
```

With `--origin release`, a change which replaces or deletes lines is stored at the earliest
release which has those lines. A change which only inserts lines has no earlier lines to follow,
and stays at `main`. One check-in may therefore hold both a correction to 0.76.0 and new text
for the next release, and each goes to the right place.

To choose the release rather than have it found, name it:

```
orthant checkin --since 0.76.0               # from main: store the whole edit at 0.76.0
```

or check that release out and edit it there:

```
orthant checkout en 0.76.0
$EDITOR doc/soundness/modules/sockets.md
orthant checkin                              # 0.76.0, and every release after it
```

`--since` is the one to use for an insertion which should have been in an earlier release, since
`--origin` would leave it at `main`.

Sometimes a correction is true of one release alone: a note about a defect which the next
release fixed, say. `--only release` keeps the edit to the release checked out, in every
language, and leaves later releases as they were:

```
orthant checkout en 0.76.0
$EDITOR doc/soundness/modules/sockets.md
orthant checkin --only release               # en and fr at 0.76.0; not 0.77.0, not main
```

The French at that release goes stale where it translates the lines changed, as after any
English edit. The points of later releases are not touched.

A released `migration/<version>.md` follows the same rule as any released document, and is
edited only to correct an error in it. New entries go in the unreleased file, at `main`.

## Translating

A translator need not know which release holds the work. `orthant todo` lists each piece at the
earliest point which has it, where doing it once does it for every later release:

```
orthant todo fr               # what is to be translated or checked, document by document
orthant todo fr --checkout    # check out the first of those points, ready to work on
orthant grid                  # every language at every release, at a glance
```

The checkout is an *indirect view*. Every line begins with a sigil and a space:

| Sigil | The line is                                                              |
|-------|--------------------------------------------------------------------------|
| (two spaces) | shared by the English and the translation, such as a line of code |
| `-`   | a line of the English                                                    |
| `+`   | the translation of the `-` lines above it                                |
| `?`   | a line of the English which has no translation yet                       |
| `!`   | the English which a translation was written against, before it changed   |

Translate a `?` line by adding `+` lines beneath it:

```
? # Soundness Module Layout Standards
+ # Disposition des modules
```

A `-` line with no `+` line beneath it is left out of the translation. Then check in, which says
how much less there is to do:

```
orthant checkin
  stored  modified  standards/module-layout.md
  leaves  language=fr release=0.76.0 with 1 fewer line to translate
```

`orthant checkout fr main --direct` shows the translation as plain documents, as a reader sees
it, with the English standing in wherever a line is not yet translated.

## Bringing a translation up to date

When the English beneath a translated passage changes, the translation is *stale*. It is still
shown to readers, and it is listed by `orthant todo` and `orthant status`. In the indirect view
the passage shows three things: the English it was written against (`!`), the English as it is
now (`-`), and the translation (`+`):

```
! A nameserver is an `Endpoint` of a UDP port. `query` sends a
- A nameserver is an `Endpoint` of a UDP port. `ask` sends a
+ Un serveur de noms est un `Endpoint` d'un port UDP. `query` envoie un
```

Correct the `+` lines, delete the `!` lines, and check in:

```
- A nameserver is an `Endpoint` of a UDP port. `ask` sends a
+ Un serveur de noms est un `Endpoint` d'un port UDP. `ask` envoie un
```

In a long document, most passages are not stale. A view against the change hides them, and
marks only what changed in the English since a release:

```
orthant ref 0.77.0            # what the English has changed since 0.77.0, and nothing else
orthant ref --parent          # back to the whole indirect view
orthant ref --none            # the plain translation
```

`orthant ref` changes the view and keeps the edits in the working tree.

## An English edit which leaves the translation right

A corrected typo or a re-wrapped paragraph changes English lines without changing what they
say. The translation is marked stale all the same, because Orthant cannot know. After checking
the English in, confirm the translation as it stands:

```
orthant checkout en main
$EDITOR doc/soundness/modules/audio.md
orthant checkin               # leaves fr/main with 1 more stale passage
orthant checkout fr main --indirect
orthant confirm soundness/modules/audio.md
```

`orthant confirm` clears every stale passage of the documents named, or of every document if
none is named. Read the passages first (`orthant status` lists them by line), and confirm by
path.

## Adding a language

A new language is a new value on the `language` axis, beneath the language it is translated
from:

```
orthant axis value language de en
orthant todo de --checkout    # de at 0.76.0, every line a `?`
```

Begin at the oldest release, as `todo` proposes. A translation made at 0.76.0 serves every later
release, and only the lines which later releases changed remain to translate there. A
translation begun at `main` leaves the released documents in English.

The new value is recorded in `.orthant/config.tel`. Commit that file with the first of the
translation. A translation may be checked in a document at a time, and published before it is
complete: untranslated lines read in the language above.

## Dialects and regional variants

A dialect is a value beneath its language whose name has the same language code: `en/US` beneath
`en`, `fr/CA` beneath `fr`.

```
orthant axis value language en/US en
orthant checkout en/US main
$EDITOR doc/standards/examples.md             # "colours" becomes "colors"
orthant checkin
```

Unlike a translation, a dialect starts complete. It has every line of its parent, nothing is
marked as untranslated, and `orthant todo en/US` reports no work. Only the lines which the
dialect changes are stored, and every other line follows the parent: a later edit to the English
appears in `en/US` without further work, unless it touches a line the dialect has changed, which
then becomes a stale passage to check.

`orthant checkout en/US main --indirect` shows only the differences, as `-` and `+` pairs, and
is the quickest way to review them.

A dialect's edit obeys the same rules as any other. Checked in at `main`, it applies to `main`
alone. To give a spelling to every release, check the dialect out at the oldest release, or
check in with `--since 0.76.0`.

## Reading without checking out

```
orthant cat soundness/modules/sockets.md fr 0.76.0   # one document at a point
orthant export out/site fr 0.77.0                    # every document at a point, as plain files
orthant export out/site fr 0.77.0 --strict           # refuse while any translation work is left
```

The scripts in `etc/` which read the documents read such a copy, `out/doc`, made by `make docs`
(`orthant export out/doc en main`), and do not read this directory.

## Keeping the store and the working tree in step

- **After a pull, a rebase or a branch switch** that changed `.orthant/`, run `orthant update`.
  It brings the working tree up to the store as it now is, and carries the edits which are not
  checked in onto the new documents.
- **Before committing**, `orthant status --all` reports on every point, and `orthant check`
  verifies everything the store holds.
- **Commit** `.orthant/config.tel` and `.orthant/data/`. This directory and `.orthant/work/`,
  which records what is checked out, are ignored by git. `.orthant/` is outside the CI input
  set, so a change to the documentation alone needs no new build to be attested.
- **To abandon edits**, `orthant checkout en main --force` discards whatever is not checked in.
- **After a release is tagged**, `orthant sync` takes the tag up as a value. Everything checked
  in at `main` becomes that release's, and `main` starts again beneath it. Commit the change to
  `.orthant/config.tel`.

`orthant <command> --help` describes each command.
