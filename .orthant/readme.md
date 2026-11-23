# Soundness's documentation

The documentation is kept in an [Orthant](https://github.com/propensive/orthant) store, in this
directory. The store holds every document in each language (`en`, and `fr` and the other
translations beneath it) at each release (0.76.0, 0.77.0 and so on, then `main` for what is not
yet released). It stores only the lines that differ from the point above.

The English at `main` is also in git as ordinary files, in `doc/` at the top of the repository.
That is the store's *live tree*: the documents are edited there, like any other file, and
`orthant tag` stores them in the store. Orthant never writes to `doc/`.

```
$EDITOR doc/standards/logging.md      # the English on main is edited where it is
orthant status                        # what is not yet stored, and what storing it leaves to do
orthant tag                           # store doc/ in the store, at en/main
```

Every other point — a translation, or the English of a release — is in the store alone. One
document is edited with `orthant edit`, and a whole point is checked out into the store's
working tree, `.orthant/work/tree/`, which git ignores:

```
orthant edit standards/logging.md fr  # the French of one document, in $EDITOR
orthant checkout fr main --indirect   # all the French, each line beside the English it translates
```

A value is given for each axis: without `main`, the release is the one last checked out, or the
oldest. This file is not in the store. It is an ordinary file in git, and no checkout touches
it.

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
| New or changed text for the next release           | in `doc/`, then `orthant tag`                  |
| A correction to text that was already released     | `en` at that release, plain `checkin`          |
| A correction to one release, and no later one      | `en` at that release, `checkin --only release` |
| Translating, or revising a translation             | `orthant todo fr --checkout`, then `checkin`   |
| An English edit which leaves the translation right | `orthant tag`, then `orthant confirm` in `fr`  |
| A new language                                     | `orthant axis value language de en`            |
| A dialect                                          | `orthant axis value language en/US en`         |

`orthant status` says what is not yet stored, in `doc/` and in the working tree, and `orthant
tag --dry-run` and `orthant checkin --dry-run` name the points an edit would change without
storing anything. Run the dry run first whenever the reach of an edit is in doubt.

## Changing what is not yet released

This is the usual case: a new tutorial section, an entry in the roadmap, a reworded
paragraph.

```
$EDITOR doc/standards/logging.md
orthant status                # the edits in doc/, and what they will leave to translate
orthant tag                   # store them at en/main, and so at fr/main
make docs                     # the English as the store now holds it, in out/doc
git add doc .orthant && git commit
```

`doc/` and `.orthant/` are committed together. `make docs`, which the checks run first, fails
while `doc/` has an edit which `orthant tag` has not stored. The tag reports the work it leaves
for translators:

```
  stored  modified  standards/logging.md
  leaves  language=fr release=main with 1 more line to translate
```

That work does not block the change. The French shows the English line until it is translated.

`orthant tag` here never takes a name. A release of Soundness is a git tag, and `orthant sync`
takes it up.

## Adding, removing and renaming documents

A new file in `doc/` is a new document at `main` and every point below it. `orthant tag`
stores it.

A deleted file is not removed by a plain `orthant tag`, which reports it as `missing` and skips
it. `orthant tag --remove` deletes it at `main` and below. The document stays in the releases
above: removed at `main`, it is still part of 0.77.0.

Documents are known by their paths, so a rename is a removal and an addition: store it with
`--remove`. The translation of the old document does not follow to the new one. Copy it across
in the translation's own checkout.

## Correcting what was already released

A released document is changed only to correct an error in it: a wrong signature, a broken
example, a misspelt name. The correction belongs at the release where the error first appeared,
so that every later release receives it.

Store whatever is unstored in `doc/` first (`orthant tag`), then check the release out, and
make the correction there:

```
orthant tag                                  # doc/ has nothing which is not in the store
orthant checkout en 0.76.0
$EDITOR .orthant/work/tree/soundness/modules/sockets.md
orthant checkin --dry-run                    # the points it would change
orthant checkin                              # 0.76.0, and every release after it
orthant export doc en main                   # bring doc/ up to the corrected English
```

`orthant edit soundness/modules/sockets.md en 0.76.0` does the first three of those for one
document, without a checkout.

The last step matters. The correction reaches `main` in the store, but `doc/` is never written
by Orthant, and still has the old lines: without the export, the next `orthant tag` would store
them again, and undo the correction at `main`. `git diff doc` then shows the correction as it
arrived.

Sometimes a correction is true of one release alone: a note about a defect which the next
release fixed, say. `--only release` keeps the edit to the release checked out, in every
language, and leaves later releases as they were:

```
orthant checkout en 0.76.0
$EDITOR .orthant/work/tree/soundness/modules/sockets.md
orthant checkin --only release               # en and fr at 0.76.0; not 0.77.0, not main
```

The French at that release goes stale where it translates the lines changed, as after any
English edit. The points of later releases are not touched.

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

A `-` line with no `+` line beneath it is left out of the translation. A line which reads the
same in both languages, such as a line of TEL structure, still wants a `+` line, as a pair of
its own:

```
-   definition
+   definition
-       A small sample of a color.
+       Un petit échantillon d'une couleur.
```

Written as one run of `-` lines and one of `+` lines, the shared first line would be taken as
unchanged, and left as a line yet to be translated. Then check in, which says how much less
there is to do:

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
say. The translation is marked stale all the same, because Orthant cannot know. After storing
the English, confirm the translation as it stands:

```
$EDITOR doc/soundness/modules/audio.md
orthant tag                   # leaves fr/main with 1 more stale passage
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
$EDITOR .orthant/work/tree/standards/examples.md   # "colours" becomes "colors"
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
(`orthant export out/doc en main`), and read neither `doc/` nor the working tree: `make docs`
refuses while `doc/` differs from the store.

## Keeping the store and the working tree in step

- **After a pull, a rebase or a branch switch** that changed `.orthant/`, run `orthant update`
  if a point is checked out. It brings the working tree up to the store as it now is, and
  carries the edits which are not checked in onto the new documents. `doc/` needs nothing: it
  and the store arrive together.
- **After a merge** in which two branches each stored `doc/`, the records at `main` may
  conflict as well as the documents. Resolve the documents in `doc/`, take either side's
  records, and run `orthant tag`, which makes the store agree with `doc/` as merged.
- **Before committing**, `orthant status --all` reports on every point, and `orthant check`
  verifies everything the store holds.
- **Commit** `doc/`, `.orthant/config.tel` and `.orthant/data/`, together. `.orthant/work/`,
  which holds the working tree and records what is checked out, is ignored by git. `doc/` and
  `.orthant/` are outside the CI input set, so a change to the documentation alone needs no new
  build to be attested.
- **To abandon edits** in `doc/`, restore the files with git; in the working tree, `orthant
  checkout <values> --force` discards whatever is not checked in.
- **After a release is tagged**, `orthant sync` takes the tag up as a value. Everything checked
  in at `main` becomes that release's, and `main` starts again beneath it. Commit the change to
  `.orthant/config.tel`.

`orthant <command> --help` describes each command.
