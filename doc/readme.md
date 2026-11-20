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

To change a document, edit it here and check it in. Fuel compiles only what is checked in, never
the working tree:

```
orthant checkin               # store the edits at the point checked out
fuel compile                  # check the documents, as the store now holds them
```

`orthant status` says what is not checked in. `orthant todo fr main` lists what has yet to be
translated, and the translations of text that has since changed. The scripts in `etc/` which read
the documents read a copy exported by `make docs` (`orthant export out/doc en main`), and do not read
this directory.

After a release is tagged, `orthant sync` takes the tag up as a value. Everything checked in at
`main` becomes that release's, and `main` starts again beneath it.
