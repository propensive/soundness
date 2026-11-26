# Similar words, paired

A plain diff reports a changed word as one deletion and one insertion. `rdiff` pairs them
where a predicate says that they are similar, and the predicate here is the edit distance of
the text library, which knows nothing of diffs.

```scala fuel
compile
frame
  label Deletions and insertions
  code
      import Diff.{Del, Ins, Par, Sub}

      val quarto = Sequence(t"to", t"be", t"or", t"not", t"to", t"be")
      val folio = Sequence(t"to", t"bee", t"or", t"not", t"to", t"bee")

      val revision = diff(quarto, folio)
frame
  label Substitutions, where two words are close
  highlight _.proximity(_)..2  the edit distance between two words
  code
      import Diff.{Del, Ins, Par, Sub}
      import proximities.levenshteinProximity, caseSensitivity.caseSensitive

      val quarto = Sequence(t"to", t"be", t"or", t"not", t"to", t"be")
      val folio = Sequence(t"to", t"bee", t"or", t"not", t"to", t"bee")

      val revision = diff(quarto, folio).rdiff(_.proximity(_) < 2)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

revision.changes.each:
  case Sub(_, _, left, right) => Out.println(t"${left.inspect} became ${right.inspect}")
  case _                      => ()
```

[Aligned differences](../../soundness/modules/diffing.md#aligned-differences) describes the
pairing, and [composability](../../philosophy/composability.md) why a predicate is all that
joins the two libraries.
