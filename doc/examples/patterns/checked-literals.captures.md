# A capture which could not bind

A pattern's captures become variables, so the compiler checks more than its brackets. A
capture inside a repeated group would match many times and bind once, and a repetition
written backwards can never match: each is an error where the pattern is written.

```scala fuel
compile
frame
  label A capture inside a repetition
  errors show
  code
      def words(line: Text): Boolean = line match
        case r"($word([a-z]+) )+" => true
        case _                    => false
frame
  label A repetition from five to two
  errors show
  code
      def words(line: Text): Boolean = line match
        case r"([a-z]+ ){5,2}" => true
        case _                 => false
frame
  label A pattern which means something
  highlight {2,5}  from two to five
  code
      def words(line: Text): Boolean = line match
        case r"([a-z]+ ){2,5}" => true
        case _                 => false
##
```

[Compiletime checking](../../soundness/modules/patterns.md#compiletime-checking) lists every
reason a pattern is rejected.
