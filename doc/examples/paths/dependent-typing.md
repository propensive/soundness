# A path whose names are in its type

A path which is built from literals carries each name in its type, innermost first. A type
which names one house is not satisfied by a path to another.

```scala fuel
compile
frame
  label The wrong house
  errors show
  code
      val house: Path of ("capulet", "verona") = % / "verona" / "montague"
frame
  label The right one
  highlight "capulet"  matches the first name in the type
    occurrence 2
  code
      val house: Path of ("capulet", "verona") = % / "verona" / "capulet"
frame
  label A step down adds a name to the type
  highlight "juliet"  the new name comes first
  code
      val house: Path of ("capulet", "verona") = % / "verona" / "capulet"
      val room: Path of ("juliet", "capulet", "verona") = house / "juliet"
##
```

[Paths in the type](../../soundness/modules/paths.md#paths-in-the-type) says what the
compiler does with a path it knows.
