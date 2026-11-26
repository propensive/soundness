# A case which may not exist

An enumeration is looked up by the name of a case, or by its position, through an instance
which the compiler derives. Neither a name nor a position is sure to correspond to a case, so
each lookup gives an `Optional`, and the code says what a miss means.

```scala fuel
compile
frame
  label The derived instance
  highlight Direction..Enumerable  nothing is written for it
    line 4
  code
      enum Direction:
        case North, South, East, West

      val directions = summon[Direction is Enumerable]
frame
  label A lookup by name, and one by position
  highlight Optional[Direction]  a name may match no case
    line 6
  code
      enum Direction:
        case North, South, East, West

      val directions = summon[Direction is Enumerable]

      val upward: Optional[Direction] = directions.value(t"Up")
      val first: Optional[Direction] = directions.value(Prim)
frame
  label A default for a miss
  highlight or(Direction.North)  what a miss means here
  code
      enum Direction:
        case North, South, East, West

      val directions = summon[Direction is Enumerable]

      val upward: Direction = directions.value(t"Up").or(Direction.North)
      val first: Optional[Direction] = directions.value(Prim)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(upward.inspect)
Out.println(first.inspect)
```

[Enumerations by name and position](../../soundness/modules/decoding.md#enumerations-by-name-and-position)
lists the lookups, and [optionality](../../philosophy/optionality.md) explains `or`.
