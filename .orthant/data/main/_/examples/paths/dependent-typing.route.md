# The route between two paths, found by the compiler

For two paths whose names are known, `toward` finds the relative path from one to the other
as the code compiles, and `conjunction` their closest common ancestor. Both are plain values,
and not optional ones, since the compiler has seen that the paths share a root.

```scala fuel
compile
frame
  label Two paths
  code
      val capulet = % / "verona" / "capulet" / "orchard"
      val montague = % / "verona" / "montague"
frame
  label From one to the other
  highlight toward  computed from the two types
  code
      val capulet = % / "verona" / "capulet" / "orchard"
      val montague = % / "verona" / "montague"

      val route = capulet.toward(montague)
      val shared = capulet.conjunction(montague)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(route.inspect)
Out.println(shared.inspect)
```

[Between two paths](../../soundness/modules/paths.md#between-two-paths) has both operations.
