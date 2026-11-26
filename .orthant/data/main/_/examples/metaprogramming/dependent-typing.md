# A value carried in a type

A list of names can be written as a type: a tuple of singleton types, under `TypeList`. It
stays in the type system, where a macro or a typeclass can inspect it, and `reify` turns it
into the runtime value at the moment one is needed.

```scala fuel
compile
frame
  label One literal, as a type
  code
      val eldest = reify["Goneril"]
frame
  label A list of them
  highlight TypeList  a tuple, read as a list
  code
      val eldest = reify["Goneril"]
      val daughters = reify[TypeList[("Goneril", "Regan", "Cordelia")]]
frame
  label A set of them
  highlight TypeSet  a union, read as a set
  code
      val eldest = reify["Goneril"]
      val daughters = reify[TypeList[("Goneril", "Regan", "Cordelia")]]
      val answers = reify[TypeSet["yes" | "no" | "nothing"]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(eldest.inspect)
Out.println(daughters.inspect)
Out.println(answers.inspect)
```

[Type-level values](../../soundness/modules/metaprogramming.md#type-level-values) lists the
type-level form of each kind of value.
