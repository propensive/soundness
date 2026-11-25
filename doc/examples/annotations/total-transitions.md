# A field, reached by a name which may be wrong

A `Dereferenceable` reaches the fields of a case class by name at runtime, through a table
which is built as the code compiles. A name is only text, and may not be a field, so an update
by name gives an `Optional`.

```scala fuel
compile
frame
  label The fields of a record, by name
  highlight Letters..Int]  a table built by the compiler
    line 4
  code
      case class Letters(alpha: Int, beta: Int, gamma: Int)

      val letters = Letters(1, 2, 3)
      val fields = summon[Letters is Dereferenceable to Int]
frame
  label An update by name may find no field
  highlight Optional[Letters]  Unset if no field has the name
  code
      case class Letters(alpha: Int, beta: Int, gamma: Int)

      val letters = Letters(1, 2, 3)
      val fields = summon[Letters is Dereferenceable to Int]

      val updated: Optional[Letters] = fields.update(letters, t"beta", 20)
      val missing: Optional[Letters] = fields.update(letters, t"epsilon", 5)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(updated.inspect)
Out.println(missing.inspect)
```

[Reaching fields by name](../../soundness/modules/annotations.md#reaching-fields-by-name) lists
the members, and [total transitions](../../philosophy/total-transitions.md) explains why the
result is optional.
