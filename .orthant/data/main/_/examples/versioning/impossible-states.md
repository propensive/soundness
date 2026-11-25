# A main class is a class name

The `Main-Class` of a JAR manifest must name a class, and a manifest which names something
else is found out when the JAR is run. Here the attribute takes an `Fqcn`, which is a class
name already checked, and text is not one.

```scala fuel
compile
frame
  label Text
  errors show
  code
      import manifestAttributes.*

      val manifest = Manifest(MainClass(t"globe.Prompter"))
frame
  label Not a class name
  errors show
  code
      import manifestAttributes.*

      val manifest = Manifest(MainClass(fqcn"globe.the prompter"))
frame
  label A class name
  highlight fqcn"globe.Prompter"  checked as the code compiles
  code
      import manifestAttributes.*

      val manifest = Manifest(MainClass(fqcn"globe.Prompter"))
##
```

[Manifests](../../soundness/modules/versioning.md#manifests) has the other attributes, and
[impossible states](../../philosophy/impossible-states.md) the principle.
