# An attribute decides the type of its value

A JAR manifest is a list of names and text, but its standard attributes mean more than text.
Reading an attribute from a `Manifest` gives a value whose type depends on which attribute
was asked for.

```scala fuel
compile
frame
  label A manifest
  code
      import manifestAttributes.*
      import strategies.throwUnsafely

      val manifest = Manifest(MainClass(fqcn"globe.Prompter"), CreatedBy(t"Soundness"))
frame
  label Its main class is a class name
  highlight Optional[Fqcn]  the type of `Main-Class`
  code
      import manifestAttributes.*
      import strategies.throwUnsafely

      val manifest = Manifest(MainClass(fqcn"globe.Prompter"), CreatedBy(t"Soundness"))
      val main: Optional[Fqcn] = manifest(MainClass)
frame
  label Its creator is text
  highlight Optional[Text]  the type of `Created-By`
  code
      import manifestAttributes.*
      import strategies.throwUnsafely

      val manifest = Manifest(MainClass(fqcn"globe.Prompter"), CreatedBy(t"Soundness"))
      val main: Optional[Fqcn] = manifest(MainClass)
      val tool: Optional[Text] = manifest(CreatedBy)
##
```

[Manifests](../../soundness/modules/versioning.md#manifests) describes the attributes.
