# A system property, written as a path of members

A system property has a dotted name, and is read by writing the same dots: `os.name` is
`System.properties.os.name()`. Each is decoded to the type it is known to hold, so the
separator of a path is a `Char`, and not text of length one.

```scala fuel
compile
frame
  label A property
  highlight os.name()  the property os.name
  code
      import systems.javaBaseSystem, strategies.throwUnsafely

      def system(): Text = System.properties.os.name()
frame
  label One which is a character
  highlight Char  decoded from the text
  code
      import systems.javaBaseSystem, strategies.throwUnsafely

      def system(): Text = System.properties.os.name()
      def separator(): Char = System.properties.file.separator()
##
```

The example is compiled, and is not run, since its results depend on the machine.
[System properties](../../soundness/modules/environment.md#system-properties) compares
properties with variables.
