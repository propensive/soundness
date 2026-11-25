# A TypeScript declaration file, read by the Scala compiler

An `Interface` names a file of foreign declarations, which is read as the code compiles. The
members of a `Foreign` value are then the members which the file declares: here an interface
`Play`, with a `scene` and a method `greet`, and nothing called `interval`.

```scala fuel
compile
frame
  label The declarations, and a value of a type they declare
  highlight "/xenophile/definitions.ts"  a resource on the compiler's classpath
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]
frame
  label A member which the file declares
  highlight play.scene  typed as the file says: a Scene
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val scene = play.scene
frame
  label A method, applied
  highlight play.greet(t"Viola")  takes a string, and returns one
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val scene = play.scene
      val greeting = play.greet(t"Viola")
frame
  label A member which it does not
  errors show
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val interval = play.interval
##
```

[Navigating foreign types](../../soundness/modules/foreign-interop.md#navigating-foreign-types)
describes what each step checks.
