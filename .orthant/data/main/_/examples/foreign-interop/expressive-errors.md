# A wrong argument, reported in the foreign language's terms

A call to a foreign method is checked against the foreign declaration. `greet` is declared in
TypeScript as taking a `string`, so the error for a number names the method and the TypeScript
type it expects, and not a Scala type.

```scala fuel
compile
frame
  label A number, where TypeScript declares a string
  errors show
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val greeting = play.greet(42)
frame
  label Text is a string
  highlight t"Viola"  a Scala Text, passed as a TypeScript string
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val greeting = play.greet(t"Viola")
##
```

[Navigating foreign types](../../soundness/modules/foreign-interop.md#navigating-foreign-types)
says how Scala values map onto foreign types, and
[expressive errors](../../philosophy/expressive-errors.md) why the message is worded so.
