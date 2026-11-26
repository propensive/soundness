# Each step of a navigation has the type the declaration gives it

The type of a foreign value carries the name of its foreign type, as a string in the Scala
type. Selecting a member computes the next type from the declarations, so a chain of selections
is typed all the way along, and a wrong expectation of its end is a type error.

```scala fuel
compile
frame
  label A Play has a scene, which is a Scene
  highlight "Scene"  computed from scene: Scene
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val scene: Foreign of "Scene" from Typescript = play.scene
frame
  label And a Scene has a play
  highlight play.scene.play  two steps, each resolved in the file
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val scene: Foreign of "Scene" from Typescript = play.scene
      val again: Foreign of "Play" from Typescript = play.scene.play
frame
  label A Scene's count is declared as a number
  errors show
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

      val count: Foreign of "string" from Typescript = play.scene.count
##
```

[Navigating foreign types](../../soundness/modules/foreign-interop.md#navigating-foreign-types)
has the declarations these types come from.
