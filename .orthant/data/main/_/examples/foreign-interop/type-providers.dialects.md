# One way to read three languages' declarations

A TypeScript declaration file, a C header and a WebAssembly WIT description are each the
authority on what exists in their world. Each is an `Interface` in its own `Ecosystem`, and the
calls checked against them are written the same way.

```scala fuel
compile
alternative TypeScript
  highlight Typescript  an interface Play, with greet(name: string): string
  code
      given (Interface in Typescript at "/xenophile/definitions.ts") =
        Interface[Typescript](cp"/xenophile/definitions.ts")

      val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]
      val greeting: Foreign of "string" from Typescript = play.greet(t"Viola")
alternative A C header
  highlight Native  int abs(int n);
  code
      given (Interface in Native at "/xenophile/library.h") =
        Interface[Native](cp"/xenophile/library.h")

      val library: Foreign of "library" from Native = Foreign["library", Native]
      val distance: Foreign of "int" from Native = library.abs(-5)
alternative WIT
  highlight Wit  greet: func(name: string) -> string;
  code
      given (Interface in Wit at "/xenophile/api.wit") =
        Interface[Wit](cp"/xenophile/api.wit")

      val api: Foreign of "api" from Wit = Foreign["api", Wit]
      val greeting: Foreign of "string" from Wit = api.greet(t"Viola")
##
```

[Loading an interface](../../soundness/modules/foreign-interop.md#loading-an-interface) lists
the dialects which can be read.
