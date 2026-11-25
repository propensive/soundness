# How certain a detection is

A few opening bytes rarely prove what a stream holds, and a detection says in its type how
much was established. A leading brace is merely consistent with JSON, so a pattern which
demands more of the JSON detector does not compile.

```scala fuel
compile
frame
  label The PNG signature is identifying
  highlight Likely  at least the identifying structure
  code
      import detectables.{jsonDetectable, pngDetectable}

      def identify(content: Data): Text = content match
        case Media["image/png", Likely](png) => t"a PNG"
        case _                               => t"something else"
frame
  label An opening brace is not
  errors show
  code
      import detectables.{jsonDetectable, pngDetectable}

      def identify(content: Data): Text = content match
        case Media["image/png", Likely](png)         => t"a PNG"
        case Media["application/json", Likely](json) => t"JSON"
        case _                                       => t"something else"
frame
  label It is plausible, and the pattern says so
  highlight Plausible  all the JSON detector establishes
  code
      import detectables.{jsonDetectable, pngDetectable}

      def identify(content: Data): Text = content match
        case Media["image/png", Likely](png)            => t"a PNG"
        case Media["application/json", Plausible](json) => t"perhaps JSON"
        case _                                          => t"something else"
##
```

[How certain a detection is](../../soundness/modules/media-types.md#how-certain-a-detection-is)
defines the three levels.
