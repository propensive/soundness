package escapade

import rudiments.*

type Escape = Ansi.Input.Esc

object Escape:
  def apply(code: String, reset: Maybe[String] = Unset): Escape =
    Ansi.Input.Esc(code, reset.otherwise(""))

object escapes:
  val BlackFg = Escape("[30m", "[39m")
  val RedFg = Escape("[31m", "[39m")
  val GreenFg = Escape("[32m", "[39m")
  val YellowFg = Escape("[33m", "[39m")
  val BlueFg = Escape("[34m", "[39m")
  val MagentaFg = Escape("[35m", "[39m")
  val CyanFg = Escape("[36m", "[39m")
  val WhiteFg = Escape("[37m", "[39m")
  
  val BlackBg = Escape("[40m", "[49m")
  val RedBg = Escape("[41m", "[49m")
  val GreenBg = Escape("[42m", "[49m")
  val YellowBg = Escape("[43m", "[49m")
  val BlueBg = Escape("[44m", "[49m")
  val MagentaBg = Escape("[45m", "[49m")
  val CyanBg = Escape("[46m", "[49m")
  val WhiteBg = Escape("[47m", "[49m")

  val BrightBlackFg = Escape("[90m", "[39m")
  val BrightRedFg = Escape("[91m", "[39m")
  val BrightGreenFg = Escape("[92m", "[39m")
  val BrightYellowFg = Escape("[93m", "[39m")
  val BrightBlueFg = Escape("[94m", "[39m")
  val BrightMagentaFg = Escape("[95m", "[39m")
  val BrightCyanFg = Escape("[96m", "[39m")
  val BrightWhiteFg = Escape("[97m", "[49m")
  
  val BrightBlackBg = Escape("[100m", "[49m")
  val BrightRedBg = Escape("[101m", "[49m")
  val BrightGreenBg = Escape("[102m", "[49m")
  val BrightYellowBg = Escape("[103m", "[49m")
  val BrightBlueBg = Escape("[104m", "[49m")
  val BrightMagentaBg = Escape("[105m", "[49m")
  val BrightCyanBg = Escape("[106m", "[49m")
  val BrightWhiteBg = Escape("[107m", "[49m")

  val Bold = Escape("[1m", "[22m")
  val Light = Escape("[2m", "[22m")
  val Italic = Escape("[3m", "[23m")
  val Underline = Escape("[4m", "[24m")
  val SlowBlink = Escape("[5m", "[25m")
  val FastBlink = Escape("[6m", "[25m")
  val Reverse = Escape("[7m", "[27m")
  val Conceal = Escape("[8m", "[28m")
  val Strike = Escape("[9m", "[29m")

  def title(name: String) = Escape(s"]0;$name${27.toChar}\\")