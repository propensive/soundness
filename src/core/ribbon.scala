package escapade

import rudiments.*
import iridescence.*
import gossamer.*

case class Ribbon(colors: Color*):
  def fill(parts: AnsiText*): AnsiText =
    import escapes.*
    IArray.from(colors.zip(parts)).curse:
      val (background, text) = cursor
      val foreground = background.standardSrgb.highContrast
      val arrow = postcursor.fm(ansi"$Reset$background()") { (col, _) => ansi"$background(${Bg(col)}())" }
      
      ansi"${Bg(background)}( $foreground($text) )$arrow"
    .join
    