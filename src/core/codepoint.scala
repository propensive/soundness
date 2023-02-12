package deviation

import rudiments.*

import scala.quoted.*

import language.experimental.captureChecking

object Codepoint:
  inline given Codepoint = ${DeviationMacros.location}

case class Codepoint(source: Text, line: Int):
  def text: Text = Text(s"${source.s.split("/").nn.last.nn}:$line")

object DeviationMacros:
  def location(using Quotes): Expr[Codepoint] =
    import quotes.*, reflect.*
    val path = Expr(Position.ofMacroExpansion.sourceFile.path)
    val line = Expr(Position.ofMacroExpansion.startLine + 1)

    '{Codepoint(Text($path), $line)}