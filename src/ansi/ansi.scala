package harlequin

import escapade.*
import iridescence.*
import spectacular.*
import vacuous.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform

package syntaxHighlighting:
  import Accent.*

  given displayableToken: Displayable[Token] =
    case Token.Unparsed(text)       => text.display
    case Token.Markup(text)         => text.display
    case Token.Newline              => e"\n"
    case Token.Code(text, Error)    => e"${rgb"#cc0033"}($text)"
    case Token.Code(text, Number)   => e"${rgb"#cc3366"}($text)"
    case Token.Code(text, Modifier) => e"${rgb"#ff9966"}($text)"
    case Token.Code(text, Keyword)  => e"${rgb"#ff6633"}($text)"
    case Token.Code(text, Ident)    => e"${rgb"#ffcc99"}($text)"
    case Token.Code(text, Term)     => e"${rgb"#ffcc33"}($text)"
    case Token.Code(text, Type)     => e"${rgb"#00cc99"}($text)"
    case Token.Code(text, String)   => e"${rgb"#99ffff"}($text)"
    case Token.Code(text, Parens)   => e"${rgb"#cc6699"}($text)"
    case Token.Code(text, Symbol)   => e"${rgb"#cc3366"}($text)"

  given numbered: Displayable[ScalaSource] = source =>
    val indent = source.lastLine.show.length
    
    val markup = source.focus.lay(e""):
      case ((startLine, startColumn), (endLine, endColumn)) =>
        if startLine != endLine then e"" else
          if startColumn == endColumn then e"\n${t" "*(startColumn + indent + 1)}${rgb"#ff0033"}(╱╲)"
          else e"\n${t" "*(startColumn + indent + 2)}${rgb"#ff0033"}(${t"‾"*(endColumn - startColumn)})"
      
    (source.offset to source.lastLine).map: lineNo =>
      val content = source(lineNo).map(_.display).join
      e"${Bg(rgb"#003333")}(${rgb"#99cc99"}(${lineNo.show.pad(indent, Rtl)})${rgb"#336666"}(┋)) $content"

    .join(e"", e"\n", markup)
  
  given unnumbered: Displayable[ScalaSource] = source =>
    (source.offset to source.lastLine).map: lineNo =>
      source(lineNo).map(_.display).join
    .join(e"\n")
    
    