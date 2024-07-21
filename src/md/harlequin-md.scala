package punctuation

import anticipation.*
import gossamer.*
import harlequin.*
import honeycomb.*

package htmlRenderers:
  given HtmlConverter as scalaSyntax:
    override def convertNode(node: Markdown.Ast.Block): Seq[Html[Flow]] = node match
      case Markdown.Ast.Block.FencedCode(t"amok", meta, value) =>
        val lines: List[Text] = value.cut(t"\n").to(List)
        val rewritten = lines.dropWhile(_ != t"##").tail.join(t"\n")
        convertNode(Markdown.Ast.Block.FencedCode(t"scala", meta, rewritten))

      case Markdown.Ast.Block.FencedCode(t"scala", meta, value) =>
        val code = ScalaSource.highlight(value).lines.flatMap: line =>
          (line :+ Token.Newline).map:
            case Token.Unparsed(text)     => Code(text)
            case Token.Markup(text)       => Code(text)
            case Token.Newline            => Code(Br)
            case Token.Code(text, accent) => accent match
              case Accent.Error             => Code.error(text)
              case Accent.Number            => Code.number(text)
              case Accent.String            => Code.string(text)
              case Accent.Ident             => Code.ident(text)
              case Accent.Term              => Code.term(text)
              case Accent.Typed             => Code.typed(text)
              case Accent.Keyword           => Code.keyword(text)
              case Accent.Symbol            => Code.symbol(text)
              case Accent.Parens            => Code.parens(text)
              case Accent.Modifier          => Code.modifier(text)

        Seq(Pre(code.init*))

      case other =>
        super.convertNode(other)
