/*
    Harlequin, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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
