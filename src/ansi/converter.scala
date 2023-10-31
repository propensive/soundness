/*
    Punctuation, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*
import escapade.*, escapes.*, rendering.output
import iridescence.*
import harlequin.*
import spectacular.*
import hieroglyph.*, textWidthCalculation.eastAsianScripts

case class BodyText(blocks: TextBlock*):
  def serialize(width: Int): Output = blocks.map(_.render(width)).join(e"\n\n")

case class TextBlock(indent: Int, text: Output):
  @targetName("add")
  def +(txt: Output): TextBlock = TextBlock(indent, text+txt)
  
  def render(width: Int): Output =
    def rest(text: Output, lines: List[Output]): List[Output] =
      if text.length == 0 then lines.reverse
      else
        try
          val pt = text.plain.where(_ == ' ', width - indent*2, Rtl)
          rest(text.drop(pt + 1), text.take(pt) :: lines)
        catch case err: OutOfRangeError =>
          rest(text.drop(width - indent*2), text.take(width - indent*2) :: lines)
    
    rest(text, Nil).map((e"  "*indent)+_).join(e"\n")

open class TextConverter():
  private def heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Seq[Markdown.Ast.Inline]): TextBlock =
    val content = e"${children.map(phrasing).join}"
    
    TextBlock(0, level match
      case 1 => e"$Bold($Underline(${content.upper}))"
      case 2 => e"$Bold($Underline($content))"
      case 3 => e"$Bold($content)"
      case 4 => e"$Underline($content)"
      case 5 => e"$Underline($content)"
      case 6 => e"$Underline($content)"
    )

  def blockify(nodes: Seq[Markdown.Ast.Node]): Seq[Markdown.Ast.Block] =
    nodes.foldLeft((true, List[Markdown.Ast.Block]())):
      case ((fresh, acc), next) => next match
        case node: Markdown.Ast.Block  =>
          (true, node :: acc)
        
        case node: Markdown.Ast.Inline =>
          if fresh then (false, Markdown.Ast.Block.Paragraph(node) :: acc)
          else
            val content = acc.head match
              case Markdown.Ast.Block.Paragraph(nodes*) =>
                Markdown.Ast.Block.Paragraph((nodes :+ node)*) :: acc.tail
              
              case _ =>
                throw Mistake("unexpected non-paragraph node found while folding inline nodes")
            (false, content)
    .apply(1).reverse

  def convert(nodes: Seq[Markdown.Ast.Node], indent: Int): BodyText =
    BodyText(blockify(nodes).foldLeft(List[TextBlock]()) {
      case (acc, next) => next match
        case Markdown.Ast.Block.Paragraph(children*) =>
          acc :+ TextBlock(indent, children.map(phrasing).join)
        
        case Markdown.Ast.Block.Heading(level, children*) =>
          acc :+ heading(level, children)
        
        case Markdown.Ast.Block.Blockquote(children*) =>
          acc ++ convert(children, indent + 1).blocks
        
        case Markdown.Ast.Block.ThematicBreak() =>
          acc :+ TextBlock(indent, e"---")
        
        case Markdown.Ast.Block.FencedCode(syntax, meta, value) =>
          if syntax == Some(t"scala") then
            val highlightedLines = ScalaSyntax.highlight(value).map:
              line =>
                line.map:
                  case Token.Code(code, flair) => flair match
                    case Accent.Type              => e"${solarized.Blue}(${code.trim})"
                    case Accent.Term              => e"${solarized.Green}(${code.trim})"
                    case Accent.Symbol            => e"${solarized.Red}(${code.trim})"
                    case Accent.Keyword           => e"${solarized.Orange}(${code.trim})"
                    case Accent.Modifier          => e"${solarized.Yellow}(${code.trim})"
                    case Accent.Ident             => e"${solarized.Cyan}(${code.trim})"
                    case Accent.Error             => e"${solarized.Red}($Underline(${code.trim}))"
                    case Accent.Number            => e"${solarized.Violet}(${code.trim})"
                    case Accent.String            => e"${solarized.Violet}(${code.trim})"
                    case other                   => e"${code.trim}"
                  case Token.Unparsed(content)   => e"${content}"
                  case _                       => throw Mistake("Should not have a newline")
                .join
            
            acc :+ TextBlock(indent, highlightedLines.join(e"\n"))
          else acc :+ TextBlock(indent, e"${foreground.BrightGreen}($value)")
        
        case Markdown.Ast.Block.BulletList(num, loose, _, items*) =>
          acc :+ TextBlock(indent, items.zipWithIndex.map { case (item, idx) =>
            e"${num.fold(t"  » ") { n => t"${(n + idx).show.fit(3)}. " }}${item.toString.show}"
          }.join(e"\n"))
    
        case Markdown.Ast.Block.Table(parts*) =>
          acc :+ TextBlock(indent, e"[table]")
        
        case other =>
          acc
    }*)

  // def tableParts(node: Markdown.Ast.TablePart): Seq[Item["thead" | "tbody"]] = node match
  //   case Markdown.Ast.TablePart.Head(rows*) => List(Thead(tableRows(true, rows)))
  //   case Markdown.Ast.TablePart.Body(rows*) => List(Tbody(tableRows(false, rows)))

  // def tableRows(heading: Boolean, rows: Seq[Markdown.Ast.Block.Row]): Seq[Item["tr"]] =
  //   rows.map { case Markdown.Ast.Block.Row(cells*) => Tr(tableCells(heading, cells)) }

  // def tableCells(heading: Boolean, cells: Seq[Markdown.Ast.Block.Cell]): Seq[Item["th" | "td"]] =
  //   cells.map { case Markdown.Ast.Block.Cell(content*) =>
  //     (if heading then Th else Td)(content.map(phrasing))
  //   }

  // def listItem(node: Markdown.Ast.ListItem): Seq[Item["li"]] = node match
  //   case Markdown.Ast.ListItem(children*) => List(Li(convert(children)*))

  def text(node: Seq[Markdown.Ast.Node]): Output = node.map:
    case Markdown.Ast.Inline.Image(text, _)         => e"[ $text ]"
    case Markdown.Ast.Inline.Weblink(s, desc)       => e"${colors.DeepSkyBlue}($Underline(${text(Seq(desc))})${colors.DarkGray}([)${colors.RoyalBlue}($Underline($s))${colors.DarkGray}(])) "
    case Markdown.Ast.Inline.Break()                => e""
    case Markdown.Ast.Inline.Emphasis(children*)    => e"$Italic(${text(children)})"
    case Markdown.Ast.Inline.Strong(children*)      => e"$Bold(${text(children)})"
    case Markdown.Ast.Inline.SourceCode(code)       => e"${colors.YellowGreen}(${Bg(Srgb(0, 0.1, 0))}($code))"
    case Markdown.Ast.Inline.Copy(text)             => e"$text"
    case Markdown.Ast.Block.BulletList(_, _, _, _*) => e""
    case Markdown.Ast.Block.Reference(_, _)         => e""
    case Markdown.Ast.Block.ThematicBreak()         => e""
    case Markdown.Ast.Block.Paragraph(children*)    => text(children)
    case Markdown.Ast.Block.Heading(_, children*)   => text(children)
    case Markdown.Ast.Block.Blockquote(children*)   => text(children)
    case Markdown.Ast.Block.FencedCode(_, _, code)  => e"${colors.YellowGreen}($code)"
    case Markdown.Ast.Block.Cell(content*)          => text(content)
    case _                                          => e""
  .join

  def phrasing(node: Markdown.Ast.Inline): Output = node match
    case Markdown.Ast.Inline.Image(altText, location) => e"[ $altText ]"
    case Markdown.Ast.Inline.Break()                  => e"\n"
    case Markdown.Ast.Inline.Emphasis(children*)      => e"$Italic(${children.map(phrasing).join})"
    case Markdown.Ast.Inline.Strong(children*)        => e"$Bold(${children.map(phrasing).join})"
    case Markdown.Ast.Inline.SourceCode(code)         => e"${colors.YellowGreen}(${Bg(Srgb(0, 0.1, 0))}($code))"
    case Markdown.Ast.Inline.Copy(str)                => e"${str.sub(t"\n", t" ")}"
    case _                                            => text(Seq(node))
