package litterateur

import honeycomb._, html5._

import scala.collection.immutable.ListMap

object Html {

  def outline(node: MdNode): Seq[honeycomb.Node[Flow]] = convert(Markdown.parse(headOutline(node).mkString("\n")))

  def slug(str: String): String =
    str.toLowerCase.replaceAll("[^a-z0-9]", "-").replaceAll("--*", "-")

  private def headOutline(node: MdNode): Seq[String] = node match {
    case Document(children@_*) => children.flatMap(headOutline(_))
    case Heading(level, children@_*) =>
      val text = inlineText(children)
      List(s"${" "*(2*level - 1)}- [${text}](#${slug(text)})")
    case Text(str) => List(str)
    case _ => Nil
  }
  
  def convert(node: MdNode): Seq[honeycomb.Node[Flow]] = node match {
    case Document(children@_*) => children.flatMap(convert(_))
    case Paragraph(children@_*) => List(p(children.flatMap(convertInline(_))))
    case Heading(level, children@_*) =>
      val tag = level match {
        case 1 => h1
        case 2 => h2
        case 3 => h3
        case 4 => h4
        case 5 => h5
        case 6 => h6
      }
      List(tag(id = slug(inlineText(children)))(children.flatMap(convertInline(_))))
    case Blockquote(children@_*) => List(blockquote(children.flatMap(convertInline(_))))
    case ThematicBreak => List(hr())
    case CodeBlock(syntax, Text(str)) => List(pre(code(escape(str))))
    case Reference(_, _) => Nil
    case Text(str) => List(p(honeycomb.Text(escape(str))))
    case BulletList(children@_*) => List(ul(children.flatMap(convertListItem(_))))
    case OrderedList(children@_*) => List(ol(children.flatMap(convertListItem(_))))
    case Table(parts@_*) => List(table(parts.flatMap(convertTableParts(_))))
  }

  def convertTableParts(node: TablePart) = node match {
    case TableHead(rows@_*) => List(thead(convertTableRows(true, rows)))
    case TableBody(rows@_*) => List(tbody(convertTableRows(false, rows)))
  }

  def convertTableRows(heading: Boolean, rows: Seq[Row]) = rows map {
    case Row(cells@_*) => tr(convertTableCells(heading, cells))
  }

  def convertTableCells(heading: Boolean, cells: Seq[Cell]) = cells map {
    case Cell(content@_*) =>
      val cellContent = content.flatMap(convertInline(_))
      if(heading) th(cellContent) else td(cellContent)
  }

  def convertListItem(node: MdNode): Seq[honeycomb.Node[ListType]] = node match {
    case ListItem(children@_*) => List(li(children.flatMap(convert(_))))
    case _ => Nil
  }

  def inlineText(node: Seq[MdNode]): String = node.map {
    case Document(_*) | BulletList(_*) | Indented(_) | OrderedList(_*) | ListItem(_*) => ""
    case Image(txt, _) => txt
    case Link(txt, _) => txt
    case Reference(_, _) => ""
    case LineBreak => ""
    case ThematicBreak => ""
    case Emphasis(children@_*) => inlineText(children)
    case StrongEmphasis(children@_*) => inlineText(children)
    case Code(children@_*) => inlineText(children)
    case Paragraph(children@_*) => inlineText(children)
    case Heading(_, children@_*) => inlineText(children)
    case Blockquote(children@_*) => inlineText(children)
    case CodeBlock(_, children@_*) => inlineText(children)
    case Text(str) => str
    case Cell(content@_*) => inlineText(content)
  }.mkString

  def convertInline(node: MdNode): Seq[honeycomb.Node[Phrasing]] = node match {
    case Document(_*) | BulletList(_*) | Indented(_) | OrderedList(_*) | ListItem(_*) => Nil
    case Image(txt, loc) => List(img(src = loc, alt = txt))
    case Link(txt, loc) => List(a(href = loc)(txt))
    case Reference(_, _) => Nil
    case LineBreak => List(br)
    case ThematicBreak => Nil
    case Emphasis(children@_*) => List(em(children.flatMap(convertInline(_))))
    case StrongEmphasis(children@_*) => List(strong(children.flatMap(convertInline(_))))
    case Code(children@_*) => List(code(children.flatMap(convertInline(_))))
    case Paragraph(children@_*) => children.flatMap(convertInline(_))
    case Heading(_, children@_*) => children.flatMap(convertInline(_))
    case Blockquote(children@_*) => children.flatMap(convertInline(_))
    case CodeBlock(_, children@_*) => children.flatMap(convertInline(_))
    case Text(str) => List(honeycomb.Text(escape(str)))
  }

  def escape(str: String): String = str.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")

}
