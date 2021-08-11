package xylophone

trait XmlPrinter[T]:
  def print(doc: Xml): T

object XmlPrinter:
  given XmlPrinter[String] = StandardXmlPrinter(false)

object printers:
  given compact: XmlPrinter[String] = StandardXmlPrinter(true)

class StandardXmlPrinter(compact: Boolean = false) extends XmlPrinter[String]:
  def print(doc: Xml): String =
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var pos: Int = 0

    def newline(n: Int = 0): Unit =
      if !compact then
        indent += n
        linebreak = true

    def append(strings: String*): Unit =
      for str <- strings do
        buf.append(str)
        pos += str.length

    def whitespace(): Unit =
      if !compact && linebreak then
        buf.append("\n")
        for i <- 1 to indent do buf.append("  ")
        pos = indent*2
      linebreak = false

    def inline(element: Ast.Element): Boolean = element.children.forall {
      case Ast.Text(_) => true
      case _       => false
    }

    def next(node: Ast): Unit = node match
      case element@Ast.Element(tagName, children, attributes, namespaces) =>
        whitespace()
        append("<", tagName.toString)

        for attribute <- attributes do attribute match
          case (key, value) => append(" ", key.toString, "=\"", value, "\"")
        
        if element.children.isEmpty then append("/")
        append(">")
        if !inline(element) then newline(1)

        for child <- element.children do
          val splitLine = child match
            case Ast.Text(_) => false
            case _           => true
          if splitLine then newline()
          next(child)
          if splitLine then newline()

        if !inline(element) then newline(-1)

        whitespace()
        if !element.children.isEmpty then
          append("</", tagName.toString, ">")
          if !inline(element) then newline(0)

      case Ast.Text(text) =>
        whitespace()
        append(text)

      case Ast.ProcessingInstruction(target, content) =>
        whitespace()
        append("<?", target, " ", content, "?>")
        newline()

      case Ast.Comment(content) =>
        whitespace()
        append("<!--", content, "-->")
        newline()

      case e => ()

    doc.root.content.foreach(next(_))

    buf.toString