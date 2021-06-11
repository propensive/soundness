package honeycomb

trait HtmlSerializer[T]:
  def serialize(doc: HtmlDoc, maxWidth: Int = -1): T

object HtmlSerializer:
  given HtmlSerializer[String] = (doc, maxWidth) =>
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var emptyLine: Boolean = true
    var pos: Int = 0
    
    def newline(n: Int = 0): Unit =
      indent += n
      linebreak = true

    def append(strings: String*): Unit =
      for str <- strings do
        buf.append(str)
        pos += str.length
      emptyLine = false

    def whitespace(): Unit =
      if linebreak then
        buf.append("\n")
        for i <- 1 to indent do buf.append("  ")
        pos = indent*2
      linebreak = false
      emptyLine = true

    def next(node: Content[?], verbatim: Boolean): Unit = node match
      case node: Item[?] =>
        whitespace()
        append("<", node.label)
        for attribute <- node.attributes do attribute match
          case (key, value: String) => append(" ", key, "=\"", value, "\"")
          case (key, true)          => append(" ", key)
          case (key, false)         => ()
        append(">")
        if !node.inline then newline(1)
        for child <- node.children do
          val splitLine = child match
            case node: Node[?] => !node.inline
            case _             => false
          if splitLine then newline()
          next(child, node.verbatim)
          if splitLine then newline()
        if !node.inline then newline(-1)
        if !node.unclosed then
          whitespace()
          append("</", node.label, ">")
          if !node.inline then newline(0)

      case text: String =>
        whitespace()
        if maxWidth == -1 then append(text) else
          if verbatim || pos + text.length <= maxWidth then append(text)
          else
            text.split("\\s+").foreach { word =>
              if !(pos + 1 + word.length < maxWidth || emptyLine) then
                linebreak = true
                whitespace()
                append(" ")
              append(if !emptyLine then " " else "", word)
            }
            if text.last.isWhitespace then append(" ")
    
    append("<!DOCTYPE html>\n")
    next(doc.root, false)

    buf.toString