package xylophone

import rudiments.*

enum Ast:
  case Element(name: XmlName, children: Seq[Ast], attributes: Map[XmlName, String] = Map(),
                   namespaces: List[Namespace] = Nil)
  case Comment(content: String)
  case ProcessingInstruction(target: String, content: String)
  case Text(content: String)
  case CData(content: String)
  case Root(content: Ast*)

  override def toString(): String = this match
    case Element(name, children, attributes, _) =>
      val inside = children.map(_.toString).join
      val attributeString = attributes.map { (k, v) => str"${k.toString}=$v" }.join(" ", " ", "")
      
      str"<${name.toString}${attributeString}>$inside</${name.toString}>"

    case Comment(content) =>
      str"<!--$content-->"

    case ProcessingInstruction(target, content) =>
      str"<?$target $content?>"

    case Text(content) =>
      content

    case CData(content) =>
      str"<![CDATA[${content.toString}]]>"

    case Root(content*) =>
      str"""<?xml version = "1.0"?>${content.map(_.toString).join}"""
