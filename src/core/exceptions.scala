package xylophone

import rudiments.*
import gossamer.*

abstract class XylophoneError(msg: String) extends Exception(str"xylophone: $msg")

case class XmlParseError(line: Int, column: Int)
extends XylophoneError(str"the XML source could not be parsed at line $line, column $column")

case class XmlReadError() extends XylophoneError("could not read value")

case class XmlAccessError(index: Int, path: XmlPath)
extends XylophoneError({
  val ref: String = if index == 0 then "any nodes" else str"node $index"
  str"could not access $ref at path ${Xml.pathString(path)}"
})
