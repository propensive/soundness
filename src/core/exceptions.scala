package xylophone

import rudiments.*

abstract class XylophoneError(msg: String) extends Exception(str"xylophone: $msg")

case class XmlParseError(line: Int, column: Int)
extends XylophoneError(str"the XML source could not be parsed at line $line, column $column")

case class XmlReadError() extends XylophoneError("could not read value")

case class XmlAccessError(index: Int, path: XmlPath)
extends XylophoneError(str"could not access ${if index == 0 then "any nodes" else
    str"node $index"} at path ${Xml.pathString(path)}")
