package xylophone

import contextual.*
import rudiments.*

case class PartialXml(text: String)
case class XmlInput(string: String)

object XmlInterpolator extends Interpolator[XmlInput, PartialXml, Xml]:
  def complete(state: PartialXml): Xml exposes ParseError = ???
  def initial: PartialXml = ???
  def insert(state: PartialXml, value: Option[XmlInput]): PartialXml exposes ParseError = ???
  def parse(state: PartialXml, next: String): PartialXml = ???
