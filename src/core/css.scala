package cataract

import rudiments.*

import language.dynamics

case class Stylesheet(rules: Rule*)
  

case class Style(properties: CssProperty*):
  override def toString(): String = properties.map(_.toString).join("\n")

case class Rule(selector: Selector, style: Style):
  override def toString(): String =
    val rules = style.properties.map(_.toString).join("; ")
    str"${selector.value} { $rules }"

case class CssProperty(key: String, value: String):
  override def toString(): String = str"$key: $value"

object Testing:
  val css = Css(border = "hello", paddingLeft = "10em")

@main
def run(): Unit =
  println(Testing.css)