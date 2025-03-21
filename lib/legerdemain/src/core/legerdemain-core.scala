package legerdemain

import anticipation.*
import distillate.*
import fulminate.*
import gossamer.*
import honeycomb.*
import prepositional.*
import vacuous.*

import html5.*

given Realm = realm"legerdemain"

trait Widget:
  def name: Text
  def value: Text
  def entered: Boolean = value != t""

object Field:
  given Field is Renderable into Html[?] = field =>
    List(Input.Text(name = field.name, value = field.value))

case class Field(name: Text, value: Text) extends Widget
case class Autocomplete(name: Text, options: List[Text], value: Text) extends Widget
case class RadioGroup(name: Text, options: List[(Text, Text)], value: Text) extends Widget

object Checkbox:
  given Checkbox is Renderable into Html[?] = checkbox =>
    List(Input.Checkbox(name = checkbox.name, checked = (checkbox.value != t"")))

case class Checkbox(name: Text, value: Text) extends Widget
case class Select(name: Text, options: List[(Text, Text)], value: Text) extends Widget

object Elicitable:
  given Boolean is Elicitable:
    type Operand = Checkbox
    def input(value: Boolean): Text = if value then t"on" else t""
    def output(value: Text): Boolean = value != t""

    def widget(id: Text, label: Text, value: Text, message: Optional[Text]): Widget =
      Field(id, value)

  given [ValueType: {Decodable in Text, Encodable in Text}] => ValueType is Elicitable:
    type Operand = Field
    def input(value: ValueType): Text = value.encode
    def output(value: Text): ValueType = value.decode

    def widget(id: Text, label: Text, value: Text, message: Optional[Text]): Widget =
      Field(id, value)


trait Elicitable:
  type Self
  type Operand
  def input(value: Self): Text
  def output(value: Text): Self
  def widget(id: Text, label: Text, value: Text, message: Optional[Text]): Widget
