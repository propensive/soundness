package spectacular

import rudiments.*

class Match1[ValueType, Param1Type](val value: ValueType) extends AnyVal, Product:
  def _1: Param1Type = ???
  def canEqual(that: Any): Boolean = true
  def productArity: Int = 1
  def productElement(n: Int): Any = _1

trait Parser[+ValueType]:
  def unapply(text: Text): Match1[Text, Char] = new Match1[Text, Char](text)

object Parser

