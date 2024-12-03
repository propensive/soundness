package cosmopolite

import anticipation.*
import rudiments.*

transparent trait Language(val code: Text):
  type Code

  def apply[ValueType](value: Locale[Code] ?=> ValueType): Polyglot[ValueType, Code] =
    Polyglot(Map[Language, ValueType](this -> value(using Locale(this))))
