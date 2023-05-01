/*
    Serpentine, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import rudiments.*
import kaleidoscope.*

import scala.compiletime.*
import scala.quoted.*

import language.experimental.captureChecking

object SerpentineMacros:
  def parse
      [ForbiddenType <: Label: Type](ctx: Expr[StringContext])(using Quotes)
      : Expr[PathElement[ForbiddenType]] =
    import quotes.reflect.*
    
    val (element: String, pos: Position) = ctx match
      case '{StringContext(${Varargs(Seq(str))}*)} => (str.value.get, str.asTerm.pos)
      case _                                       => fail("A StringContext should contain literals")
    
    def checkType(repr: TypeRepr): Unit = repr.dealias.asMatchable match
      case OrType(left, right) =>
        checkType(left)
        checkType(right)
      
      case ConstantType(StringConstant(pattern)) =>
        if element.matches(pattern) then
          Text(pattern) match
            case r"\.\*\\?$char@(.)\.\*" =>
              fail(s"a path element may not contain the character '$char'", pos)
            case r"$start@([a-zA-Z0-9]*)\.\*" =>
              fail(s"a path element may not start with '$start'", pos)
            case r"[a-zA-Z0-9]*" =>
              fail(s"a path element may not be '$pattern'", pos)
            case other =>
              fail(s"a path element may not match the pattern '$other'")

      case other =>
        fail(s"Unexpectedly found type $other")
    
    checkType(TypeRepr.of[ForbiddenType])

    '{${Expr(element)}.asInstanceOf[PathElement[ForbiddenType]]}

extension (inline ctx: StringContext)
  inline def p[ForbiddenType <: Label](): PathElement[ForbiddenType] =
    ${SerpentineMacros.parse[ForbiddenType]('ctx)}