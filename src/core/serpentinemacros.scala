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

import scala.compiletime.*
import scala.quoted.*

import language.experimental.captureChecking

object SerpentineMacros:
  def parse
      [ForbiddenType <: ForbiddenSet: Type](ctx: Expr[StringContext])(using Quotes)
      : Expr[PathElement[ForbiddenType]] =
    import quotes.reflect.*
    
    val (element: String, pos: Position) = ctx match
      case '{StringContext(${Varargs(Seq(str))}*)} => (str.value.get, str.asTerm.pos)
      case _                                       => fail("A StringContext should contain literals")
    
    def checkType(repr: TypeRepr): Unit = repr.dealias.asMatchable match
      case OrType(left, right) =>
        checkType(left)
        checkType(right)
      
      case ConstantType(StringConstant(str)) =>
        if element == str then fail(s"'$str' is not a valid name for a path element", pos)

      case ConstantType(CharConstant(char)) =>
        element.indexOf(char) match
          case -1  => ()
          case idx =>
            val pos2 = Position(pos.sourceFile, pos.start + idx, pos.start + idx + 1)
            fail(s"the character '$char' is not permitted in a path element", pos2)
    
      case other =>
        fail(s"Unexpectedly found type $other")
    
    checkType(TypeRepr.of[ForbiddenType])

    '{${Expr(element)}.asInstanceOf[PathElement[ForbiddenType]]}

extension (inline ctx: StringContext)
  inline def p[ForbiddenType <: ForbiddenSet](): PathElement[ForbiddenType] =
    ${SerpentineMacros.parse[ForbiddenType]('ctx)}