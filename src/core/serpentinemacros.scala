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
import gossamer.*
import kaleidoscope.*

import scala.compiletime.*
import scala.quoted.*

object SerpentineMacros:
  def runtimeParse
      [ForbiddenType <: Label: Type]
      (text: Expr[Text])(using Quotes)
      : Expr[PathName[ForbiddenType]] =
    import quotes.reflect.*

    val checks = patterns(TypeRepr.of[ForbiddenType])
    
    def recur
        (patterns: List[Text], expr: Expr[PathName[ForbiddenType]])
        : Expr[PathName[ForbiddenType]] =
      patterns match
        case pattern :: tail =>
          import PathError.Reason.*
          def reasonExpr: Expr[PathError.Reason] = pattern match
            case r"\.\*\\?$char(.)\.\*"       => '{InvalidChar(${Expr(char.head)})}
            case r"$prefix([a-zA-Z0-9]*)\.\*" => '{InvalidPrefix(${Expr(prefix)})}
            case r"\.\*$suffix([a-zA-Z0-9]*)" => '{InvalidSuffix(${Expr(suffix)})}
            case other                        => '{InvalidName(${Expr(pattern)})}
          
          recur(tail, '{
            given CanThrow[PathError] = unsafeExceptions.canThrowAny
            if $text.s.matches(${Expr(pattern)}.s) then throw PathError($reasonExpr) else $expr
          })
        
        case _ =>
          expr

    recur(checks, '{$text.asInstanceOf[PathName[ForbiddenType]]})

  private def patterns(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): List[Text] =
    import quotes.reflect.*
    
    (repr.dealias.asMatchable: @unchecked) match
      case OrType(left, right)                   => patterns(left) ++ patterns(right)
      case ConstantType(StringConstant(pattern)) => List(Text(pattern))
  
  def parse
      [ForbiddenType <: Label: Type](context: Expr[StringContext])(using Quotes)
      : Expr[PathName[ForbiddenType]] =
    import quotes.reflect.*
    
    val (element: String, pos: Position) = context match
      case '{StringContext(${Varargs(Seq(str))}*)} => (str.value.get, str.asTerm.pos)
      case _                                       => fail("A StringContext should contain literals")
    
    patterns(TypeRepr.of[ForbiddenType]).foreach: pattern =>
      if element.matches(pattern.s) then pattern match
        case r"\.\*\\?$char(.)\.\*" =>
          fail(s"a path element may not contain the character '$char'", pos)

        case r"$start([a-zA-Z0-9]*)\.\*" =>
          fail(s"a path element may not start with '$start'", pos)

        case r"\.\*$end([a-zA-Z0-9]*)" =>
          fail(s"a path element may not end with '$end'", pos)

        case pattern@r"[a-zA-Z0-9]*" =>
          fail(s"a path element may not be '$pattern'", pos)

        case other =>
          fail(s"a path element may not match the pattern '$other'")

    '{${Expr(element)}.asInstanceOf[PathName[ForbiddenType]]}
