/*
    Serpentine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import fulminate.*
import kaleidoscope.*
import perforate.*
import anticipation.*
import gossamer.*

import scala.quoted.*
import scala.compiletime.*

object SerpentineMacro:
  def parse
      [NameType <: Label: Type](context: Expr[StringContext])(using Quotes)
      : Expr[PExtractor[NameType]] =
    import quotes.reflect.*
    
    val (element: String, pos: Position) = (context: @unchecked) match
      case '{StringContext(${Varargs(Seq(str))}*)} => (str.value.get, str.asTerm.pos)
    
    patterns(TypeRepr.of[NameType]).each: pattern =>
      if element.matches(pattern) then pattern match
        case r"\.\*\\?$char(.)\.\*" =>
          fail(msg"a path element may not contain the character $char", pos)

        case r"$start([a-zA-Z0-9]*)\.\*" =>
          fail(msg"a path element may not start with $start", pos)

        case r"\.\*$end([a-zA-Z0-9]*)" =>
          fail(msg"a path element may not end with $end", pos)

        case pattern@r"[a-zA-Z0-9]*" =>
          fail(msg"a path element may not be $pattern", pos)

        case other =>
          fail(msg"a path element may not match the pattern $other")

    '{
      new PExtractor[NameType]():
        def apply(): PathName[NameType] = ${Expr(element)}.asInstanceOf[PathName[NameType]]
        def unapply(name: PathName[NameType]): Boolean = name.render.s == ${Expr(element)}
    }
  
  private[serpentine] def patterns
      (using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
      : List[String] =
    import quotes.reflect.*
    
    (repr.dealias.asMatchable: @unchecked) match
      case OrType(left, right)                   => patterns(left) ++ patterns(right)
      case ConstantType(StringConstant(pattern)) => List(pattern)

  def runtimeParse
      [NameType <: Label: Type]
      (text: Expr[Text], errorHandler: Expr[Raises[PathError]])(using Quotes)
      : Expr[PathName[NameType]] =
    import quotes.reflect.*

    val checks: List[String] = patterns(TypeRepr.of[NameType])
    
    def recur(patterns: List[String], statements: Expr[Unit]): Expr[Unit] = patterns match
      case pattern :: tail =>
        import PathError.Reason.*
        
        def reasonExpr: Expr[PathError.Reason] = pattern match
          case r"\.\*\\?$char(.)\.\*"       => '{InvalidChar(${Expr(char.head)})}
          case r"$prefix([a-zA-Z0-9]*)\.\*" => '{InvalidPrefix(Text(${Expr(prefix.toString)}))}
          case r"\.\*$suffix([a-zA-Z0-9]*)" => '{InvalidSuffix(Text(${Expr(suffix.toString)}))}
          case other                        => '{InvalidName(Text(${Expr(pattern)}))}
        
        recur(tail, '{
          $statements

          if $text.s.matches(${Expr(pattern)})
          then raise(PathError($text, $reasonExpr))($text.asInstanceOf[PathName[NameType]])(using $errorHandler)
        })
      
      case _ =>
        statements

    '{
      ${recur(checks, '{()})}
      $text.asInstanceOf[PathName[NameType]]
    }
