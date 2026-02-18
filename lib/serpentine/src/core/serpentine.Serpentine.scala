                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package serpentine

import scala.quoted.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import stenography.*
import vacuous.*

object Serpentine:
  def path(context: Expr[StringContext]): Macro[Path] =
    val name: String = context.valueOrAbort.parts.head
    safely(name.tt.decode[Path on Posix]).let: path =>
      '{Path.of[Posix, %.type, Tuple](${Expr(path.root)}, ${Varargs(path.descent.map(Expr(_)))}*)}
    . or:
        safely(name.tt.decode[Path on Windows]).let: path =>
          val varargs = Varargs(path.descent.map(Expr(_)))
          '{Path.of[Windows, Drive, Tuple](${Expr(path.root)}, $varargs*)}

        . or(halt(m"The path ${name} is not a valid Windows or POSIX path"))

  private def plane[path <: Path: Type](using Quotes): Optional[quotes.reflect.Symbol] =
    import quotes.reflect.*

    Type.of[path] match
      case '[type topic; Path { type Plane = plane }] =>
        if !TypeRepr.of[plane].typeSymbol.isAbstractType then TypeRepr.of[plane].typeSymbol
        else Unset

      case _ =>
        Unset

  private def limit[path <: Path: Type](using Quotes): Optional[quotes.reflect.Symbol] =
    import quotes.reflect.*

    Type.of[path].match
      case '[type limit; Path { type Limit = limit }] =>
        val typeRepr = TypeRepr.of[limit]
        if typeRepr.isSingleton then typeRepr.typeSymbol else Unset

      case _ =>
        Unset

  private def topic[path <: Path: Type](using Quotes)
  :   Optional[Either[List[String], List[String]]] =
    import quotes.reflect.*

    def decompose[path: Type](done: List[String] = Nil)
    :   Optional[Either[List[String], List[String]]] =
      Type.of[path] match
        case '[type head; type tail <: Tuple; head *: tail] =>
          TypeRepr.of[head].absolve match
            case ConstantType(StringConstant(element)) =>
              decompose[tail](element :: done)

        case '[EmptyTuple] =>
          Right(done.reverse)

        case _ =>
          if done.nil then Unset else Left(done.reverse)

    Type.of[path] match
      case '[type topic <: Tuple; Path { type Topic = topic }] => decompose[topic]()
      case _                                                   => Unset


  private def sameRoot[left <: Path: Type, right <: Path: Type](using Quotes): Optional[Boolean] =
    import quotes.reflect.*

    limit[left].present && limit[left] == limit[right] || plane[left] == plane[right] &&
      {
        try plane[left].let(_.typeMember("UniqueRoot").typeRef.dealias).lay(false):
          case ConstantType(BooleanConstant(boolean)) => boolean
          case _                                      => false
        catch
          case _: Throwable => false
      }

  def toward[subject <: Path: Type, target <: Path: Type](subject: Expr[subject], target: Expr[target])
  :   Macro[Relative] =

    import quotes.reflect.*

    conjunction[target, subject](target, subject).absolve match
      case '{type base <: Path; $base: base} =>
        plane[target].let(_.typeRef.asType).or(Type.of[Any]).absolve match
          case '[plane] =>
            topic[subject].let:
              case Right(subjectDescent) =>
                topic[base].let:
                  case Right(baseDescent) =>
                    val ascent: Int = subjectDescent.length - baseDescent.length
                    ConstantType(IntConstant(ascent)).asType.absolve match
                      case '[type limit <: Int; limit] =>
                        topic[target].let:
                          case Right(targetDescent) =>
                            val descent = targetDescent.dropRight(baseDescent.length)
                            tuple(descent).asType.absolve match
                              case '[type tuple <: Tuple; tuple] =>
                                val varargs = Varargs(descent.map(Expr[Text](_)))
                                '{Relative[plane, tuple, limit](${Expr(ascent)}, $varargs*)}
                          case Left(_) => Unset
                  case Left(_) => Unset
              case Left(_) => Unset
            . or:
                val depth = '{$target.depth - $base.depth}
                val ascent = '{$subject.depth - $base.depth}
                '{Relative[plane, Tuple, Nat]($ascent, $target.descent.take($depth)*)}


  def conjunction[left <: Path: Type, right <: Path: Type](left: Expr[left], right: Expr[right])
  :   Macro[Path] =

    import quotes.reflect.*

    if !sameRoot[left, right].or(false) then '{$left.calculate($right)} else
      topic[left] match
        case Right(leftDescent) => topic[right] match
          case Right(rightDescent) =>
            calculate(leftDescent, rightDescent).asType.absolve match
              case '[type tuple <: Tuple; tuple] =>
                '{$left.calculate($right).asInstanceOf[Path of tuple]}

          case _ =>
            '{$left.calculate($right)}

        case _ =>
          '{$left.calculate($right)}

  private def tuple(elements: List[String])(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    elements.map: string =>
      ConstantType(StringConstant(string)).asType
    . foldLeft(Type.of[EmptyTuple]: Type[? <: Tuple]): (tuple, element) =>
        tuple.absolve match
          case '[type tuple <: Tuple; tuple] => element.absolve match
            case '[element] => Type.of[element *: tuple]
    . absolve
    . match
        case '[type tuple <: Tuple; tuple] => TypeRepr.of[tuple]

  private def calculate(left: List[String], right: List[String])(using Quotes)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val difference = left.length - right.length
    val left0 = left.drop(difference).to(List)
    val right0 = right.drop(-difference).to(List)

    def recur(left: List[String], right: List[String], size: Int, count: Int): List[String] =
      if left.nil then left0.drop(size - count)
      else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
      else recur(left.tail, right.tail, size + 1, 0)

    tuple(recur(left0, right0, 0, 0).reverse)
