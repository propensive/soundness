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
┃    Soundness, version 0.45.0.                                                                    ┃
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
package honeycomb

import anticipation.*
import fulminate.*
import prepositional.*
import proscenium.*
import vacuous.*

import scala.quoted.*

object Honeycomb:
  private given realm: Realm = realm"honeycomb"


  def read[name <: Label: Type, child <: Label: Type, result <: Label: Type]
       (node:       Expr[Node[name]],
        className:  Expr[String],
        name:       Expr[name],
        attributes: Expr[Seq[(Label, Any)]])
  : Macro[StartTag[name, result]] =

      import quotes.reflect.*

      def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[Optional[(String, Optional[Text])]]] =
        exprs match
          case '{("", $value: valueType)} +: tail =>
            val expr: Expr[HtmlAttribute[? >: valueType]] =
              Expr.summon[HtmlAttribute[? >: valueType] onto name]
              . orElse(Expr.summon[HtmlAttribute[? >: valueType]])
              . getOrElse:
                  val typeName = TypeRepr.of[valueType]
                  halt(m"""the attribute name cannot be uniquely determined from its type,
                           ${typeName.show}""")

            val key: Text = expr.absolve match
              case '{ type keyType <: Label
                      $attribute: (HtmlAttribute[valueType] { type Self = keyType}) } =>
                TypeRepr.of[keyType].absolve match
                  case ConstantType(StringConstant(key)) => key.tt


            '{  $expr.convert($value) match
                  case HtmlAttribute.NotShown => Unset
                  case Unset                  => ($expr.rename.or(${Expr(key)}).s, Unset)
                  case attribute: Text        => ($expr.rename.or(${Expr(key)}).s, attribute)
            } :: recur(tail)

          case '{type keyType <: Label; ($key: keyType, $value: valueType)} +: tail =>
            val attribute: String = key.value.get

            val expr: Expr[keyType is HtmlAttribute[valueType]] =
              Expr.summon[keyType is HtmlAttribute[valueType] onto name]
              . orElse(Expr.summon[keyType is HtmlAttribute[valueType]])
              . getOrElse:
                  val typeName = TypeRepr.of[valueType]
                  halt(m"the attribute $attribute cannot take a value of type ${typeName.show}")

            '{  $expr.convert($value) match
                  case HtmlAttribute.NotShown => Unset
                  case Unset                  => ($expr.rename.or($key.tt).s, Unset)
                  case attribute: Text        => ($expr.rename.or($key.tt).s, attribute)
            } :: recur(tail)

          case _ =>
            if className.value == Some("apply") then Nil else List('{("class", $className.tt)})

      attributes.absolve match
        case Varargs(exprs) =>
          '{
              StartTag
               ($name,
                $node.attributes ++ ${Expr.ofSeq(recur(exprs))}.compact.collect:
                  case (key, value: Text) => (key, value)
                  case (key, Unset)       => (key, Unset))  }
