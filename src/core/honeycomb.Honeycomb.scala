/*
    Honeycomb, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

import rudiments.*
import vacuous.*
import fulminate.*
import prepositional.*
import anticipation.*

import scala.quoted.*

object Honeycomb:
  given Realm = realm"honeycomb"

  def read[NameType <: Label: Type, ChildType <: Label: Type, ReturnType <: Label: Type]
     (node:       Expr[Node[NameType]],
      className:  Expr[String],
      name:       Expr[NameType],
      attributes: Expr[Seq[(Label, Any)]])
     (using Quotes)
          : Expr[StartTag[NameType, ReturnType]] =

    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]])
            : List[Expr[Optional[(String, Optional[Text])]]] =
      exprs match
        case '{type keyType <: Label; ($key: keyType, $value: valueType)} +: tail =>
          val attribute: String = key.value.get

          val expr: Expr[keyType is HtmlAttribute[valueType]] =
            Expr.summon[keyType is HtmlAttribute[valueType] onto NameType]
             .orElse(Expr.summon[keyType is HtmlAttribute[valueType]])
             .getOrElse:
              val typeName = TypeRepr.of[valueType].show
              abandon(m"""the attribute $attribute cannot take a value of type $typeName""")

          '{  $expr.convert($value) match
                case HtmlAttribute.NotShown => Unset
                case Unset                  => ($expr.rename.or($key.tt).s, Unset)
                case attribute: Text        => ($expr.rename.or($key.tt).s, attribute)
           } :: recur(tail)

        case _ =>
          if className.value == Some("apply") then Nil else List('{("class", $className.tt)})

    (attributes: @unchecked) match
      case Varargs(exprs) =>
        '{
            StartTag
             ($name,
              $node.unclosed,
              $node.block,
              $node.verbatim,
              $node.attributes ++ ${Expr.ofSeq(recur(exprs))}.compact.collect:
                case (key, value: Text) => (key, value)
                case (key, Unset)       => (key, Unset))  }
