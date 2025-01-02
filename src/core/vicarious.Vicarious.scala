/*
    Vicarious, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package vicarious

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

object Vicarious:

  def proxy[KeyType: Type, ValueType: Type](matcher: Boolean)(using Quotes)
          : Expr[Proxy[KeyType, ValueType] | MatchProxy[KeyType]] =

    import quotes.reflect.*

    def recompose[LabelsType <: Tuple: Type, ElementsType <: Tuple: Type](result: TypeRepr)
            : TypeRepr =

      (Type.of[LabelsType]: @unchecked) match
        case '[ EmptyTuple ] =>
          result

        case '[ type headLabel <: String; type tailLabels <: Tuple; headLabel *: tailLabels ] =>
          (Type.of[ElementsType]: @unchecked) match
            case '[ type tailElement <: Tuple; headElement *: tailElements ] =>
              (TypeRepr.of[headLabel].asMatchable: @unchecked) match
                case ConstantType(StringConstant(label)) =>
                  recompose[tailLabels, tailElements](Refinement(result, label, proxy[headElement]))

    def proxy[KeyType2: Type]: TypeRepr = Expr.summon[Mirror.ProductOf[KeyType2]] match
      case Some('{ type labels <: Tuple
                   type types <: Tuple
                   $mirror: Mirror.Product
                             { type MirroredElemLabels = labels
                               type MirroredElemTypes = types } }) =>
        recompose[labels, types]:
          if matcher then  TypeRepr.of[MatchProxy[KeyType]]
          else TypeRepr.of[Proxy[KeyType, ValueType]]

      case _ =>
        if matcher then TypeRepr.of[MatchProxy[KeyType]] else TypeRepr.of[ValueType]

    (proxy[KeyType].asType: @unchecked) match
      case '[ type resultType <: Proxy[KeyType, ValueType] | MatchProxy[KeyType]; resultType ] =>
        '{Proxy().asInstanceOf[resultType]}
