/*
    Spectacular, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import scala.quoted.*

import anticipation.*

object Spectacular:
  def enumerable[EnumType <: reflect.Enum: Type](using Quotes): Expr[EnumType is Enumerable] =
    import quotes.reflect.*

    val companion = Ref(TypeRepr.of[EnumType].typeSymbol.companionModule).asExpr

    '{
        new Enumerable:
          type Self = EnumType
          val name: Text = ${Expr(TypeRepr.of[EnumType].show)}.tt
          val values: IArray[EnumType] =
            ${  (companion: @unchecked) match
                  case '{ $companion: companionType } =>
                    val ref = TypeRepr.of[companionType].typeSymbol.declaredMethod("values").head
                    companion.asTerm.select(ref).asExprOf[Array[EnumType]]
            }.asInstanceOf[IArray[EnumType]]
    }
