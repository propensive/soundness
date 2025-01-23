/*
    Cataclysm, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cataclysm

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*

import scala.quoted.*

object Cataclysm:
  def rule(selector: Expr[Selector], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[CssRule] =
    '{CssRule($selector, ${read(props)})}

  def keyframe(name: Expr[String], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Keyframe] =
    '{Keyframe(Text($name), ${read(props)})}

  def read(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[CssStyle] =
    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[CssProperty]] = exprs match
      case '{type keyType <: Label; ($key: keyType, $value: valueType)} +: tail =>
        val exp: Expr[keyType is PropertyDef[valueType]] =
          Expr.summon[keyType is PropertyDef[valueType]].getOrElse:
            val typeName = TypeRepr.of[valueType].show
            halt(m"no valid CSS element ${key.valueOrAbort} taking values of type $typeName exists")

        '{CssProperty(Text($key).uncamel.kebab, compiletime.summonInline[ShowProperty[valueType]].show($value))} :: recur(tail)

      case _ =>
        Nil

    properties.absolve match
      case Varargs(exprs) => '{CssStyle(${Expr.ofSeq(recur(exprs))}*)}
