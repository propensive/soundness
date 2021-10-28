/*
    Honeycomb, version 0.9.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import scala.quoted.*

object Macro:
  def read[Name <: Label: Type, Children <: Label: Type, Return <: Label: Type]
          (name: Expr[Name], unclosed: Expr[Boolean], inline: Expr[Boolean],
               verbatim: Expr[Boolean], attributes: Expr[Seq[(Label, Any)]])
          (using Quotes)
          : Expr[Element[Name, Return]] =
    import quotes.reflect.{Singleton as _, *}

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[(String, Maybe[Txt])]] =
      exprs match
        case '{($key: k & Label, $value: v)} +: tail =>
          val att = key.valueOrAbort
          val exp: Expr[Attribute[k & Label, v, Name]] =
            Expr.summon[Attribute[k & Label, v, Name]].getOrElse {
              val typeName = TypeRepr.of[v].show
              report.errorAndAbort(txt"""honeycomb: the attribute $att cannot take a value of type
                                         $typeName""".s)
            }
          
          '{($exp.rename.getOrElse(Txt($key)).s, $exp.convert($value))} :: recur(tail)
        
        case _ =>
          Nil

    attributes match
      case Varargs(exprs) =>
        '{Element($name, $unclosed, $inline, $verbatim, ${Expr.ofSeq(recur(exprs))}.to(Map))}
      
      case _ =>
        throw Impossible("expected varargs")
