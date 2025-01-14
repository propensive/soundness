/*
    Scintillate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package scintillate

import scala.annotation.*
import scala.quoted.*

import fulminate.*
import telekinesis.*

class servlet extends MacroAnnotation:
  override def transform(using Quotes)
     (tree: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition])
          : List[quotes.reflect.Definition] =
    import quotes.reflect.*

    tree match
      case defDef@DefDef(name, params, returnType, Some(body)) =>
        if !(returnType.tpe <:< TypeRepr.of[HttpResponse])
        then halt(m"the return type ${returnType.show} is not a subtype of HttpResponse[?]")
        val ref = Ref(defDef.symbol).etaExpand(tree.symbol.owner).asExprOf[HttpConnection => HttpResponse]
        val parents0 = List('{new JavaServletFn($ref)}.asTerm)
        val parents = List(TypeTree.of[HttpConnection])
        val newClassName = Symbol.freshName(name)
        val cls = Symbol.newClass(Symbol.spliceOwner, name, parents.map(_.tpe), _ => Nil, selfType = None)
        val clsDef = ClassDef(cls, parents, body = Nil)
        List(tree, clsDef)

      case other =>
        halt(m"the @servlet annotation must be applied to a method")
