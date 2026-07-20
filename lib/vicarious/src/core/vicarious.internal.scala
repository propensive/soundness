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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package vicarious

import scala.collection.immutable.{List, Nil, ::}

import scala.compiletime.*
import scala.quoted.*

import denominative.*
import gigantism.*
import prepositional.*
import rudiments.*

object internal:
  def catalog[key: Type, value: Type]
    ( lambda:   Expr[[field] => (field: field) -> value],
      value:    Expr[key],
      classTag: Expr[ClassTag[value]] )
  :   Macro[Catalog[key, value]] =

    import quotes.reflect.*

    def fields[product: Type](term: Term): List[Term] =
      TypeRepr.of[product].typeSymbol.caseFields.flatMap: field =>
        term.select(field).asExpr.absolve match
          case '{$field: field} =>
            '{$lambda[field]($field)}.asTerm :: fields[field](field.asTerm)

    // The root value (`$lambda[key]($value)`) is prepended so the values align with the
    // root-inclusive `paths`/`fieldReprs` scheme: index 0 is the root, then each field depth-first.
    val rootValue = '{$lambda[key]($value)}.asExprOf[value]
    val values = rootValue :: fields[key](value.asTerm).map(_.asExprOf[value])

    ' {
        given classTag0: ClassTag[value] = $classTag
        Catalog(IArray(${Varargs(values)}*))
      }


  def fieldNames[product: Type](prefix: String)(using Quotes): List[String] =
    import quotes.reflect.*

    TypeRepr.of[product].typeSymbol.caseFields.flatMap: field =>
      val label = if prefix == "" then field.name else prefix+"."+field.name

      field.info.asType.absolve match
        case '[fieldType] => label :: fieldNames[fieldType](label)


  // All flattened paths of `product`, depth-first, with the empty root path "" at index 0. A
  // `Proxy`'s `id` indexes into this list, so navigation from the root (`id` 0, path "") resolves
  // each field correctly — distinguishing the root from the first field (which both collided at 0).
  def paths[product: Type](using Quotes): List[String] = "" :: fieldNames[product]("")


  // The flattened field types, aligned with `paths`: index 0 is the whole `product`, then each
  // field's type depth-first, matching the order `fieldNames` flattens labels.
  def fieldReprs[product: Type](using Quotes): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    def recur(repr: TypeRepr): List[TypeRepr] =
      repr.typeSymbol.caseFields.flatMap: field =>
        field.info :: recur(field.info)

    TypeRepr.of[product] :: recur(TypeRepr.of[product])


  def dereference[key: Type, value: Type, id <: Nat: Type](key: Expr[String])
  :   Macro[value | Proxy[key, value, Nat]] =

    import quotes.reflect.*

    val index = TypeRepr.of[id].asMatchable.absolve match
      case ConstantType(IntConstant(index)) => index

    val all = paths[key]
    val prefix = all(index)
    val name = key.valueOrAbort
    val label = if prefix == "" then name else prefix+"."+name
    val target = all.indexOf(label)

    if target < 0 then report.errorAndAbort("vicarious: "+label+" is not a valid field path")

    ConstantType(IntConstant(target)).asType.absolve match
      case '[type id <: Nat; id] => '{Proxy[key, value, id](${Expr(target)})}


  // Builds a `key is Specific over transport` from a partial function of field-path cases. Reads
  // each `case root.a.b() => instance` from the lambda's tree: the proxy in the pattern carries the
  // path's flattened index in its type, giving the dotted path and the field type to check the
  // instance (`transport { type Self = fieldType }`) against. The instances are paired with their
  // paths into the runtime `instances` map; the partial function itself is never run.
  def specific[key: Type, transport: Type]
    ( lambda: Expr[(Proxy[key, Any, 0] aka "root") ?=> Specific.Cases[key]] )
  :   Macro[key is Specific over transport] =

    import quotes.reflect.*

    val all = paths[key]
    val reprs = fieldReprs[key]

    // Collect every `case` of the partial function, wherever the literal's desugaring placed it.
    val caseDefs =
      val accumulator = new TreeAccumulator[List[CaseDef]]:
        def foldTree(state: List[CaseDef], tree: Tree)(owner: Symbol): List[CaseDef] = tree match
          case caseDef: CaseDef => foldOverTree(caseDef :: state, tree)(owner)
          case _                => foldOverTree(state, tree)(owner)

      accumulator.foldTree(Nil, lambda.asTerm)(Symbol.spliceOwner).reverse

    // The proxy whose `unapply` drives a pattern — drilling through the `unapply` selection to its
    // qualifier (`root.cto.joined`), whose type carries the path index.
    def proxyOf(tree: Tree): Term = tree.absolve match
      case Unapply(fun, _, _)   => proxyOf(fun)
      case Select(qualifier, _) => qualifier
      case Apply(fun, _)        => proxyOf(fun)
      case TypeApply(fun, _)    => proxyOf(fun)
      case Inlined(_, _, body)  => proxyOf(body)

    val pairs: List[Expr[(String, Any)]] = caseDefs.map: caseDef =>
      val index = proxyOf(caseDef.pattern).tpe.widen.dealias.asMatchable.absolve match
        case AppliedType(_, List(_, _, idType)) => idType.asMatchable.absolve match
          case ConstantType(IntConstant(index)) => index

      val fieldType = reprs(index)
      val required = Refinement(TypeRepr.of[transport], "Self", TypeBounds(fieldType, fieldType))
      val body = caseDef.rhs

      if !(body.tpe.widen <:< required) then
        report.errorAndAbort
          ( "vicarious: the instance for this path is not a "+required.show, body.pos )

      '{(${Expr(all(index))}, ${body.asExpr})}

    val instancesExpr = '{Map(${Varargs(pairs)}*)}

    val result =
      ' {
          new Specific:
            type Self = key
            type Transport = transport
            def instances: Map[String, Any] = $instancesExpr
        }

    result.asExprOf[key is Specific over transport]


  def proxy[key: Type, value: Type]: Macro[Proxy[key, value, 0]] =
    import quotes.reflect.*

    val fields = paths[key]

    def recur(prefix: String, repr: TypeRepr): TypeRepr =
      val index: Int = fields.indexOf(prefix)
      val nat = ConstantType(IntConstant(index))

      val base =
        TypeRepr.of[Proxy].appliedTo(List(TypeRepr.of[key], TypeRepr.of[value], nat))

      repr.typeSymbol.caseFields.fuse(base):
        val label = if prefix == "" then next.name else prefix+"."+next.name
        val fieldType: TypeRepr = next.info
        Refinement(state, next.name, recur(label, fieldType))

    recur("", TypeRepr.of[key]).asType.absolve match
      case '[type proxyType <: Proxy[key, value, 0]; proxyType] =>
        '{Proxy[key, value, 0](0).asInstanceOf[proxyType]}
