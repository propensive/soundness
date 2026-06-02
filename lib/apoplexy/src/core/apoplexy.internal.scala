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
package apoplexy

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import hellenism.*
import jacinta.*
import rudiments.*
import spectacular.*
import strategies.throwUnsafely
import telekinesis.*
import turbulence.*
import vacuous.*

object Apoplexy:
  // --- compile-time spec access -------------------------------------------

  private val specs: scala.collection.mutable.HashMap[Text, OpenApi] =
    scala.collection.mutable.HashMap()

  private def spec(using Quotes)(source: Text): OpenApi =
    specs.synchronized:
      specs.at(source).or:
        val stream = Optional(getClass.getResourceAsStream(source.s)).or:
          halt(m"apoplexy: could not read the OpenAPI spec at $source on the classpath")

        val content = scala.io.Source.fromInputStream(stream).mkString.tt

        val doc =
          try content.read[OpenApi]
          catch case error: Exception => halt(m"apoplexy: the OpenAPI spec at $source is not valid")

        specs(source) = doc
        doc

  // --- refinement-type helpers (mirroring xenophile) ----------------------

  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

  private def stringOf(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): Text =
    import quotes.reflect.*

    repr.absolve match
      case ConstantType(StringConstant(value)) => value.tt
      case _                                   => halt(m"apoplexy: expected a string literal type")

  private def literalType(using quotes: Quotes)(value: Text): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    ConstantType(StringConstant(value.s))

  private def apiType(using quotes: Quotes)(locus: Text, source: Text): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    val withLocus =
      Refinement(TypeRepr.of[Api], "Locus", TypeBounds(literalType(locus), literalType(locus)))

    Refinement(withLocus, "Source", TypeBounds(literalType(source), literalType(source)))

  private def receiver(using quotes: Quotes)(self: Expr[Api]): (Text, Text) =
    import quotes.reflect.*

    val members = refinements(self.asTerm.tpe.widen)
    val locus = members.at(t"Locus").lay(t"/")(stringOf(_))
    val source = members.at(t"Source").or(halt(m"apoplexy: the receiver has no spec `Source`"))

    (locus, stringOf(source))

  // --- path utilities ------------------------------------------------------

  private def segments(path: Text): List[Text] = path.cut(t"/").filter(_ != t"").to(List)
  private def isTemplate(segment: Text): Boolean = segment.starts(t"{") && segment.s.endsWith("}")
  private def templateName(segment: Text): Text = segment.skip(1).keep(segment.length - 2)

  private def isPrefix(short: List[Text], long: List[Text]): Boolean =
    short.length <= long.length && short.zip(long).forall(_ == _)

  private def join(locus: Text, segment: Text): Text =
    if locus == t"/" then t"/$segment" else t"$locus/$segment"

  // --- schema → Scala type -------------------------------------------------

  private def schemaType(using quotes: Quotes)(doc: OpenApi, schema: JsonSchema)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    schema match
      case _: JsonSchema.Integer => TypeRepr.of[Int]
      case _: JsonSchema.Number  => TypeRepr.of[Double]
      case _: JsonSchema.String  => TypeRepr.of[Text]
      case _: JsonSchema.Boolean => TypeRepr.of[Boolean]

      case array: JsonSchema.Array =>
        array.items.lay(TypeRepr.of[Json])(schemaType(doc, _)).asType.absolve match
          case '[element] => TypeRepr.of[List[element]]

      case _ =>
        TypeRepr.of[Json]

  private def pathParamType(using quotes: Quotes)(doc: OpenApi, path: Text, parameter: Text)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val params =
      doc.paths.at(path).lay(List[OpenApi.Parameter]()): item =>
        item.operations.values.flatMap(_.parameters).to(List)

    params.find { p => p.name == parameter && p.`in` == OpenApi.Parameter.In.Path } match
      case Some(param) => param.schema.lay(TypeRepr.of[Text])(schemaType(doc, _))
      case None        => TypeRepr.of[Text]

  // --- macros --------------------------------------------------------------

  def root(resource: Expr[Resource]): Macro[Api] =
    import quotes.reflect.*

    val members = refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)

    val source =
      members.at(t"Locus").lay(halt(m"apoplexy: the resource has no `Locus` path"))(stringOf(_))

    val doc = spec(source)
    val base = if doc.servers.isEmpty then t"" else doc.servers.head.url
    val baseExpr = Expr(base.s)

    apiType(t"/", source).asType.absolve match
      case '[type result <: Api; result] =>
        '{Api.make(Api.Request(Http.Get, $baseExpr.tt, t"/")).asInstanceOf[result]}

  def select(self: Expr[Api], field: Expr[String]): Macro[Api] =
    val name = field.valueOrAbort.tt
    val (locus, source) = receiver(self)
    val doc = spec(source)
    val keys = doc.paths.keys.map(segments).to(List)
    val newLocus = join(locus, name)
    val newSegs = segments(newLocus)

    if !keys.exists(isPrefix(newSegs, _)) then halt(m"apoplexy: no path begins with $newLocus")

    val locusExpr = Expr(newLocus.s)

    apiType(newLocus, source).asType.absolve match
      case '[type result <: Api; result] =>
        '{Api.make($self.request.copy(path = $locusExpr.tt)).asInstanceOf[result]}

  def applied(self: Expr[Api], field: Expr[String], args: Expr[Seq[Any]]): Macro[Api] =
    import quotes.reflect.*

    val name = field.valueOrAbort.tt
    val (locus, source) = receiver(self)
    val doc = spec(source)
    val keys = doc.paths.keys.map(segments).to(List)
    val newLocus = join(locus, name)
    val newSegs = segments(newLocus)

    if !keys.exists(isPrefix(newSegs, _)) then halt(m"apoplexy: no path begins with $newLocus")

    val children = keys.filter: key =>
      isPrefix(newSegs, key) && key.length > newSegs.length

    children.map(_(newSegs.length)).find(isTemplate) match
      case None =>
        halt(m"apoplexy: invocation is not implemented yet")

      case Some(template) =>
        val parameter = templateName(template)
        val templatedLocus = join(newLocus, template)

        val arg = args match
          case Varargs(Seq(only)) => only
          case Varargs(_)         => halt(m"apoplexy: path parameter $parameter needs one argument")
          case _                  => halt(m"apoplexy: the path argument must be passed directly")

        val expected = pathParamType(doc, templatedLocus, parameter)
        val actual = arg.asTerm.tpe.widen

        if !(actual <:< expected)
        then halt(m"apoplexy: path parameter $parameter expects ${expected.show}")

        val locusExpr = Expr(templatedLocus.s)
        val paramExpr = Expr(parameter.s)

        apiType(templatedLocus, source).asType.absolve match
          case '[type result <: Api; result] => actual.asType.absolve match
            case '[argType] =>
              val valueExpr = arg.asExprOf[argType]

              val showable = Expr.summon[argType is Showable].getOrElse:
                halt(m"apoplexy: the path parameter $parameter cannot be rendered as text")

              ' {
                  val rendered = $showable.text($valueExpr)
                  val updated = $self.request.substitutions.updated($paramExpr.tt, rendered)

                  Api.make($self.request.copy(path = $locusExpr.tt, substitutions = updated))
                  . asInstanceOf[result]
                }

  def appliedNamed(self: Expr[Api], field: Expr[String], args: Expr[Seq[(String, Any)]])
  :   Macro[Api.Response] =

    halt(m"apoplexy: invocation is not implemented yet")

  def decode[value: Type](self: Expr[Api.Response]): Macro[value] =
    halt(m"apoplexy: .as is not implemented yet")
