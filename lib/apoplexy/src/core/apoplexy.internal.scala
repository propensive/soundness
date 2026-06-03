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
import hieroglyph.*
import jacinta.*
import prepositional.*
import rudiments.*
import denominative.*
import spectacular.*
import telekinesis.*
import turbulence.*
import vacuous.*
import xylophone.*

import charEncoders.utf8
import strategies.throwUnsafely

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
      case AndType(left, right)                        => refinements(left) ++ refinements(right).iterator
      case _                                           => Map()

  private def stringOf(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): Text =
    import quotes.reflect.*

    repr.absolve match
      case ConstantType(StringConstant(value)) => value.tt
      case _                                   => halt(m"apoplexy: expected a string literal type")

  private def literalType(using quotes: Quotes)(value: Text): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    ConstantType(StringConstant(value.s))

  private def bounds(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   quotes.reflect.TypeBounds =

    import quotes.reflect.*
    TypeBounds(repr, repr)

  private def apiType(using quotes: Quotes)(locus: Text, source: Text, wire: Wire)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val withLocus = Refinement(TypeRepr.of[Api], "Locus", bounds(literalType(locus)))
    val withSource = Refinement(withLocus, "Source", bounds(literalType(source)))
    Refinement(withSource, "Transport", bounds(transportRepr(wire)))

  private def receiver(using quotes: Quotes)(self: Expr[Api]): (Text, Text, Wire) =
    import quotes.reflect.*

    val members = refinements(self.asTerm.tpe.widen)
    val locus = members.at(t"Locus").lay(t"/")(stringOf(_))
    val source = members.at(t"Source").or(halt(m"apoplexy: the receiver has no spec `Source`"))
    val wire = members.at(t"Transport").lay(Wire.Json)(wireOfRepr(_))

    (locus, stringOf(source), wire)

  // --- path utilities ------------------------------------------------------

  private def segments(path: Text): List[Text] = path.cut(t"/").filter(_ != t"")
  private def isTemplate(segment: Text): Boolean = segment.starts(t"{") && segment.s.endsWith("}")
  private def templateName(segment: Text): Text = segment.skip(1).keep(segment.length - 2)

  private def isPrefix(short: List[Text], long: List[Text]): Boolean =
    short.length <= long.length && short.zip(long).all(_ == _)

  private def join(locus: Text, segment: Text): Text =
    if locus == t"/" then t"/$segment" else t"$locus/$segment"

  private def escape(text: Text): Text = text.sub(t"~", t"~0").sub(t"/", t"~1")

  // --- HTTP method helpers -------------------------------------------------

  private val verbs: Map[Text, Http.Method] =
    Map(t"get" -> Http.Get, t"post" -> Http.Post, t"put" -> Http.Put, t"patch" -> Http.Patch,
        t"delete" -> Http.Delete)

  private def methodName(method: Http.Method): Text = method match
    case Http.Post   => t"post"
    case Http.Put    => t"put"
    case Http.Patch  => t"patch"
    case Http.Delete => t"delete"
    case _           => t"get"

  private def methodExpr(using Quotes)(method: Http.Method): Expr[Http.Method] = method match
    case Http.Post   => '{Http.Post}
    case Http.Put    => '{Http.Put}
    case Http.Patch  => '{Http.Patch}
    case Http.Delete => '{Http.Delete}
    case _           => '{Http.Get}

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
        item.operations.values.flatMap(_.parameters).to[List]

    params.find { p => p.name == parameter && p.`in` == OpenApi.Parameter.In.Path } match
      case Some(param) => param.schema.lay(TypeRepr.of[Text])(schemaType(doc, _))
      case None        => TypeRepr.of[Text]

  // --- wire format ---------------------------------------------------------

  // The wire format an operation speaks, inferred from its `content` media types.
  // The end-user never chooses this; the OpenAPI spec dictates it.
  private enum Wire:
    case Json, Xml

  private def mediaOf(wire: Wire): Text = wire match
    case Wire.Json => t"application/json"
    case Wire.Xml  => t"application/xml"

  // JSON wins ties (the OpenAPI default, and the historical behaviour).
  private def wireOf(content: Map[Text, OpenApi.MediaTypeObject]): Optional[Wire] =
    if content.nil then Unset
    else if content.has(t"application/json") then Wire.Json
    else if content.has(t"application/xml") || content.has(t"text/xml") then Wire.Xml
    else Wire.Json

  // The wire format of an operation's first 2xx response body, if any.
  private def responseWire(operation: OpenApi.Operation): Optional[Wire] =
    val status = operation.responses.keys.filter(_.starts(t"2")).to[List].sortBy(_.s).prim

    status.let(operation.responses.at(_)).let: response =>
      wireOf(response.content)

  // The spec-wide wire format if every operation agrees, else `Json` as a neutral
  // placeholder for navigation types. The authoritative format is always recomputed
  // per operation by `invoke`.
  private def uniformWire(doc: OpenApi): Wire =
    val wires =
      doc.paths.values.flatMap(_.operations.values).to[List].flatMap: operation =>
        responseWire(operation).lay(List[Wire]())(List(_))

    . to[Set]

    if wires.size == 1 then wires.head else Wire.Json

  private def transportRepr(using quotes: Quotes)(wire: Wire): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    wire match
      case Wire.Json => TypeRepr.of[Json]
      case Wire.Xml  => TypeRepr.of[Xml]

  private def wireOfRepr(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): Wire =
    import quotes.reflect.*
    if repr =:= TypeRepr.of[Xml] then Wire.Xml else Wire.Json

  // --- invocation ----------------------------------------------------------

  // Builds the `Api.Response` for invoking `method` on the complete endpoint
  // `locus`: typechecks named args against the query parameters, the single
  // optional positional against the request body, and records the 2xx response
  // schema's pointer in the result type.
  private def invoke(using quotes: Quotes)
    ( self:       Expr[Api],
      doc:        OpenApi,
      source:     Text,
      locus:      Text,
      method:     Http.Method,
      named:      List[(Text, Expr[Any])],
      positional: List[Expr[Any]] )
  :   Expr[Any] =

    import quotes.reflect.*

    val verb = methodName(method)

    val operation = doc.paths.at(locus).let(_.operations.at(method)).or:
      halt(m"apoplexy: $locus defines no $verb operation")

    val queryParams = operation.parameters.filter(_.`in` == OpenApi.Parameter.In.Query)

    val queryEntries: List[Expr[(Text, Text)]] = named.map: (name, argExpr) =>
      val param = queryParams.find(_.name == name).getOrElse:
        halt(m"apoplexy: $verb $locus has no query parameter $name")

      val expected = param.schema.lay(TypeRepr.of[Text])(schemaType(doc, _))
      val actual = argExpr.asTerm.tpe.widen

      if !(actual <:< expected)
      then halt(m"apoplexy: the query parameter $name expects ${expected.show}")

      actual.asType.absolve match
        case '[argType] =>
          val value = argExpr.asExprOf[argType]

          val showable = Expr.summon[argType is Showable].getOrElse:
            halt(m"apoplexy: the query parameter $name cannot be rendered as text")

          '{(${Expr(name.s)}.tt, $showable.text($value))}

    queryParams.filter(_.required.or(false)).each: param =>
      if !named.exists(_(0) == param.name)
      then halt(m"apoplexy: required query parameter ${param.name} is missing")

    val queryExpr = Expr.ofList(queryEntries.scala)

    val status =
      operation.responses.keys.filter(_.starts(t"2")).to[List].sortBy(_.s).prim.or(t"200")

    // The wire format the spec dictates for this operation: the response body's
    // media type, else the request body's, else JSON. An operation that mixes
    // request and response media types is not supported.
    val respWire = operation.responses.at(status).let: response =>
      wireOf(response.content)

    val reqWire = operation.requestBody.let: body =>
      wireOf(body.content)

    if respWire.present && reqWire.present && respWire.vouch != reqWire.vouch
    then halt(m"apoplexy: $verb $locus mixes request and response media types")

    val wire = respWire.or(reqWire.or(Wire.Json))

    val bodyExpr: Expr[Api.Body] = positional match
      case Nil =>
        if operation.requestBody.let(_.required.or(false)).or(false)
        then halt(m"apoplexy: $verb $locus requires a request body")

        '{Api.Body.Empty}

      case List(argExpr) =>
        if operation.requestBody.absent then halt(m"apoplexy: $verb $locus takes no request body")

        argExpr.asTerm.tpe.widen.asType.absolve match
          case '[bodyType] =>
            val value = argExpr.asExprOf[bodyType]

            wire match
              case Wire.Json =>
                val encodable = Expr.summon[bodyType is Encodable in Json].getOrElse:
                  halt(m"apoplexy: the request body cannot be encoded as JSON")

                '{Api.Body.Json($encodable.encoded($value))}

              case Wire.Xml =>
                val encodable = Expr.summon[bodyType is Encodable in Xml].getOrElse:
                  halt(m"apoplexy: the request body cannot be encoded as XML")

                '{Api.Body.Xml($encodable.encoded($value))}

      case _ =>
        halt(m"apoplexy: $verb $locus takes a single request body")

    val mediaContent = escape(mediaOf(wire))

    val pointer =
      t"#/paths/${escape(locus)}/$verb/responses/$status/content/$mediaContent/schema"

    val mExpr = methodExpr(method)
    val locusExpr = Expr(locus.s)

    val responseType =
      Refinement
        ( Refinement
           ( Refinement(TypeRepr.of[Api.Response], "Result", bounds(literalType(pointer))),
             "Form",
             bounds(literalType(source)) ),
          "Transport",
          bounds(transportRepr(wire)) )

    responseType.asType.absolve match
      case '[type result <: Api.Response; result] =>
        ' {
            val request =
              $self.request.copy
                ( method = $mExpr,
                  path   = $locusExpr.tt,
                  query  = List.from($queryExpr),
                  body   = $bodyExpr )

            Api.Response.make(request).asInstanceOf[result]
          }

  // Invokes the sole non-DELETE method of a complete endpoint (the `apply`
  // shortcut). DELETE never participates: a sole-DELETE endpoint still requires
  // an explicit `.delete()`.
  private def shortcut(using quotes: Quotes)
    ( self: Expr[Api], doc: OpenApi, source: Text, locus: Text,
      named: List[(Text, Expr[Any])], positional: List[Expr[Any]] )
  :   Expr[Any] =

    val methods =
      doc.paths.at(locus).lay(List[Http.Method]()): item =>
        item.operations.keys.filter(_ != Http.Delete).to[List]

    methods match
      case List(method) =>
        invoke(self, doc, source, locus, method, named, positional)

      case Nil =>
        halt(m"apoplexy: $locus has no invokable operation (use `.delete()` for a DELETE endpoint)")

      case _ =>
        halt(m"apoplexy: $locus has several operations; use `.get`, `.post`, `.put` or `.patch`")

  // Extracts the (name, value) pairs from `applyDynamicNamed` arguments; a
  // positional argument arrives with an empty name.
  private def pairs(using quotes: Quotes)(args: Expr[Seq[(String, Any)]])
  :   List[(Text, Expr[Any])] =

    args match
      case Varargs(exprs) => List.from(exprs).map:
        case '{($key: String, $value)} => (key.valueOrAbort.tt, value)
        case _                         => halt(m"apoplexy: arguments must be passed directly")

      case _ =>
        halt(m"apoplexy: arguments must be passed directly")

  private def defines(using Quotes)(doc: OpenApi, locus: Text, method: Http.Method): Boolean =
    doc.paths.at(locus).let(_.operations.contains(method)).or(false)

  // --- macros --------------------------------------------------------------

  def root(resource: Expr[Resource]): Macro[Api] =
    import quotes.reflect.*

    val members = refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)

    val source =
      members.at(t"Locus").lay(halt(m"apoplexy: the resource has no `Locus` path"))(stringOf(_))

    val doc = spec(source)
    val base = if doc.servers.nil then t"" else doc.servers.head.url
    val baseExpr = Expr(base.s)
    val wire = uniformWire(doc)

    apiType(t"/", source, wire).asType.absolve match
      case '[type result <: Api; result] =>
        '{Api.make(Api.Request(Http.Get, $baseExpr.tt, t"/")).asInstanceOf[result]}

  def select(self: Expr[Api], field: Expr[String]): Macro[Any] =
    val name = field.valueOrAbort.tt
    val (locus, source, wire) = receiver(self)
    val doc = spec(source)

    verbs.at(name) match
      case method: Http.Method if defines(doc, locus, method) =>
        invoke(self, doc, source, locus, method, Nil, Nil)

      case _ =>
        navigate(self, source, doc, locus, name, wire)

  private def navigate(using quotes: Quotes)
    ( self: Expr[Api], source: Text, doc: OpenApi, locus: Text, name: Text, wire: Wire )
  :   Expr[Any] =

    val newLocus = join(locus, name)
    val newSegs = segments(newLocus)
    val keys = doc.paths.keys.map(segments).to[List]

    if !keys.exists(isPrefix(newSegs, _)) then halt(m"apoplexy: no path begins with $newLocus")

    val locusExpr = Expr(newLocus.s)

    apiType(newLocus, source, wire).asType.absolve match
      case '[type result <: Api; result] =>
        '{Api.make($self.request.copy(path = $locusExpr.tt)).asInstanceOf[result]}

  def applied(self: Expr[Api], field: Expr[String], args: Expr[Seq[Any]]): Macro[Any] =
    val name = field.valueOrAbort.tt
    val (locus, source, wire) = receiver(self)
    val doc = spec(source)

    val positional = args match
      case Varargs(exprs) => List.from(exprs)
      case _              => halt(m"apoplexy: arguments must be passed directly")

    verbs.at(name) match
      case method: Http.Method if defines(doc, locus, method) =>
        invoke(self, doc, source, locus, method, Nil, positional)

      case _ =>
        val newLocus = join(locus, name)
        val newSegs = segments(newLocus)
        val keys = doc.paths.keys.map(segments).to[List]

        if !keys.exists(isPrefix(newSegs, _)) then halt(m"apoplexy: no path begins with $newLocus")

        def deeper(key: List[Text]): Boolean =
          isPrefix(newSegs, key) && key.length > newSegs.length

        val following = keys.filter(deeper).map(_(newSegs.length)).find(isTemplate)

        following match
          case None => shortcut(self, doc, source, newLocus, Nil, positional)

          case Some(template) =>
            fillTemplate(self, source, doc, newLocus, template, positional, wire)

  private def fillTemplate(using quotes: Quotes)
    ( self:       Expr[Api],
      source:     Text,
      doc:        OpenApi,
      newLocus:   Text,
      template:   Text,
      positional: List[Expr[Any]],
      wire:       Wire )
  :   Expr[Any] =

    import quotes.reflect.*

    val parameter = templateName(template)
    val templatedLocus = join(newLocus, template)

    val arg = positional match
      case List(only) => only
      case _          => halt(m"apoplexy: path parameter $parameter needs one argument")

    val expected = pathParamType(doc, templatedLocus, parameter)
    val actual = arg.asTerm.tpe.widen

    if !(actual <:< expected)
    then halt(m"apoplexy: path parameter $parameter expects ${expected.show}")

    val locusExpr = Expr(templatedLocus.s)
    val paramExpr = Expr(parameter.s)

    apiType(templatedLocus, source, wire).asType.absolve match
      case '[type result <: Api; result] => actual.asType.absolve match
        case '[argType] =>
          val value = arg.asExprOf[argType]

          val showable = Expr.summon[argType is Showable].getOrElse:
            halt(m"apoplexy: the path parameter $parameter cannot be rendered as text")

          ' {
              val rendered = $showable.text($value)
              val updated = $self.request.substitutions.updated($paramExpr.tt, rendered)

              Api.make($self.request.copy(path = $locusExpr.tt, substitutions = updated))
              . asInstanceOf[result]
            }

  def appliedNamed(self: Expr[Api], field: Expr[String], args: Expr[Seq[(String, Any)]])
  :   Macro[Any] =

    val name = field.valueOrAbort.tt
    val (locus, source, wire) = receiver(self)
    val doc = spec(source)
    val entries = pairs(args)
    val named = entries.filter(_(0) != t"")
    val positional = entries.filter(_(0) == t"").map(_(1))

    verbs.at(name) match
      case method: Http.Method if defines(doc, locus, method) =>
        invoke(self, doc, source, locus, method, named, positional)

      case _ =>
        val newLocus = join(locus, name)
        val newSegs = segments(newLocus)
        val keys = doc.paths.keys.map(segments).to[List]

        if !keys.exists(isPrefix(newSegs, _)) then halt(m"apoplexy: no path begins with $newLocus")

        if doc.paths.at(newLocus).absent
        then halt(m"apoplexy: $newLocus is not a complete endpoint")

        shortcut(self, doc, source, newLocus, named, positional)

  // --- response decoding ---------------------------------------------------

  private val specJsons: scala.collection.mutable.HashMap[Text, Json] =
    scala.collection.mutable.HashMap()

  private def specJson(using Quotes)(source: Text): Json =
    specJsons.synchronized:
      specJsons.at(source).or:
        val stream = Optional(getClass.getResourceAsStream(source.s)).or:
          halt(m"apoplexy: could not read the OpenAPI spec at $source on the classpath")

        val content = scala.io.Source.fromInputStream(stream).mkString.tt

        val json =
          try content.read[Json]
          catch case error: Exception => halt(m"apoplexy: the OpenAPI spec at $source is not valid")

        specJsons(source) = json
        json

  // Resolve the response-schema `JsonSchema` at a JSON-pointer into the spec.
  private def resolveSchema(using Quotes)(source: Text, pointer: Text): JsonSchema =
    val segments = pointer.cut(t"/").to[List].drop(1)

    val node =
      segments.foldLeft(specJson(source)): (node, segment) =>
        try node(segment.sub(t"~1", t"/").sub(t"~0", t"~"))
        catch case error: Exception => halt(m"apoplexy: could not resolve the schema at $pointer")

    try node.as[JsonSchema]
    catch case error: Exception => halt(m"apoplexy: the response schema at $pointer is not valid")

  // Compile-time check that `value` structurally matches the response schema.
  private def conformsTo(using quotes: Quotes)(value: quotes.reflect.TypeRepr, schema: JsonSchema)
  :   Unit =

    import quotes.reflect.*

    def simpleName(repr: TypeRepr): Text = repr.dealias.typeSymbol.name.tt

    def listElement(repr: TypeRepr): Optional[TypeRepr] = repr match
      case AppliedType(_, scala.collection.immutable.List(element)) if repr <:< TypeRepr.of[List[Any]] => element
      case _                                                                => Unset

    def componentName(pointer: JsonPointer): Text = pointer.encode.cut(t"/").to[List].last

    def ok(value: TypeRepr, schema: JsonSchema): Boolean = schema match
      case ref: JsonSchema.Ref   => simpleName(value) == componentName(ref.pointer)
      case _: JsonSchema.String  => value =:= TypeRepr.of[Text]
      case _: JsonSchema.Integer => value =:= TypeRepr.of[Int]
      case _: JsonSchema.Number  => value =:= TypeRepr.of[Double]
      case _: JsonSchema.Boolean => value =:= TypeRepr.of[Boolean]
      case _: JsonSchema.Object  => value.typeSymbol.flags.is(Flags.Case)

      case array: JsonSchema.Array =>
        listElement(value).lay(false): element =>
          array.items.lay(true)(ok(element, _))

      case _ => true

    if !ok(value, schema)
    then halt(m"apoplexy: ${value.show} does not conform to the response schema")

  // The inline entry point called from `Api.Response.call`: a splice cannot appear
  // as a mid-body statement in an inline method, so the macro is wrapped here.
  transparent inline def check[value](inline self: Api.Response): Unit =
    ${conform[value]('self)}

  // Compile-time only: verify `value` conforms to the endpoint's response
  // schema. The actual send and decode happen in inline code in `Api.Response.call`
  // (where `value` is concrete), so this returns `Unit`. The raw `Http.Response`
  // and `Json` targets bypass the schema check.
  def conform[value: Type](self: Expr[Api.Response]): Macro[Unit] =
    import quotes.reflect.*

    val valueRepr = TypeRepr.of[value]

    val raw =
      valueRepr =:= TypeRepr.of[Http.Response]
      || valueRepr =:= TypeRepr.of[Json]
      || valueRepr =:= TypeRepr.of[Unit]

    if !raw then
      val members = refinements(self.asTerm.tpe) ++ refinements(self.asTerm.tpe.widen)

      val pointer =
        members.at(t"Result").lay(halt(m"apoplexy: missing response schema pointer"))(stringOf(_))

      val source =
        members.at(t"Form").lay(halt(m"apoplexy: missing spec source"))(stringOf(_))

      conformsTo(valueRepr, resolveSchema(source, pointer))

    '{()}
