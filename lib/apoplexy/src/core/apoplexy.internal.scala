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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.collection.immutable.Seq

import scala.quoted.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gigantism.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import vacuous.*
import xylophone.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely
import denominative.dysasymptotics.linearSize
import rudiments.sortingAlgorithms.timsort

object Apoplexy:
  // --- compile-time spec access -------------------------------------------

  private val specs: scala.collection.mutable.HashMap[Text, OpenApi] =
    scala.collection.mutable.HashMap()

  private def resource(using Quotes)(source: Text): Text =
    val stream = Optional(getClass.getResourceAsStream(source.s)).or:
      halt(m"apoplexy: could not read the OpenAPI spec at $source on the classpath")

    scala.io.Source.fromInputStream(stream).mkString.tt

  // The caches are keyed by the resource's content as well as its name, so that a spec edited
  // between two compilations in the same compiler process is read afresh.
  private def key(source: Text, content: Text): Text = t"$source#${content.hashCode}"

  private def spec(using Quotes)(source: Text): OpenApi =
    val content = resource(source)

    specs.synchronized:
      specs.at(key(source, content)).or:
        val doc =
          try content.read[OpenApi]
          catch case error: Exception => halt(m"apoplexy: the OpenAPI spec at $source is not valid")

        specs(key(source, content)) = doc
        doc

  // --- refinement-type helpers (mirroring xenophile) ----------------------

  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   scala.collection.immutable.Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => scala.collection.immutable.Map()

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

    val members = refinements(self.asTerm.tpe.widen).to(Map)
    val locus = members(t"Locus").lay(t"/")(stringOf(_))
    val source = members(t"Source").or(halt(m"apoplexy: the receiver has no spec `Source`"))
    val wire = members(t"Transport").lay(Wire.Json)(wireOfRepr(_))

    (locus, stringOf(source), wire)

  // --- path utilities ------------------------------------------------------

  private def segments(path: Text): List[Text] = path.cut(t"/").filter(_ != t"")
  private def isTemplate(segment: Text): Boolean = segment.starts(t"{") && segment.s.endsWith("}")
  private def templateName(segment: Text): Text = segment.skip(1).keep(segment.length - 2)

  private def isPrefix(short: List[Text], long: List[Text]): Boolean =
    short.size <= long.size && short.zip(long).all(_ == _)

  private def join(locus: Text, segment: Text): Text =
    if locus == t"/" then t"/$segment" else t"$locus/$segment"

  private def escape(text: Text): Text = text.sub(t"~", t"~0").sub(t"/", t"~1")

  // --- HTTP method helpers -------------------------------------------------

  private val verbs: Map[Text, Http.Method] =
    Map(t"get" -> Http.Get, t"post" -> Http.Post, t"put" -> Http.Put, t"patch" -> Http.Patch,
        t"delete" -> Http.Delete, t"head" -> Http.Head, t"options" -> Http.Options,
        t"trace" -> Http.Trace)

  private def methodName(method: Http.Method): Text = method match
    case Http.Post    => t"post"
    case Http.Put     => t"put"
    case Http.Patch   => t"patch"
    case Http.Delete  => t"delete"
    case Http.Head    => t"head"
    case Http.Options => t"options"
    case Http.Trace   => t"trace"
    case _            => t"get"

  private def methodExpr(using Quotes)(method: Http.Method): Expr[Http.Method] = method match
    case Http.Post    => '{Http.Post}
    case Http.Put     => '{Http.Put}
    case Http.Patch   => '{Http.Patch}
    case Http.Delete  => '{Http.Delete}
    case Http.Head    => '{Http.Head}
    case Http.Options => '{Http.Options}
    case Http.Trace   => '{Http.Trace}
    case _            => '{Http.Get}

  // --- the specification's components --------------------------------------

  // A component written in place or by reference, resolved; a reference the document cannot
  // satisfy is a compile error naming it.
  private def resolve[value <: OpenApi.Parameter | OpenApi.Response | OpenApi.RequestBody]
    ( using Quotes, value is OpenApi.Componental )
    ( doc: OpenApi, referable: OpenApi.Referable[value] )
  :   value =

    given OpenApi = doc

    try referable()
    catch case error: OpenApi.Error => halt(m"apoplexy: ${error.message}")

  // The parameters of an operation: those its path item declares for every operation, overridden
  // by its own on the same name and location, each resolved through the components.
  private def parameters(using Quotes)(doc: OpenApi, locus: Text, method: Http.Method)
  :   List[OpenApi.Parameter] =

    val item = doc.paths(locus)
    def resolved(referables: List[OpenApi.Referable[OpenApi.Parameter]])
    :   List[OpenApi.Parameter] =

      referables.map(resolve[OpenApi.Parameter](doc, _))

    val operation = item.let(_.operations(method))
    val own = operation.lay(List[OpenApi.Parameter]())(_.parameters.pipe(resolved))
    val shared = item.lay(List[OpenApi.Parameter]())(_.parameters.pipe(resolved))

    def overridden(parameter: OpenApi.Parameter): Boolean =
      own.exists(param => param.name == parameter.name && param.`in` == parameter.`in`)

    List.concat(own, shared.filter(!overridden(_)))

  // --- schema → Scala type -------------------------------------------------

  private def schemaType(using quotes: Quotes)(doc: OpenApi, schema: JsonSchema)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    schema match
      case integer: JsonSchema.Integer =>
        if integer.format == JsonSchema.Format.Int64 then TypeRepr.of[Long] else TypeRepr.of[Int]

      case _: JsonSchema.Number  => TypeRepr.of[Double]
      case _: JsonSchema.String  => TypeRepr.of[Text]
      case _: JsonSchema.Boolean => TypeRepr.of[Boolean]

      case array: JsonSchema.Array =>
        array.items.lay(TypeRepr.of[Json])(schemaType(doc, _)).asType.absolve match
          case '[element] => TypeRepr.of[List[element]]

      case _ =>
        TypeRepr.of[Json]

  // Whether an argument's type may fill a parameter of the schema's type: exactly, or by the
  // widenings a caller would expect, an `Int` where the schema says int64, or an integer where
  // it says number.
  private def conforms(using quotes: Quotes)
    ( actual: quotes.reflect.TypeRepr, expected: quotes.reflect.TypeRepr )
  :   Boolean =

    import quotes.reflect.*

    val integral = actual <:< TypeRepr.of[Int] || actual <:< TypeRepr.of[Long]
    val widensToLong = expected =:= TypeRepr.of[Long] && actual <:< TypeRepr.of[Int]
    val fractional = integral || actual <:< TypeRepr.of[Float]
    val widensToDouble = expected =:= TypeRepr.of[Double] && fractional

    actual <:< expected || widensToLong || widensToDouble

  private def pathParamType(using quotes: Quotes)(doc: OpenApi, path: Text, parameter: Text)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val params =
      doc.paths(path).lay(List[OpenApi.Parameter]()): item =>
        item.operations.keys.to[List].bind(parameters(doc, path, _))

    def matches(param: OpenApi.Parameter): Boolean =
      param.name == parameter && param.`in` == OpenApi.Parameter.In.Path

    params.seek(matches).lay(TypeRepr.of[Text]): param =>
      param.schema.lay(TypeRepr.of[Text])(schemaType(doc, _))

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
    else if content.defines(t"application/json") then Wire.Json
    else if content.defines(t"application/xml") || content.defines(t"text/xml") then Wire.Xml
    else Wire.Json

  // The status of the response an operation's success returns: `200` or `201` where the
  // operation declares one, else its lowest-numbered 2xx.
  private def successStatus(operation: OpenApi.Operation): Optional[Text] =
    val statuses = operation.responses.keys.filter(_.starts(t"2")).to[List]

    if statuses.has(t"200") then t"200"
    else if statuses.has(t"201") then t"201"
    else statuses.order(_.s).prim

  // The wire format of an operation's success response body, if any.
  private def responseWire(using Quotes)(doc: OpenApi, operation: OpenApi.Operation)
  :   Optional[Wire] =

    successStatus(operation).let(operation.responses(_)).let(resolve[OpenApi.Response](doc, _))
    . let: response => wireOf(response.content)

  // The spec-wide wire format if every operation agrees, else `Json` as a neutral
  // placeholder for navigation types. The authoritative format is always recomputed
  // per operation by `invoke`.
  private def uniformWire(using Quotes)(doc: OpenApi): Wire =
    val wires =
      doc.paths.values.flatMap(_.operations.values).to[List].bind: operation =>
        responseWire(doc, operation).lay(List[Wire]())(List(_))

    . to[Set]

    wires.to[List] match
      case List(wire) => wire
      case _          => Wire.Json

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
  // `locus`: typechecks named args against the query and header parameters, the
  // single optional positional against the request body, and records the success
  // response schema's pointer in the result type.
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

    val operation = doc.paths(locus).let(_.operations(method)).or:
      halt(m"apoplexy: $locus defines no $verb operation")

    val params = parameters(doc, locus, method)
    val queryParams = params.filter(_.`in` == OpenApi.Parameter.In.Query)
    val headerParams = params.filter(_.`in` == OpenApi.Parameter.In.Header)

    // A named argument fills the query parameter of that name, else the header parameter
    def entry(name: Text, argExpr: Expr[Any], param: OpenApi.Parameter): Expr[(Text, Text)] =
      val expected = param.schema.lay(TypeRepr.of[Text])(schemaType(doc, _))
      val actual = argExpr.asTerm.tpe.widen
      val where = if param.`in` == OpenApi.Parameter.In.Query then t"query" else t"header"

      if !conforms(actual, expected)
      then halt(m"apoplexy: the $where parameter $name expects ${expected.show}")

      actual.asType.absolve match
        case '[argType] =>
          val value = argExpr.asExprOf[argType]

          val showable = Expr.summon[argType is Showable].getOrElse:
            halt(m"apoplexy: the $where parameter $name cannot be rendered as text")

          '{(${Expr(name.s)}.tt, $showable.text($value))}

    val queryEntries: List[Expr[(Text, Text)]] = named.bind: (name, argExpr) =>
      queryParams.seek(_.name == name).lay(List[Expr[(Text, Text)]]()): param =>
        List(entry(name, argExpr, param))

    val headerEntries: List[Expr[(Text, Text)]] = named.bind: (name, argExpr) =>
      headerParams.seek(_.name == name).lay(List[Expr[(Text, Text)]]()): param =>
        List(entry(name, argExpr, param))

    named.each: (name, _) =>
      if !queryParams.exists(_.name == name) && !headerParams.exists(_.name == name)
      then halt(m"apoplexy: $verb $locus has no query or header parameter $name")

    List.concat(queryParams, headerParams).filter(_.required.or(false)).each: param =>
      if !named.exists(_(0) == param.name)
      then halt(m"apoplexy: required parameter ${param.name} is missing")

    val queryExpr = Lifts.list(queryEntries)
    val headersExpr = Lifts.list(headerEntries)

    val status = successStatus(operation).or(t"200")

    val response: Optional[OpenApi.Referable[OpenApi.Response]] = operation.responses(status)

    // The wire format the spec dictates for this operation: the response body's
    // media type, else the request body's, else JSON. An operation that mixes
    // request and response media types is not supported.
    val respWire = response.let(resolve[OpenApi.Response](doc, _)).let: response =>
      wireOf(response.content)

    val requestBody = operation.requestBody.let(resolve[OpenApi.RequestBody](doc, _))
    val reqWire = requestBody.let: body => wireOf(body.content)

    respWire.let: resp =>
      reqWire.let: req =>
        if resp != req
        then halt(m"apoplexy: $verb $locus mixes request and response media types")

    val wire = respWire.or(reqWire.or(Wire.Json))

    val bodyExpr: Expr[Api.Body] = positional match
      case Nil =>
        if requestBody.let(_.required.or(false)).or(false)
        then halt(m"apoplexy: $verb $locus requires a request body")

        '{Api.Body.Empty}

      case List(argExpr) =>
        if requestBody.absent then halt(m"apoplexy: $verb $locus takes no request body")

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

    // The schema's pointer: into the components when the response is a reference, else into
    // the operation
    val pointer = response match
      case OpenApi.Ref(reference) =>
        t"${reference.encode}/content/$mediaContent/schema"

      case _ =>
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
                ( method  = $mExpr,
                  path    = $locusExpr.tt,
                  query   = $queryExpr,
                  body    = $bodyExpr,
                  headers = $headersExpr )

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
      doc.paths(locus).lay(List[Http.Method]()): item =>
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

    // Hoisted from the `map` below: quote patterns inside a combinator lambda in a macro risk
    // the `wildApprox` crash.
    def pair(expr: Expr[(String, Any)]): (Text, Expr[Any]) = expr match
      case '{($key: String, $value)} => (key.valueOrAbort.tt, value)
      case _                         => halt(m"apoplexy: arguments must be passed directly")

    args match
      case Lifts.Varargs(exprs) => exprs.map(pair)

      case _ =>
        halt(m"apoplexy: arguments must be passed directly")

  private def defines(using Quotes)(doc: OpenApi, locus: Text, method: Http.Method): Boolean =
    doc.paths(locus).let(_.operations.defines(method)).or(false)

  // --- macros --------------------------------------------------------------

  def root(resource: Expr[Resource]): Macro[Api] = rootWith(resource, Unset)

  def rootAt(resource: Expr[Resource], base: Expr[Text]): Macro[Api] = rootWith(resource, base)

  // The base URL comes from the spec's first server, with its variables at their defaults. A
  // server URL which is relative (`/api/v3`), or absent, needs a base from the caller, which it
  // then extends; a caller's base replaces an absolute server URL outright.
  private def rootWith(using Quotes)(resource: Expr[Resource], base: Optional[Expr[Text]])
  :   Expr[Api] =

    import quotes.reflect.*

    val members = (refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)).to(Map)

    val source =
      members(t"Locus").lay(halt(m"apoplexy: the resource has no `Locus` path"))(stringOf(_))

    val doc = spec(source)
    val server = doc.servers.prim.lay(t"")(_.resolved)
    val serverExpr = Expr(server.s)

    // A plain match: a quote inside an inline argument (`lay`'s lambda) crashes the pickler
    val baseExpr: Expr[Text] = base match
      case Unset =>
        '{$serverExpr.tt}

      case supplied: Expr[Text] @unchecked =>
        if server.starts(t"/") then '{($supplied.s + $serverExpr).tt} else supplied

    val wire = uniformWire(doc)

    apiType(t"/", source, wire).asType.absolve match
      case '[type result <: Api; result] =>
        '{Api.make(Api.Request(Http.Get, $baseExpr, t"/")).asInstanceOf[result]}

  def select(self: Expr[Api], field: Expr[String]): Macro[Any] =
    val name = field.valueOrAbort.tt
    val (locus, source, wire) = receiver(self)
    val doc = spec(source)

    verbs(name) match
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
      case Lifts.Varargs(exprs) => exprs
      case _                    => halt(m"apoplexy: arguments must be passed directly")

    verbs(name) match
      case method: Http.Method if defines(doc, locus, method) =>
        invoke(self, doc, source, locus, method, Nil, positional)

      case _ =>
        val newLocus = join(locus, name)
        val newSegs = segments(newLocus)
        val keys = doc.paths.keys.map(segments).to[List]

        if !keys.exists(isPrefix(newSegs, _)) then halt(m"apoplexy: no path begins with $newLocus")

        def deeper(key: List[Text]): Boolean =
          isPrefix(newSegs, key) && key.size > newSegs.size

        keys.filter(deeper).map(_.stdlib(newSegs.size)).seek(isTemplate).lay:
          shortcut(self, doc, source, newLocus, Nil, positional)
        . apply: template =>
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

    if !conforms(actual, expected)
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
              val updated = $self.request.substitutions.define($paramExpr.tt, rendered)

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

    verbs(name) match
      case method: Http.Method if defines(doc, locus, method) =>
        invoke(self, doc, source, locus, method, named, positional)

      case _ =>
        val newLocus = join(locus, name)
        val newSegs = segments(newLocus)
        val keys = doc.paths.keys.map(segments).to[List]

        if !keys.exists(isPrefix(newSegs, _)) then halt(m"apoplexy: no path begins with $newLocus")

        if doc.paths(newLocus).absent
        then halt(m"apoplexy: $newLocus is not a complete endpoint")

        shortcut(self, doc, source, newLocus, named, positional)

  // --- response decoding ---------------------------------------------------

  private val specJsons: scala.collection.mutable.HashMap[Text, Json] =
    scala.collection.mutable.HashMap()

  private def specJson(using Quotes)(source: Text): Json =
    val content = resource(source)

    specJsons.synchronized:
      specJsons.at(key(source, content)).or:
        val json =
          try OpenApi.sourceJson(content)
          catch case error: Exception => halt(m"apoplexy: the OpenAPI spec at $source is not valid")

        specJsons(key(source, content)) = json
        json

  // Resolve the response-schema `JsonSchema` at a JSON-pointer into the spec.
  private def resolveSchema(using Quotes)(source: Text, pointer: Text): JsonSchema =
    val segments = pointer.cut(t"/").skip(1)

    val node =
      segments.fold(specJson(source)): (node, segment) =>
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

    def componentName(pointer: JsonPointer): Text = pointer.encode.cut(t"/").stdlib.last

    def ok(value: TypeRepr, schema: JsonSchema): Boolean = schema match
      case ref: JsonSchema.Ref   => simpleName(value) == componentName(ref.pointer)
      case _: JsonSchema.String  => value =:= TypeRepr.of[Text]
      case _: JsonSchema.Integer => value =:= TypeRepr.of[Int] || value =:= TypeRepr.of[Long]
      case _: JsonSchema.Number  => value =:= TypeRepr.of[Double] || value =:= TypeRepr.of[Float]
      case _: JsonSchema.Boolean => value =:= TypeRepr.of[Boolean]
      case _: JsonSchema.Object  => value.typeSymbol.flags.is(Flags.Case)

      case array: JsonSchema.Array =>
        listElement(value).lay(false): element => array.items.lay(true)(ok(element, _))

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
      valueRepr =:= TypeRepr.of[Http.Response] ||
        valueRepr =:= TypeRepr.of[Json] ||
        valueRepr =:= TypeRepr.of[Unit]

    if !raw then
      val members = (refinements(self.asTerm.tpe) ++ refinements(self.asTerm.tpe.widen)).to(Map)

      val pointer =
        members(t"Result").lay(halt(m"apoplexy: missing response schema pointer"))(stringOf(_))

      val source =
        members(t"Form").lay(halt(m"apoplexy: missing spec source"))(stringOf(_))

      conformsTo(valueRepr, resolveSchema(source, pointer))

    '{()}
