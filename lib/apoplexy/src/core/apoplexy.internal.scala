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
import distillate.*
import fulminate.*
import gesticulate.*
import gigantism.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import jacinta.*
import legerdemain.*
import polyvinyl.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
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

  private def apiType(using quotes: Quotes)
    ( locus: Text, source: Text, transport: quotes.reflect.TypeRepr )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val withLocus = Refinement(TypeRepr.of[Api], "Locus", bounds(literalType(locus)))
    val withSource = Refinement(withLocus, "Source", bounds(literalType(source)))
    Refinement(withSource, "Transport", bounds(transport))

  private def receiver(using quotes: Quotes)(self: Expr[Api])
  :   (Text, Text, quotes.reflect.TypeRepr) =

    import quotes.reflect.*

    val members = refinements(self.asTerm.tpe.widen).to(Map)
    val locus = members(t"Locus").lay(t"/")(stringOf(_))
    val source = members(t"Source").or(halt(m"apoplexy: the receiver has no spec `Source`"))
    val transport = members(t"Transport").or(TypeRepr.of[Json])

    (locus, stringOf(source), transport)

  // --- path utilities ------------------------------------------------------

  private def segments(path: Text): List[Text] = path.cut(t"/").filter(_ != t"")
  private def isTemplate(segment: Text): Boolean = segment.starts(t"{") && segment.s.endsWith("}")
  private def templateName(segment: Text): Text = segment.skip(1).keep(segment.length - 2)

  private def isPrefix(short: List[Text], long: List[Text]): Boolean =
    short.size <= long.size && short.zip(long).all(_ == _)

  private def join(locus: Text, segment: Text): Text =
    if locus == t"/" then t"/$segment" else t"$locus/$segment"

  private def escape(text: Text): Text = text.sub(t"~", t"~0").sub(t"/", t"~1")

  // The specification's own key for a path, which may carry a trailing slash the navigation
  // does not (`/apis/rbac.authorization.k8s.io/v1/`), and its item
  private def pathItem(doc: OpenApi, locus: Text): Optional[(Text, OpenApi.PathItem)] =
    doc.paths(locus).let((locus, _)).or:
      val target = segments(locus)

      doc.paths.keys.to[List].seek { key => segments(key) == target }.let: key =>
        doc.paths(key).let((key, _))

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

    val item = pathItem(doc, locus).let(_(1))
    def resolved(referables: List[OpenApi.Referable[OpenApi.Parameter]])
    :   List[OpenApi.Parameter] =

      referables.map(resolve[OpenApi.Parameter](doc, _))

    val operation = item.let(_.operations(method))
    val own = operation.lay(List[OpenApi.Parameter]())(_.parameters.pipe(resolved))
    val shared = item.lay(List[OpenApi.Parameter]())(_.parameters.pipe(resolved))

    def overridden(parameter: OpenApi.Parameter): Boolean =
      own.exists: param => param.name == parameter.name && param.`in` == parameter.`in`

    List.concat(own, shared.filter(!overridden(_)))

  // --- schema → Scala type -------------------------------------------------

  private def schemaType(using quotes: Quotes)(doc: OpenApi, schema: JsonSchema)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    schema match
      // A reference to a component schema is followed, so a `$ref` to a string type reads as
      // `Text`; a chain deeper than a few hops (or a cycle) reads as `Json`
      case ref: JsonSchema.Ref =>
        def follow(schema: JsonSchema, depth: Int): JsonSchema = schema match
          case ref: JsonSchema.Ref if depth < 8 =>
            given OpenApi = doc

            try follow(OpenApi.apply(ref: JsonSchema)(), depth + 1)
            catch case error: OpenApi.Error => ref

          case other =>
            other

        follow(ref, 0) match
          case _: JsonSchema.Ref => TypeRepr.of[Json]
          case other             => schemaType(doc, other)

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

    // A parameter whose schema names no one type (`oneOf`, an object) takes any argument
    val untyped = expected =:= TypeRepr.of[Json]

    actual <:< expected || widensToLong || widensToDouble || untyped

  private def pathParamType(using quotes: Quotes)(doc: OpenApi, path: Text, parameter: Text)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val params =
      pathItem(doc, path).lay(List[OpenApi.Parameter]()): (_, item) =>
        item.operations.keys.to[List].bind(parameters(doc, path, _))

    def matches(param: OpenApi.Parameter): Boolean =
      param.name == parameter && param.`in` == OpenApi.Parameter.In.Path

    params.seek(matches).lay(TypeRepr.of[Text]): param =>
      param.schema.lay(TypeRepr.of[Text])(schemaType(doc, _))

  // --- media types and their carriers ---------------------------------------

  // A media type as a specification writes it (`application/json; charset=utf-8`), reduced to
  // the name a `Construable` is keyed by
  private def normalise(media: Text): Text = media.cut(t";").prim.or(media).trim.lower

  // The type which carries a media type, if a `Construable` for it is in scope at the expansion.
  // A structured-syntax suffix (`application/problem+json`) falls back to the type it names
  // (`application/json`).
  private def construable(using quotes: Quotes)(media: Text): Optional[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    def search(name: Text): Optional[TypeRepr] =
      val target = Refinement(TypeRepr.of[Construable], "Self", bounds(literalType(name)))

      Implicits.search(target) match
        case success: ImplicitSearchSuccess =>
          refinements(success.tree.tpe.widen).get(t"Result") match
            case Some(result) => result
            case None         => Unset

        case _ =>
          Unset

    search(media).or:
      val plus = media.s.lastIndexOf('+')
      val group = media.cut(t"/").prim.or(t"application")

      if plus < 0 then Unset else search(t"$group/${media.s.substring(plus + 1).nn}")

  // The media types a body may take, in order of preference: `application/json` first, then by
  // name
  private def medias(content: Map[Text, OpenApi.MediaTypeObject]): List[Text] =
    val names = content.keys.to[List].map(normalise).order(_.s)

    if names.has(t"application/json")
    then List.concat(List(t"application/json"), names.filter(_ != t"application/json"))
    else names

  // The media type of a body among those the specification offers: the first for which a
  // `Construable` is in scope, else the first offered, which nothing in scope construes
  private def chosenMedia(using Quotes)(content: Map[Text, OpenApi.MediaTypeObject])
  :   Optional[Text] =

    val names = medias(content)
    names.seek(construable(_).present).or(names.prim)

  // A media type the spec names, as a `MediaType` value in the generated code, if it parses
  private def mediaTypeExpr(using Quotes)(media: Text): Optional[Expr[MediaType]] =
    try
      Media.parse(media)
      Optional('{unsafely[MediaType.Error](Media.parse(${Expr(media.s)}.tt))})
    catch case error: MediaType.Error => Unset

  // The type a response outside the success range is construed as: the carrier of the media
  // type of the operation's `default` response, else its `4XX`/`5XX` or lowest-numbered error
  // response with a body; the body as `Text` where none is declared or construable. (Not the
  // raw `Http.Response`: it is a capability, which an error's payload cannot be.)
  private def failureTransport(using quotes: Quotes)(doc: OpenApi, operation: OpenApi.Operation)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val statuses = operation.responses.keys.filter(!_.starts(t"2")).to[List]

    val first = List(t"default", t"4XX", t"5XX").filter(statuses.has(_))
    val ordered = List.concat(first, statuses.filter(!first.has(_)).order(_.s))

    val contents = ordered.bind: status =>
      operation.responses(status).let(resolve[OpenApi.Response](doc, _)).let(_.content)
      . lay(List[Map[Text, OpenApi.MediaTypeObject]]()): content =>
          if content.nil then List() else List(content)

    contents.prim.let(chosenMedia(_)).let(construable(_)).or(TypeRepr.of[Text])

  // The status of the response an operation's success returns: `200` or `201` where the
  // operation declares one, else its lowest-numbered 2xx.
  private def successStatus(operation: OpenApi.Operation): Optional[Text] =
    val statuses = operation.responses.keys.filter(_.starts(t"2")).to[List]

    if statuses.has(t"200") then t"200"
    else if statuses.has(t"201") then t"201"
    else statuses.order(_.s).prim

  // The content an operation's success response declares, if any
  private def responseContent(using Quotes)(doc: OpenApi, operation: OpenApi.Operation)
  :   Optional[Map[Text, OpenApi.MediaTypeObject]] =

    successStatus(operation).let(operation.responses(_)).let(resolve[OpenApi.Response](doc, _))
    . let(_.content)

  // The type an `Api.Response` construes its body as: the carrier of the response's media type;
  // the raw `Http.Response`, with a warning, when nothing in scope construes it; `Unit` when the
  // response has no body.
  private def responseTransport(using quotes: Quotes)
    ( doc: OpenApi, locus: Text, verb: Text, operation: OpenApi.Operation )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    responseContent(doc, operation).let(chosenMedia(_)).lay(TypeRepr.of[Unit]): media =>
      construable(media).or:
        report.warning
          (s"apoplexy: nothing in scope construes the $media response of $verb $locus, so " +
            "`call()` yields the raw `Http.Response`; import its entry from `construables` " +
            "(for example `construables.pngConstruable`) to read it as a value")

        TypeRepr.of[Http.Response]

  // The transport of the navigation types: the carrier every operation's success response has,
  // else `Json` as a neutral placeholder. The authoritative type is recomputed per operation by
  // `invoke`.
  private def uniformTransport(using quotes: Quotes)(doc: OpenApi): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    val names =
      doc.paths.values.flatMap(_.operations.values).to[List].bind: operation =>
        responseContent(doc, operation).let(medias(_).prim).lay(List[Text]())(List(_))

    names.to[Set].to[List] match
      case List(name) => construable(name).or(TypeRepr.of[Json])
      case _          => TypeRepr.of[Json]

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

    val found = pathItem(doc, locus).let { (key, item) => item.operations(method).let((key, _)) }

    val (key, operation) = found.or:
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

    val queryExpr: Expr[Query] = '{Query(${Lifts.list(queryEntries)})}

    val headersExpr: Expr[List[Http.Header]] =
      Lifts.list(headerEntries.map { entry => '{Http.Header($entry(0), $entry(1))} })

    val status = successStatus(operation).or(t"200")

    val response: Optional[OpenApi.Referable[OpenApi.Response]] = operation.responses(status)
    val transport = responseTransport(doc, locus, verb, operation)
    val requestBody = operation.requestBody.let(resolve[OpenApi.RequestBody](doc, _))

    // The `accept` header names the response media type the client construes
    val accept: Optional[Text] = responseContent(doc, operation).let(chosenMedia(_))

    val acceptExpr: Expr[Optional[MediaType]] = accept.let(mediaTypeExpr(_)) match
      case Unset                             => '{Unset}
      case media: Expr[MediaType] @unchecked => '{Optional($media)}

    val bodyExpr: Expr[Api.Body] = positional match
      case Nil =>
        if requestBody.let(_.required.or(false)).or(false)
        then halt(m"apoplexy: $verb $locus requires a request body")

        '{Api.Body.Empty}

      case List(argExpr) =>
        val media = requestBody.let { body => chosenMedia(body.content) }.or:
          halt(m"apoplexy: $verb $locus takes no request body")

        val carrierRepr = construable(media).or:
          val advice = t"import its entry from `construables`"
          val body = t"the $media request body of $verb $locus"
          halt(m"apoplexy: nothing in scope construes $body; $advice")

        val mediaExpr: Expr[MediaType] = mediaTypeExpr(media).or:
          halt(m"apoplexy: the request media type $media of $verb $locus is not a media type")

        val actual = argExpr.asTerm.tpe.widen

        carrierRepr.asType.absolve match
          case '[carrier] =>
            val postable = Expr.summon[carrier is Postable].getOrElse:
              val advice = t"import its entry from `postables`"
              val body = t"the ${carrierRepr.show} request body of $verb $locus"
              halt(m"apoplexy: no `Postable` for $body is in scope; $advice")

            if actual <:< carrierRepr then
              val value = argExpr.asExprOf[carrier]
              '{Api.Body.content[carrier]($mediaExpr, $value)(using $postable)}
            else
              actual.asType.absolve match
                case '[bodyType] =>
                  val value = argExpr.asExprOf[bodyType]

                  val encodable = Expr.summon[bodyType is Encodable in carrier].getOrElse:
                    halt(m"apoplexy: the request body cannot be encoded as ${carrierRepr.show}")

                  val encoded = '{$encodable.encoded($value)}
                  '{Api.Body.content[carrier]($mediaExpr, $encoded)(using $postable)}

      case _ =>
        halt(m"apoplexy: $verb $locus takes a single request body")

    val mediaContent = escape(accept.or(t"application/json"))

    // The schema's pointer: into the components when the response is a reference, else into
    // the operation
    val pointer = response match
      case OpenApi.Ref(reference) =>
        t"${reference.encode}/content/$mediaContent/schema"

      case _ =>
        t"#/paths/${escape(key)}/$verb/responses/$status/content/$mediaContent/schema"

    val mExpr = methodExpr(method)
    val locusExpr = Expr(key.s)

    val failure = failureTransport(doc, operation)

    val responseType =
      Refinement
        ( Refinement
            ( Refinement
                ( Refinement(TypeRepr.of[Api.Response], "Result", bounds(literalType(pointer))),
                  "Form",
                  bounds(literalType(source)) ),
              "Transport",
              bounds(transport) ),
          "Failure",
          bounds(failure) )

    responseType.asType.absolve match
      case '[type result <: Api.Response; result] =>
        ' {
            val request =
              $self.request.copy
                ( method  = $mExpr,
                  path    = $locusExpr.tt,
                  query   = $queryExpr,
                  body    = $bodyExpr,
                  headers = $headersExpr,
                  accept  = $acceptExpr )

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
      pathItem(doc, locus).lay(List[Http.Method]()): (_, item) =>
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
    pathItem(doc, locus).let(_(1).operations.defines(method)).or(false)

  // --- macros --------------------------------------------------------------

  def root(resource: Expr[Resource]): Macro[Api] = rootWith(resource, Unset)

  def rootAt(resource: Expr[Resource], base: Expr[HttpUrl]): Macro[Api] = rootWith(resource, base)

  // The base URL comes from the spec's first server, with its variables at their defaults,
  // checked as a URL when the code compiles. A server URL which is relative (`/api/v3`), or
  // absent, needs a base from the caller, which it then extends; a caller's base replaces an
  // absolute server URL outright.
  private def rootWith(using Quotes)(resource: Expr[Resource], base: Optional[Expr[HttpUrl]])
  :   Expr[Api] =

    import quotes.reflect.*

    val members = (refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)).to(Map)

    val source =
      members(t"Locus").lay(halt(m"apoplexy: the resource has no `Locus` path"))(stringOf(_))

    val doc = spec(source)
    val server = doc.servers.prim.lay(t"")(_.resolved)
    val serverExpr = Expr(server.s)

    // A plain match: a quote inside an inline argument (`lay`'s lambda) crashes the pickler
    val baseExpr: Expr[HttpUrl] = base match
      case Unset =>
        if server == t"" then halt(m"apoplexy: the spec declares no server; pass a `base` URL")

        if server.starts(t"/")
        then halt(m"apoplexy: the spec's server URL $server is relative; pass a `base` URL")

        try server.as[HttpUrl]
        catch case error: Url.Error => halt(m"apoplexy: the spec's server URL $server is not valid")

        '{unsafely[Url.Error]($serverExpr.tt.as[HttpUrl])}

      case supplied: Expr[HttpUrl] @unchecked =>
        if server.starts(t"/") then '{Api.extend($supplied, $serverExpr.tt)} else supplied

    val transport = uniformTransport(doc)

    apiType(t"/", source, transport).asType.absolve match
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
    ( self:   Expr[Api],
      source: Text,
      doc:    OpenApi,
      locus:  Text,
      name:   Text,
      wire:   quotes.reflect.TypeRepr )
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
      wire:       quotes.reflect.TypeRepr )
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

        if pathItem(doc, newLocus).absent
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

  // The raw JSON of the response schema at a JSON pointer into the spec
  private def schemaNode(using Quotes)(source: Text, pointer: Text): Json =
    val segments = pointer.cut(t"/").skip(1)

    segments.fold(specJson(source)): (node, segment) =>
      try node(segment.sub(t"~1", t"/").sub(t"~0", t"~"))
      catch case error: Exception => halt(m"apoplexy: could not resolve the schema at $pointer")

  // Resolve the response-schema `JsonSchema` at a JSON-pointer into the spec.
  private def resolveSchema(using Quotes)(source: Text, pointer: Text): JsonSchema =
    try schemaNode(source, pointer).as[JsonSchema]
    catch case error: Exception => halt(m"apoplexy: the response schema at $pointer is not valid")

  // The response's schema pointer and spec source, from its refinements, once the response is
  // known to be JSON
  private def jsonResponse(using Quotes)(self: Expr[Api.Response], what: Text): (Text, Text) =
    import quotes.reflect.*

    val members = (refinements(self.asTerm.tpe) ++ refinements(self.asTerm.tpe.widen)).to(Map)

    val transport = members(t"Transport").or:
      halt(m"apoplexy: $what needs a response whose transport is known")

    if !(transport =:= TypeRepr.of[Json]) then
      val shown = transport.show
      halt(m"apoplexy: $what reads a JSON response, but this response is construed as $shown")

    val pointer = members(t"Result").lay(halt(m"apoplexy: missing response schema pointer"))(stringOf(_))
    val source = members(t"Form").lay(halt(m"apoplexy: missing spec source"))(stringOf(_))

    (pointer, source)

  // The polyvinyl member the response schema describes, which must be an object or an array
  // of objects; its `$ref`s resolve against the whole spec
  private def responseMember(using Quotes)(source: Text, pointer: Text, what: Text)
  :   (List[(Text, Member)], Boolean) =

    Json.Provider.memberOf(specJson(source), schemaNode(source, pointer)) match
      case Member.Record(fields, Multiplicity.One)  => (fields, false)
      case Member.Record(fields, Multiplicity.Many) => (fields, true)

      case _ =>
        val advice = t"use `call[T]()` for this response"
        halt(m"apoplexy: $what needs a schema describing an object or an array of objects; $advice")

  // The inline entry points called from `Api.Response.record()`/`tuple()`, which supply the
  // response body already read as JSON, with the givens the send needs bound at the call site
  transparent inline def record(inline self: Api.Response, json: Json): Any =
    ${recordMacro('self, 'json)}

  transparent inline def tuple(inline self: Api.Response, json: Json): Any =
    ${tupleMacro('self, 'json)}

  // `Api.Response.record()`: the response, read as JSON by `call`, becomes a `Record` refined
  // with the schema's properties, built over `Api.Records` with the expansion polyvinyl makes
  // of the schema's fields
  def recordMacro(self: Expr[Api.Response], json: Expr[Json]): Macro[Any] =
    import quotes.reflect.*

    val (pointer, source) = jsonResponse(self, t"record()")
    val (fields, many) = responseMember(source, pointer, t"record()")
    val target = '{Api.Records}
    val (refined, transform) = Specification.recordExpansion[Json, Json.Provider](target, fields)

    refined.absolve match
      case '[type refined <: Record; refined] =>
        if many then '{Api.Records.list($json, $transform).asInstanceOf[List[refined]]}
        else '{Api.Records.build($json, $transform).asInstanceOf[refined]}

  // `Api.Response.tuple()`: as `record()`, as a named tuple read eagerly
  def tupleMacro(self: Expr[Api.Response], json: Expr[Json]): Macro[Any] =
    import quotes.reflect.*

    val (pointer, source) = jsonResponse(self, t"tuple()")
    val (fields, many) = responseMember(source, pointer, t"tuple()")
    val target = '{Api.Records}
    val (tuple, make) = Specification.tupleExpansion[Json, Json.Provider](target, fields)

    tuple.absolve match
      case '[type tuple <: NamedTuple.AnyNamedTuple; tuple] =>
        if many then '{Api.Records.repeated(t"", $json).map($make(_)).asInstanceOf[List[tuple]]}
        else '{$make($json).asInstanceOf[tuple]}

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

    val members = (refinements(self.asTerm.tpe) ++ refinements(self.asTerm.tpe.widen)).to(Map)
    val transport = members(t"Transport")

    // The raw response, the carrier itself and `Unit` bypass the schema check
    val raw =
      valueRepr =:= TypeRepr.of[Http.Response] ||
        valueRepr =:= TypeRepr.of[Unit] ||
        transport.let(valueRepr =:= _).or(false)

    if !raw then

      val pointer =
        members(t"Result").lay(halt(m"apoplexy: missing response schema pointer"))(stringOf(_))

      val source =
        members(t"Form").lay(halt(m"apoplexy: missing spec source"))(stringOf(_))

      conformsTo(valueRepr, resolveSchema(source, pointer))

    '{()}
