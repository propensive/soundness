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
import orthodoxy.*
import polyvinyl.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import xylophone.*
import zephyrine.Parse

import codepages.utf8Codepage
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

  // --- security ----------------------------------------------------------------

  // The credential in scope for a security scheme, by the scheme's name, and the type of its
  // value
  private def credentialFor(using quotes: Quotes)(scheme: Text)
  :   Optional[(Expr[Any], quotes.reflect.TypeRepr)] =

    import quotes.reflect.*

    val target = Refinement(TypeRepr.of[Credential], "Self", bounds(literalType(scheme)))

    Implicits.search(target) match
      case success: ImplicitSearchSuccess =>
        refinements(success.tree.tpe.widen).get(t"Result") match
          case Some(result) => (success.tree.asExpr, result)
          case None         => Unset

      case _ =>
        Unset

  // The header, or query parameter, presenting one credential, for one scheme of a requirement;
  // `Unset` when no credential of a fitting type is in scope
  private def presentation(using quotes: Quotes)
    ( doc: OpenApi, verb: Text, key: Text, scheme: Text, scopes: List[Text] )
  :   Optional[Either[Expr[Http.Header], Expr[(Text, Text)]]] =

    import quotes.reflect.*

    val definition = doc.components.let(_.securitySchemes(scheme)).or:
      halt(m"apoplexy: $verb $key requires the security scheme $scheme, which the spec does not define")

    credentialFor(scheme) match
      case Unset => Unset

      case (credential: Expr[Any] @unchecked, result: TypeRepr @unchecked) =>
        def text: Boolean = result <:< TypeRepr.of[Text]
        def auth: Boolean = result <:< TypeRepr.of[Auth]
        def token: Boolean = result <:< TypeRepr.of[Authorization]

        definition.kind match
          case OpenApi.SecurityScheme.Kind.ApiKey if text =>
            val name = Expr(definition.name.or(scheme).s)
            val typed = '{$credential.asInstanceOf[Credential { type Result = Text }]}

            definition.`in`.or(t"header") match
              case t"query"  => Right('{($name.tt, $typed.value)})
              case t"cookie" => Left('{Api.cookieKey($typed, $name.tt)})
              case _         => Left('{Api.apiKey($typed, $name.tt)})

          case OpenApi.SecurityScheme.Kind.Http | OpenApi.SecurityScheme.Kind.OAuth2
              | OpenApi.SecurityScheme.Kind.OpenIdConnect if auth =>
            Left('{Api.httpAuth($credential.asInstanceOf[Credential { type Result <: Auth }])})

          case OpenApi.SecurityScheme.Kind.Http | OpenApi.SecurityScheme.Kind.OAuth2
              | OpenApi.SecurityScheme.Kind.OpenIdConnect if token =>
            val typed = '{$credential.asInstanceOf[Credential { type Result = Authorization }]}
            val strings: Expr[scala.collection.immutable.List[String]] =
              Expr(scopes.map(_.s).stdlib)

            val scopesExpr: Expr[List[Text]] = '{List.from($strings).map(_.tt)}

            val tactic = Expr.summon[Tactic[OAuth.Error]].getOrElse:
              val advice = t"a `Tactic[OAuth.Error]` is needed, for a token lacking a scope"
              halt(m"apoplexy: $verb $key requires the scopes ${scopes.join(t", ")} of $scheme; $advice")

            val diagnostics = Expr.summon[Diagnostics].getOrElse:
              halt(m"apoplexy: a `Diagnostics` is needed where an API is called")

            Left('{Api.tokenAuth($typed, $scopesExpr)(using $tactic, $diagnostics)})

          case OpenApi.SecurityScheme.Kind.MutualTls =>
            halt(m"apoplexy: $verb $key requires $scheme, a mutual-TLS scheme, which is not supported")

          case kind =>
            val shown = result.show
            val kindName = kind.toString.tt
            val advice = t"an API key is a `Credential to Text`, HTTP authentication a `Credential to Auth`, a token a `Credential to Authorization`"
            halt(m"apoplexy: the credential for $scheme (a $kindName scheme) has the type $shown; $advice")

  private type Presentation = Either[Expr[Http.Header], Expr[(Text, Text)]]
  private type Presentations = scala.collection.immutable.List[Presentation]

  // The presentations of every scheme of one requirement, when each has a credential in scope;
  // `Unset` otherwise. Explicit recursion over the standard library's list, typed at each step:
  // lambdas over `Optional` results here trip the compiler's `wildApprox` assertion.
  private def satisfy(using Quotes)
    ( doc:         OpenApi,
      verb:        Text,
      key:         Text,
      requirement: OpenApi.Requirement,
      schemes:     scala.collection.immutable.List[Text] )
  :   Optional[Presentations] =

    schemes match
      case scala.collection.immutable.Nil => scala.collection.immutable.Nil

      case scala.collection.immutable.::(scheme, rest) =>
        val scopes: List[Text] = requirement(scheme).or(Nil)
        val found: Optional[Presentation] = presentation(doc, verb, key, scheme, scopes)

        found match
          case Unset => Unset

          case found: Presentation @unchecked =>
            val more: Optional[Presentations] = satisfy(doc, verb, key, requirement, rest)

            more match
              case Unset                          => Unset
              case more: Presentations @unchecked => scala.collection.immutable.::(found, more)

  // The first alternative every scheme of which is satisfied
  private def firstSatisfied(using Quotes)
    ( doc:          OpenApi,
      verb:         Text,
      key:          Text,
      alternatives: scala.collection.immutable.List[OpenApi.Requirement] )
  :   Optional[Presentations] =

    alternatives match
      case scala.collection.immutable.Nil => Unset

      case scala.collection.immutable.::(requirement, rest) =>
        val schemes: scala.collection.immutable.List[Text] =
          Map.keys(requirement).to[List].order(_.s).stdlib

        val found: Optional[Presentations] = satisfy(doc, verb, key, requirement, schemes)

        found match
          case Unset                           => firstSatisfied(doc, verb, key, rest)
          case found: Presentations @unchecked => found

  private def empty(requirement: OpenApi.Requirement): Boolean =
    Map.keys(requirement).to[List].stdlib.isEmpty

  // The presentations of the first requirement alternative every scheme of which has a
  // credential in scope; none when the operation requires no credentials (or offers an empty
  // alternative); a compile error, naming each alternative and the givens which would satisfy
  // it, when no alternative is met
  private def credentials(using Quotes)
    ( doc: OpenApi, verb: Text, key: Text, operation: OpenApi.Operation )
  :   Presentations =

    val alternatives: scala.collection.immutable.List[OpenApi.Requirement] =
      operation.security.or(doc.security).stdlib

    if alternatives.isEmpty || alternatives.exists(empty) then scala.collection.immutable.Nil
    else
      val found: Optional[Presentations] = firstSatisfied(doc, verb, key, alternatives)

      found match
        case Unset                           => unsatisfied(doc, verb, key, List.from(alternatives))
        case found: Presentations @unchecked => found

  private def describe(doc: OpenApi, scheme: Text): Text =
    val kind = doc.components.let(_.securitySchemes(scheme)).let(_.kind)

    val result = kind.or(OpenApi.SecurityScheme.Kind.Unknown) match
      case OpenApi.SecurityScheme.Kind.ApiKey => t"Text"
      case OpenApi.SecurityScheme.Kind.Http   => t"Auth"
      case _                                  => t"Authorization"

    t"""("$scheme" is Credential to $result)"""

  private def describeAll(doc: OpenApi, schemes: scala.collection.immutable.List[Text])
  :   scala.collection.immutable.List[Text] =

    schemes match
      case scala.collection.immutable.Nil               => scala.collection.immutable.Nil
      case scala.collection.immutable.::(scheme, rest)  =>
        scala.collection.immutable.::(describe(doc, scheme), describeAll(doc, rest))

  private def unsatisfied(using Quotes)
    ( doc: OpenApi, verb: Text, key: Text, alternatives: List[OpenApi.Requirement] )
  :   Nothing =

    val described: List[Text] = alternatives.map: requirement =>
      val schemes = Map.keys(requirement).to[List].order(_.s).stdlib
      List.from(describeAll(doc, schemes)).join(t" and ")

    val listed = described.join(t"; or ")
    halt(m"apoplexy: $verb $key requires credentials; provide a given for $listed")

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

  // How a declared error response is raised: by the class of its status, a range class for
  // `4XX`-style keys (and for a numbered status telekinesis does not name), or `OtherError` for
  // `default`; and what its payload is
  private enum Payload:
    case Empty
    case Carrier(name: Text)
    case Record(fields: List[(Text, Member)], many: Boolean)

  private case class Failure(key: Text, status: Optional[Http.Status], payload: Payload):
    def range: Boolean = status.absent && key != t"default"

  private def statusOf(key: Text): Optional[Http.Status] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(Http.Status.values)
    . find(_.code.toString == key.s) match
      case Some(status) => status
      case None         => Unset

  // The declared error responses of an operation, each with the payload its body construes: a
  // record for a JSON object schema, the carrier of its media type otherwise, `Text` where
  // nothing construes the media type, nothing where the response has no body
  private def failures(using Quotes)
    ( doc: OpenApi, source: Text, key: Text, verb: Text, operation: OpenApi.Operation )
  :   List[Failure] =

    val keys = operation.responses.keys.filter(!_.starts(t"2")).to[List].order(_.s)

    // Hoisted, explicitly typed: `t""` inside nested `lay` lambdas with an inferred enum result
    // trips the compiler's `wildApprox` assertion
    def schemaPointer(status: Text, referable: OpenApi.Referable[OpenApi.Response], media: Text)
    :   Text =

      referable match
        case OpenApi.Ref(reference) =>
          t"${reference.encode}/content/${escape(media)}/schema"

        case _ =>
          t"#/paths/${escape(key)}/$verb/responses/$status/content/${escape(media)}/schema"

    def recordPayload(status: Text, referable: OpenApi.Referable[OpenApi.Response], media: Text)
    :   Payload =

      val pointer: Text = schemaPointer(status, referable, media)

      // An error payload's record follows references two levels deep; beyond, raw `Json`
      Json.Provider.memberOf(specJson(source), schemaNode(source, pointer), 2) match
        case Member.Record(fields, Multiplicity.One)  => Payload.Record(relaxed(fields), false)
        case Member.Record(fields, Multiplicity.Many) => Payload.Record(relaxed(fields), true)
        case _                                        => Payload.Carrier(media)

    def payloadOf(status: Text, referable: OpenApi.Referable[OpenApi.Response]): Payload =
      val response: OpenApi.Response = resolve[OpenApi.Response](doc, referable)
      val content: Map[Text, OpenApi.MediaTypeObject] = response.content
      val media: Optional[Text] = chosenMedia(content)

      media match
        case Unset       => Payload.Empty
        case media: Text => construable(media) match
          case Unset => Payload.Carrier(t"text/plain")

          case repr: quotes.reflect.TypeRepr @unchecked =>
            if repr =:= quotes.reflect.TypeRepr.of[Json] then recordPayload(status, referable, media)
            else Payload.Carrier(media)

    keys.map: status =>
      val referable: Optional[OpenApi.Referable[OpenApi.Response]] = operation.responses(status)

      val payload: Payload = referable match
        case Unset                                                     => Payload.Empty
        case referable: OpenApi.Referable[OpenApi.Response] @unchecked => payloadOf(status, referable)

      Failure(status, statusOf(status), payload)

  // An error payload is read for diagnosis, not validated: its constrained members (an `enum`, a
  // `pattern`, a bounded number) read as their plain types, so that no member of the record is
  // fallible — a fallible member would make the error type a capability, which it cannot be
  private def relax(member: Member): Member = member match
    case Member.Value(label, _, multiplicity) =>
      val plain = label.s match
        case "string!" | "enum" | "pattern" => t"string"
        case "integer!"                     => t"integer"
        case "number!"                      => t"number"
        case other                          => other.tt

      Member.Value(plain, Nil, multiplicity)

    case Member.Record(fields, multiplicity) => Member.Record(relaxed(fields), multiplicity)
    case Member.Union(alternatives, multiplicity) =>
      Member.Union(alternatives.map { (kind, member) => (kind, relax(member)) }, multiplicity)

  private def relaxed(fields: List[(Text, Member)]): List[(Text, Member)] =
    fields.map { (name, member) => (name, relax(member)) }

  // The error class a declared response raises, applied to its payload type
  private def failureType(using quotes: Quotes)(failure: Failure): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    val payload: TypeRepr = failure.payload match
      case Payload.Empty          => TypeRepr.of[Unit]
      case Payload.Carrier(media) => construable(media).or(TypeRepr.of[Text])

      case Payload.Record(fields, many) =>
        val (refined, _) =
          Specification.recordExpansion[Json, Json.Provider]('{Api.Records}, fields)

        refined.absolve match
          case '[refined] =>
            if many then TypeRepr.of[List[refined]] else TypeRepr.of[refined]

    val errorClass: TypeRepr = failure.status match
      case status: Http.Status =>
        val name = status.toString
        Symbol.requiredClass(s"apoplexy.Api.$name").typeRef

      case _ =>
        val name =
          if failure.key == t"default" then "OtherError"
          else if failure.key.starts(t"1") then "Informational"
          else if failure.key.starts(t"3") then "Redirection"
          else if failure.key.starts(t"4") then "ClientError"
          else "ServerError"

        Symbol.requiredClass(s"apoplexy.Api.$name").typeRef

    errorClass.appliedTo(payload)

  // The union of an operation's declared error types, `Nothing` where it declares none
  private def failureTransport(using quotes: Quotes)(failures: List[Failure])
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*
    val types: List[TypeRepr] = failures.map(failureType(_))
    types.fold[TypeRepr](TypeRepr.of[Nothing]) { (left, right) => OrType(left, right) }

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

    val presented: Presentations = credentials(doc, verb, key, operation)

    val credentialHeaders: List[Expr[Http.Header]] =
      List.from(presented.collect { case Left(header) => header })

    val credentialQueries: List[Expr[(Text, Text)]] =
      List.from(presented.collect { case Right(entry) => entry })

    val queryExpr: Expr[Query] =
      '{Query(${Lifts.list(List.concat(queryEntries, credentialQueries))})}

    val paramHeaders: List[Expr[Http.Header]] =
      headerEntries.map { entry => '{Http.Header($entry(0), $entry(1))} }

    val headersExpr: Expr[List[Http.Header]] =
      Lifts.list(List.concat(paramHeaders, credentialHeaders))

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

    val failure = failureTransport(failures(doc, source, key, verb, operation))

    val responseType =
      List
        ( ("Result", bounds(literalType(pointer))),
          ("Form", bounds(literalType(source))),
          ("Locus", bounds(literalType(key))),
          ("Verb", bounds(literalType(verb))),
          ("Transport", bounds(transport)),
          ("Failure", bounds(failure)) )
      . fold[TypeRepr](TypeRepr.of[Api.Response]): (parent, member) =>
          Refinement(parent, member(0), member(1))

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

  // The cases of the dispatch, for the exactly-declared statuses and for the declared ranges.
  // Object-level recursion over the standard library's list: a lambda building a quote under a
  // `map`'s live type variables trips the compiler's `wildApprox` assertion.
  private def statusCases(using Quotes)
    ( failures: scala.collection.immutable.List[Failure],
      code0:    Expr[Int],
      raise:    Failure => Expr[Nothing] )
  :   scala.collection.immutable.List[(Expr[Boolean], Expr[Nothing])] =

    failures match
      case scala.collection.immutable.Nil => scala.collection.immutable.Nil

      case scala.collection.immutable.::(failure, rest) =>
        val code: Int = failure.status match
          case status: Http.Status => status.code
          case _                   => 0

        val expected: Expr[Int] = Expr(code)
        val test: Expr[Boolean] = '{$code0 == $expected}
        val body: Expr[Nothing] = raise(failure)
        scala.collection.immutable.::((test, body), statusCases(rest, code0, raise))

  private def rangeCasesOf(using Quotes)
    ( failures: scala.collection.immutable.List[Failure],
      code0:    Expr[Int],
      raise:    Failure => Expr[Nothing] )
  :   scala.collection.immutable.List[(Expr[Boolean], Expr[Nothing])] =

    failures match
      case scala.collection.immutable.Nil => scala.collection.immutable.Nil

      case scala.collection.immutable.::(failure, rest) =>
        val digit: Int = failure.key.s.charAt(0).toInt - '0'.toInt
        val range: Expr[Int] = Expr(digit)
        val test: Expr[Boolean] = '{$code0 / 100 == $range}
        val body: Expr[Nothing] = raise(failure)
        scala.collection.immutable.::((test, body), rangeCasesOf(rest, code0, raise))

  // A chain of `if`s over the cases, ending in `otherwise`
  private def cascade(using Quotes)
    ( cases:     scala.collection.immutable.List[(Expr[Boolean], Expr[Nothing])],
      otherwise: Expr[Nothing] )
  :   Expr[Nothing] =

    cases match
      case scala.collection.immutable.Nil => otherwise

      case scala.collection.immutable.::((test, body), rest) =>
        val tail: Expr[Nothing] = cascade(rest, otherwise)
        '{if $test then $body else $tail}

  // The inline entry point called from `Api.Response.ensure`
  inline def ensure(inline self: Api.Response, response: Http.Response): Unit =
    ${ensureMacro('self, 'response)}

  // Raises the declared error for a response outside the success range. Each declared error
  // type needs a `Tactic` where the call is written — as does `Api.Violation`, for a status the
  // specification does not declare — which is what makes handling exhaustive: a handler missing
  // one is a compile error naming it.
  def ensureMacro(self: Expr[Api.Response], response: Expr[Http.Response]): Macro[Unit] =
    import quotes.reflect.*

    val members = (refinements(self.asTerm.tpe) ++ refinements(self.asTerm.tpe.widen)).to(Map)

    def member(name: Text): Text =
      members(name).lay(halt(m"apoplexy: the response has no `$name` member"))(stringOf(_))

    val source = member(t"Form")
    val key = member(t"Locus")
    val verb = member(t"Verb")
    val doc = spec(source)

    val operation =
      pathItem(doc, key).let(_(1)).let(_.operations(verbs(verb).or(Http.Get))).or:
        halt(m"apoplexy: $key defines no $verb operation")

    val declared = failures(doc, source, key, verb, operation)

    // A `Tactic` for an error type, summoned where the call is written. `Tactic` is
    // contravariant, so one for the whole `Failure` union (an `attempt`'s) serves each member.
    def tacticFor(repr: TypeRepr): Expr[Tactic[Nothing]] =
      Implicits.search(TypeRepr.of[Tactic].appliedTo(repr)) match
        case success: ImplicitSearchSuccess => success.tree.asExprOf[Tactic[Nothing]]

        case _ =>
          val shown = repr.show
          val advice = t"a `Tactic[$shown]` is needed"
          halt(m"apoplexy: $verb $key may respond with $shown, which nothing handles here; $advice")

    val diagnostics = Expr.summon[Diagnostics].getOrElse:
      halt(m"apoplexy: a `Diagnostics` is needed where an API is called")

    // The payload of a failure, read from the response
    def payloadExpr(failure: Failure): Expr[Any] = failure.payload match
      case Payload.Empty => '{()}

      case Payload.Carrier(media) =>
        construable(media).or(TypeRepr.of[Text]).asType.absolve match
          case '[carrier] =>
            val conformant = Expr.summon[(carrier is Conformant) over carrier].getOrElse:
              val shown = Type.show[carrier]
              halt(m"apoplexy: the ${failure.key} response of $verb $key cannot be read as $shown")

            '{$conformant.read($response)}

      case Payload.Record(fields, many) =>
        val (refined, transform) =
          Specification.recordExpansion[Json, Json.Provider]('{Api.Records}, fields)

        val parse = Expr.summon[Tactic[Parse.Error]].getOrElse:
          val advice = t"a `Tactic[Parse.Error]` is needed"
          halt(m"apoplexy: reading the ${failure.key} response of $verb $key; $advice")

        refined.absolve match
          case '[type refined <: Record; refined] =>
            val json = '{Api.jsonOf($response)(using $parse)}

            if many then '{Api.Records.list($json, $transform).asInstanceOf[List[refined]]}
            else '{Api.Records.build($json, $transform).asInstanceOf[refined]}

    // The raise of one declared failure: the error constructed by class, with its payload and
    // (for a range or `default` class) the status, through its own tactic
    def raise(failure: Failure): Expr[Nothing] =
      val errorType = failureType(failure)
      val tactic = tacticFor(errorType)
      val payloadType = errorType.typeArgs.head
      val constructor = errorType.typeSymbol.primaryConstructor

      val arguments: List[Term] = failure.status match
        case status: Http.Status => List(payloadExpr(failure).asTerm)
        case _                   => List('{$response.status}.asTerm, payloadExpr(failure).asTerm)

      val construction =
        New(Inferred(errorType)).select(constructor).appliedToType(payloadType)
        . appliedToArgs(arguments.stdlib).appliedTo(diagnostics.asTerm)

      errorType.asType.absolve match
        case '[error] =>
          val errorExpr = '{${construction.asExpr}.asInstanceOf[error & Hazard]}
          '{$tactic.asInstanceOf[Tactic[error & Hazard]].abort($errorExpr)}

    val status0: Expr[Http.Status] = '{$response.status}
    val code0: Expr[Int] = '{$response.status.code}

    val violationTactic = Expr.summon[Tactic[Api.Violation]].getOrElse:
      halt(m"apoplexy: a `Tactic[Api.Violation]` is needed, for a status $verb $key does not declare")

    val violation: Expr[Nothing] =
      '{$violationTactic.abort(Api.Violation($status0, Api.dataOf($response))(using $diagnostics))}

    val undeclared: Expr[Nothing] = declared.seek(_.key == t"default") match
      case Unset            => violation
      case failure: Failure => raise(failure)

    // The dispatch on the status: each exactly-declared status, then each declared range, then
    // `default`, else a violation. The cases are built on the standard library's list and
    // cascaded by an object-level method: a nested recursive method over the opaque `List`, or
    // a quote inside a fold's lambda, trips the compiler's `wildApprox` assertion.
    val exactCases = statusCases(declared.filter(_.status.present).stdlib, code0, raise)
    val rangeCases = rangeCasesOf(declared.filter(_.range).stdlib, code0, raise)
    val dispatch: Expr[Nothing] = cascade(exactCases ++ rangeCases, undeclared)

    ' {
        if $status0.category != Http.Status.Category.Successful then $dispatch
      }

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
