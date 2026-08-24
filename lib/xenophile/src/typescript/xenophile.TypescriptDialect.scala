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
package xenophile

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The `Dialect` adapter over `Typescript.Parser`: it projects the parsed declarations onto the
// `Foreign` model the foreign-function macro reads.
//
// The projection is lossy by design — `Foreign.Type` describes only what a call site needs to
// marshal — but the *parse* is not, which is the difference from the grammar this replaced.
// Generic interfaces and `extends` clauses used to be dropped whole, so a member declared on a
// base interface surfaced to the user as "has no member"; they are now resolved.
object TypescriptDialect extends Dialect:

  // `Dialect.parse` is total: it cannot report an error, and the macro that calls it reports
  // "the foreign type is not defined" from the empty result. A discipline computing a
  // compatibility claim must never take that path — it calls `Typescript.Parser.parse` directly,
  // where an unreadable declaration is an error rather than an absent contract.
  def parse(source: Text): Map[Text, Map[Text, Prototype]] =
    safely(project(Typescript.Parser.parse(source))).or(Map())

  private def project(declarations: List[Typescript.Declaration])
  :   Map[Text, Map[Text, Prototype]] =

    val byName = scala.collection.mutable.LinkedHashMap[Text, Typescript.Declaration]()

    declarations.stdlib.foreach: declaration =>
      declaration match
        case _: Typescript.Declaration.Interface => byName.put(declaration.key, declaration)
        case _: Typescript.Declaration.Class     => byName.put(declaration.key, declaration)
        case _                                  => ()

    // Inherited members are resolved against the declarations of this same file. A base named by
    // a declaration this file does not carry contributes nothing — the file is the whole world
    // the macro has — but it never removes what is declared here.
    def members(key: Text, seen: scala.collection.immutable.Set[Text])
    :   scala.collection.immutable.Map[Text, Prototype] =

      if seen.contains(key) then scala.collection.immutable.Map() else
        byName.get(key) match
          case scala.None => scala.collection.immutable.Map()

          case scala.Some(declaration) =>
            val bases = declaration match
              case Typescript.Declaration.Interface(_, _, _, extending, _, _) => extending.stdlib

              case Typescript.Declaration.Class(_, _, _, extending, implements, _, _, _) =>
                extending.option.toList ++ implements.stdlib

              case _ => scala.Nil

            val inherited = bases.foldLeft(scala.collection.immutable.Map[Text, Prototype]()):
              (accumulated, base) =>
                base match
                  case Typescript.Type.Named(name, _) => accumulated ++ members(name, seen + key)
                  case _                             => accumulated

            declaration.declaredMembers.stdlib.foldLeft(inherited): (accumulated, member) =>
              prototype(member).lay(accumulated): value =>
                accumulated.updated(member.name, value)

    (byName.keys.toList.map { key => key -> members(key, scala.collection.immutable.Set()).to(Map) }.toMap).to(Map)

  // Index, call and construct signatures have no name a `Foreign` member selection could use,
  // and a private member is not the consumer's to call.
  private def prototype(member: Typescript.Member): Optional[Prototype] =
    if !member.visible then Unset else member.kind match
      case Typescript.Member.Kind.Call | Typescript.Member.Kind.Construct
         | Typescript.Member.Kind.Index => Unset

      case Typescript.Member.Kind.Property | Typescript.Member.Kind.Getter =>
        member.signatures.stdlib.headOption.map: signature =>
          val result = signature match
            case Typescript.Type.Function(_, result, _, _) => foreign(result)
            case other                                    => foreign(other)

          Prototype(Unset, if member.optional then optional(result) else result)

        . getOrElse(Unset)

      case Typescript.Member.Kind.Method | Typescript.Member.Kind.Setter =>
        // The first declared signature wins where a member is overloaded: `Prototype` records one
        // arity, and TypeScript resolves against the signatures in order.
        member.signatures.stdlib.headOption.map: signature =>
          signature match
            case Typescript.Type.Function(parameters, result, _, _) =>
              val arguments = parameters.map: parameter =>
                val typed = parameter.typed.lay(Foreign.Type.Named(t"any"))(foreign(_))
                if parameter.optional then optional(typed) else typed

              Prototype(arguments, foreign(result))

            case other => Prototype(Unset, foreign(other))

        . getOrElse(Unset)

  private def optional(foreign: Foreign.Type): Foreign.Type =
    Foreign.Type.Union(List(foreign, Foreign.Type.Named(t"undefined")))

  // The projection onto `Foreign.Type`. Constructs the foreign model cannot express are rendered
  // to a named type carrying their source shape, so they remain distinguishable from one another
  // and from anything that *is* expressible — they simply will not marshal.
  private def foreign(typed: Typescript.Type): Foreign.Type = typed match
    case Typescript.Type.Named(t"null" | t"undefined", _) => Foreign.Type.Named(t"undefined")
    case Typescript.Type.Named(name, Nil)                 => Foreign.Type.Named(name)

    case Typescript.Type.Named(name, arguments) =>
      Foreign.Type.Applied(name, arguments.map(foreign(_)))

    case Typescript.Type.Array(element) =>
      Foreign.Type.Applied(t"Array", List(foreign(element)))

    case Typescript.Type.Union(members)     => Foreign.Type.Union(members.map(foreign(_)))
    case Typescript.Type.Literal(value, _)  => Foreign.Type.Named(value)
    case other                             => Foreign.Type.Named(other.text)
