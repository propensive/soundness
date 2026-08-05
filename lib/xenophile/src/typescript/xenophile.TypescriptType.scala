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
import gossamer.*
import rudiments.*
import vacuous.*

object TypescriptType:
  // A literal type's kind, kept apart from its text because `"1"` and `1` are different types
  // whose source forms differ only in quoting, which the lexer has already removed.
  enum LiteralKind:
    case String, Number, Boolean

  // A type parameter's binder: its bound (`T extends U`) and its default (`T = U`). The name is
  // carried for diagnostics and for resolving references within the binder's scope; a canonical
  // encoding is expected to replace it with a positional index.
  case class Parameter(name: Text, bound: Optional[TypescriptType], default: Optional[TypescriptType])

  // A value parameter of a function, method or constructor. `rest` marks `...args: T[]`, whose
  // arity is unbounded, and `optional` marks `a?: T`, which callers may omit — both change what
  // call sites are legal, so both are part of the contract.
  case class Argument
    ( name:     Text,
      typed:    Optional[TypescriptType],
      optional: Boolean = false,
      rest:     Boolean = false )

// A TypeScript type expression.
//
// The vocabulary is deliberately closed: `TypescriptParser` raises rather than guessing when it
// meets a construct not represented here, so a `.d.ts` file using one is rejected loudly instead
// of being read as a smaller interface than it declares. That property is what lets a discipline
// atomize these values at all (LIRA §11.2 requirement 3).
enum TypescriptType:
  case Named(name: Text, arguments: List[TypescriptType] = Nil)
  case Literal(value: Text, kind: TypescriptType.LiteralKind)
  case Union(members: List[TypescriptType])
  case Intersection(members: List[TypescriptType])
  case Tuple(members: List[TypescriptType], names: List[Optional[Text]] = Nil)
  case Array(element: TypescriptType)
  case Object(members: List[TypescriptMember])
  case Keyof(target: TypescriptType)
  case Typeof(target: Text)
  case Indexed(target: TypescriptType, index: TypescriptType)
  case Predicate(parameter: Text, target: TypescriptType)

  case Function
    ( parameters: List[TypescriptType.Argument],
      result:     TypescriptType,
      typed:      List[TypescriptType.Parameter] = Nil,
      construct:  Boolean = false )

  // A stable rendering, used in diagnostics. It is *not* the canonical encoding: the encoding a
  // discipline hashes is structural and binder-name-free, and lives with the discipline.
  //
  // Rendering runs over the `stdlib` view rather than the opaque collections' `map`/`join`. An
  // extension method applied under a still-uninstantiated type variable is the shape that trips
  // the compiler's `wildApprox` assertion (scala/scala3#24824), and a recursive renderer over a
  // generic collection reaches it reliably.
  def text: Text =
    def render(types: List[TypescriptType], separator: String): String =
      types.stdlib.map { typed => typed.text.s }.mkString(separator)

    val rendered: String = this match
      case Named(name, Nil)       => name.s
      case Literal(value, _)      => value.s
      case Union(members)         => render(members, " | ")
      case Intersection(members)  => render(members, " & ")
      case Array(element)         => element.text.s+"[]"
      case Keyof(target)          => "keyof "+target.text.s
      case Typeof(target)         => "typeof "+target.s
      case Named(name, arguments) => name.s+"<"+render(arguments, ", ")+">"
      case Tuple(members, _)      => "["+render(members, ", ")+"]"
      case Indexed(target, index) => target.text.s+"["+index.text.s+"]"
      case Predicate(name, target) => name.s+" is "+target.text.s

      case Object(members) =>
        "{ "+members.stdlib.map { member => member.name.s }.mkString("; ")+" }"

      case Function(parameters, result, _, construct) =>
        val arguments = parameters.stdlib.map: parameter =>
          parameter.name.s+": "+parameter.typed.lay("any") { value => value.text.s }

        (if construct then "new " else "")+"("+arguments.mkString(", ")+") => "+result.text.s

    rendered.tt
