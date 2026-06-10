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
package cataclysm

import anticipation.*
import contingency.*
import gossamer.*
import nomenclature.*
import spectacular.*
import vacuous.*

// The structural model of a CSS selector, following Selectors Level 4. The
// three nesting levels are: a `SelectorList` (comma-separated, the loosest), a
// `Selector` (a complex selector — compounds joined by combinators), and a
// `Compound` (simple selectors with no whitespace between them, the tightest).

object SelectorList:
  given showable: SelectorList is Showable = _.selectors.map(_.show).join(t", ")

  // A non-raising parse of already-validated selector text, used by the `css"…"`
  // interpolator to rebuild a rule's selector at runtime.
  def read(text: Text): SelectorList = unsafely(SelectorParser.parse(text))

case class SelectorList(selectors: List[Selector]) derives CanEqual

object Selector:
  given showable: Selector is Showable = selector =>
    val lead = selector.lead.lay(t""):
      case Combinator.Descendant => t""
      case other                 => t"${other.show} "

    val rest = selector.rest.map: (combinator, compound) =>
      combinator match
        case Combinator.Descendant => t" ${compound.show}"
        case other                 => t" ${other.show} ${compound.show}"

    t"$lead${selector.head.show}${rest.join}"

// A complex selector: a head compound followed by combinator/compound steps.
// `lead` is set only for a relative selector (e.g. the `>` in `:has(> img)`).
case class Selector(lead: Optional[Combinator], head: Compound, rest: List[(Combinator, Compound)])
derives CanEqual

object Compound:
  given showable: Compound is Showable = _.parts.map(_.show).join

// A compound selector: a run of simple selectors bound together with no
// combinator (hence no whitespace) between them.
case class Compound(parts: List[Simple]) derives CanEqual

// The right-hand side of an attribute selector, e.g. the `^= "x" i` part.
case class AttributeTest(matcher: AttributeMatcher, value: Text, modifier: Optional[Char])
derives CanEqual

object Combinator:
  given showable: Combinator is Showable =
    case Descendant        => t" "
    case Child             => t">"
    case NextSibling       => t"+"
    case SubsequentSibling => t"~"
    case Column            => t"||"

enum Combinator derives CanEqual:
  case Descendant         //
  case Child              // >
  case NextSibling        // +
  case SubsequentSibling  // ~
  case Column             // ||

enum AttributeMatcher derives CanEqual:
  case Exact      // =
  case Includes   // ~=
  case DashMatch  // |=
  case Prefix     // ^=
  case Suffix     // $=
  case Substring  // *=

enum Namespace derives CanEqual:
  case Any                    // *|
  case Default                // |
  case Named(prefix: Text)    // ns|

object Simple:
  given showable: Simple is Showable =
    case Universal(namespace)          => t"${prefix(namespace)}*"
    case Type(namespace, name)         => t"${prefix(namespace)}$name"
    case Id(name)                      => t"#$name"
    case Class(name)                   => t".$name"
    case Nesting                       => t"&"
    case Attribute(namespace, name, t) => t"[${prefix(namespace)}$name${attributeTest(t)}]"
    case PseudoClass(name, argument)   => t":$name${pseudoArgument(argument)}"
    case PseudoElement(name, argument) => t"::$name${pseudoArgument(argument)}"

  // Render the optional namespace prefix of a type, universal or attribute selector.
  private def prefix(namespace: Optional[Namespace]): Text = namespace.lay(t""):
    case Namespace.Any         => t"*|"
    case Namespace.Default     => t"|"
    case Namespace.Named(name) => t"$name|"

  private def attributeTest(test: Optional[AttributeTest]): Text = test.lay(t""): test =>
    val modifier = test.modifier.lay(t""): char => t" ${char.show}"
    t"${matcherSymbol(test.matcher)}${test.value}$modifier"

  private def matcherSymbol(matcher: AttributeMatcher): Text = matcher match
    case AttributeMatcher.Exact     => t"="
    case AttributeMatcher.Includes  => t"~="
    case AttributeMatcher.DashMatch => t"|="
    case AttributeMatcher.Prefix    => t"^="
    case AttributeMatcher.Suffix    => t"$$="
    case AttributeMatcher.Substring => t"*="

  private def pseudoArgument(argument: Optional[PseudoArgument]): Text = argument.lay(t""):
    case PseudoArgument.Selectors(list) => t"(${list.show})"
    case PseudoArgument.Nth(a, b, of)   => t"(${nth(a, b)}${ofClause(of)})"
    case PseudoArgument.Raw(text)       => t"($text)"

  private def ofClause(of: Optional[SelectorList]): Text = of.lay(t""): list =>
    t" of ${list.show}"

  // Render an `An+B` micro-syntax, canonicalising `1n`→`n`, `-1n`→`-n` and `a==0`→`b`.
  private def nth(a: Int, b: Int): Text =
    if a == 0 then b.show else
      val coefficient = a match
        case 1  => t"n"
        case -1 => t"-n"
        case _  => t"${a}n"

      val offset =
        if b == 0 then t""
        else if b > 0 then t"+$b"
        else t"-${-b}"

      t"$coefficient$offset"

enum Simple derives CanEqual:
  case Universal(namespace: Optional[Namespace])                                  // *
  case Type(namespace: Optional[Namespace], name: Text)                           // div
  case Id(name: Name[DomId])                                                      // #id
  case Class(name: Name[CssClass])                                                // .cls
  case Nesting                                                                    // &
  case Attribute(namespace: Optional[Namespace], name: Text, test: Optional[AttributeTest])
  case PseudoClass(name: Text, argument: Optional[PseudoArgument])                // :hover
  case PseudoElement(name: Text, argument: Optional[PseudoArgument])              // ::before

enum PseudoArgument derives CanEqual:
  case Selectors(list: SelectorList)                     // :is(…) :where(…) :not(…) :has(…)
  case Nth(a: Int, b: Int, of: Optional[SelectorList])   // :nth-child(an+b [of S])
  case Raw(text: Text)                                   // :lang(en), unrecognized functions
