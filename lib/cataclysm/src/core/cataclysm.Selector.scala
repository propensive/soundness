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
import vacuous.*

// The structural model of a CSS selector, following Selectors Level 4. The
// three nesting levels are: a `SelectorList` (comma-separated, the loosest), a
// `Selector` (a complex selector — compounds joined by combinators), and a
// `Compound` (simple selectors with no whitespace between them, the tightest).

case class SelectorList(selectors: List[Selector]) derives CanEqual

// A complex selector: a head compound followed by combinator/compound steps.
// `lead` is set only for a relative selector (e.g. the `>` in `:has(> img)`).
case class Selector(lead: Optional[Combinator], head: Compound, rest: List[(Combinator, Compound)])
derives CanEqual

// A compound selector: a run of simple selectors bound together with no
// combinator (hence no whitespace) between them.
case class Compound(parts: List[Simple]) derives CanEqual

// The right-hand side of an attribute selector, e.g. the `^= "x" i` part.
case class AttributeTest(matcher: AttributeMatcher, value: Text, modifier: Optional[Char])
derives CanEqual

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

enum Simple derives CanEqual:
  case Universal(namespace: Optional[Namespace])                                  // *
  case Type(namespace: Optional[Namespace], name: Text)                           // div
  case Id(name: Text)                                                             // #id
  case Class(name: Text)                                                          // .cls
  case Nesting                                                                    // &
  case Attribute(namespace: Optional[Namespace], name: Text, test: Optional[AttributeTest])
  case PseudoClass(name: Text, argument: Optional[PseudoArgument])                // :hover
  case PseudoElement(name: Text, argument: Optional[PseudoArgument])              // ::before

enum PseudoArgument derives CanEqual:
  case Selectors(list: SelectorList)                     // :is(…) :where(…) :not(…) :has(…)
  case Nth(a: Int, b: Int, of: Optional[SelectorList])   // :nth-child(an+b [of S])
  case Raw(text: Text)                                   // :lang(en), unrecognized functions
