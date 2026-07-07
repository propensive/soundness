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
package honeycomb

import anticipation.*
import gossamer.*
import prepositional.*
import vacuous.*

object Unattributive:
  given textTextual: Whatwg.Textual is Unattributive to Text = _.or(t"")

  // given boolean: Boolean is Attributive to Presence = (key, value) =>
  //   if value then (key, Unset) else Unset

  // given switch: Boolean is Attributive to Switch = (key, value) =>
  //   (key, if value then t"on" else t"off")

  // given truth: Boolean is Attributive to Truth = (key, value) =>
  //   (key, if value then t"true" else t"false")

  given int: Whatwg.Integral is Unattributive to Optional[Int] = _.let(_.s.toInt)
  given positiveInt: Whatwg.PositiveInt is Unattributive to Optional[Int] = _.let(_.s.toInt)

  given cssClassList: Whatwg.CssClassList is Unattributive to List[Text] =
    _.let(_.cut(t" ")).or(Nil)

  // given posInt: Int is Attributive to PositiveInt = _ -> _.show
  // given double: Double is Attributive to Decimal = _ -> _.toString.tt
  // given domId: DomId is Attributive to Id = _ -> _.toString.tt
  // given stylesheet: Stylesheet is Attributive to CssClassList = _ -> _.classes.join(t" ")

  // given url: [url: Abstractable across Urls to Text] => url is Attributive to Url =
  //   (key, value) => (key, value.generic)

  // given url: HttpUrl is Attributive to Url = (key, value) => (key, value.show)

  // given style: Text is Attributive to Css = (key, value) => (key, value)

trait Unattributive extends Typeclass, Resultant:
  def unattribute(value: Optional[Text]): Result
