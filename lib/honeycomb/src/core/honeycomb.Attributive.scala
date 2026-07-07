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
import nomenclature.*
import prepositional.*
import spectacular.*
import urticose.*
import vacuous.*

object Attributive:
  given textTextual: Text is Attributive to Whatwg.Textual = (key, value) => (key, value)
  given stringTextual: String is Attributive to Whatwg.Textual = (key, value) => (key, value.tt)

  given boolean: Boolean is Attributive to Whatwg.Presence = (key, value) =>
    if value then (key, Unset) else Unset

  given switch: Boolean is Attributive to Whatwg.Switch = (key, value) =>
    ( key, if value then t"on" else t"off" )

  given truth: Boolean is Attributive to Whatwg.Truth = (key, value) =>
    ( key, if value then t"true" else t"false" )

  given int: Int is Attributive to Whatwg.Integral = _ -> _.show
  given posInt: Int is Attributive to Whatwg.PositiveInt = _ -> _.show
  given double: Double is Attributive to Whatwg.Decimal = _ -> _.toString.tt
  given domId: (Name[DomId] is Attributive to Whatwg.Id) = _ -> _
  given cssClass: (Name[CssClass] is Attributive to Whatwg.CssClassList) = _ -> _
  given classList: ClassList is Attributive to Whatwg.CssClassList = _ -> _.classes.join(t" ")

  given url: [url: Abstractable across Urls to Text] => url is Attributive to Whatwg.Url =
    (key, value) => (key, value.generic)

  given url: HttpUrl is Attributive to Whatwg.Url = (key, value) => (key, value.show)
  given style: Text is Attributive to Whatwg.Css = (key, value) => (key, value)

  given cssClassList: List[Name[CssClass]] is Attributive to Whatwg.CssClassList =
    (key, value) => (key, value.join(t" "))

  given domIds: List[Name[DomId]] is Attributive to Whatwg.Ids =
    (key, value) => (key, value.join(t" "))

trait Attributive extends Typeclass, Resultant:
  def attribute(key: Text, value: Self): Optional[(Text, Optional[Text])]
