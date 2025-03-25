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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package gesticulate

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import gossamer.{where as _, *}
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics
//import language.experimental.captureChecking

case class Medium
   (group:     Media.Group,
    subtype:    Media.Subtype,
    suffixes:   List[Media.Suffix] = Nil,
    parameters: List[(Text, Text)] = Nil)
extends Dynamic:

  private def suffixString: Text = suffixes.map { s => t"+${s.name}" }.join
  def basic: Text = t"${group.name}/${subtype.name}$suffixString"
  def base: Medium = Medium(group, subtype, suffixes)

  def at(name: Text): Optional[Text] = parameters.where(_(0) == name).let(_(1))

  def applyDynamicNamed(apply: "apply")(kvs: (String, Text)*): Medium =
    copy(parameters = parameters ::: kvs.map(_.show -> _).to(List))

object Medium:
  given inspectable: Medium is Inspectable = mt => t"""media"${mt}""""

  given showable: Medium is Showable =
    mt => t"${mt.basic}${mt.parameters.map { p => t"; ${p(0)}=${p(1)}" }.join}"

  given encodable: Medium is Encodable in Text = _.show
  given decodable: Tactic[MediumError] => Medium is Decodable in Text = Media.parse(_)

  given formenctype: ("formenctype" is GenericHtmlAttribute[Medium]):
    def name: Text = t"formenctype"
    def serialize(medium: Medium): Text = medium.show

  given media: ("media" is GenericHtmlAttribute[Medium]):
    def name: Text = t"media"
    def serialize(medium: Medium): Text = medium.show

  given enctype: ("enctype" is GenericHtmlAttribute[Medium]):
    def name: Text = t"enctype"
    def serialize(medium: Medium): Text = medium.show

  given htype: ("htype" is GenericHtmlAttribute[Medium]):
    def name: Text = t"type"
    def serialize(medium: Medium): Text = medium.show

  def unapply(value: Text): Option[Medium] = safely(Some(Media.parse(value))).or(None)
