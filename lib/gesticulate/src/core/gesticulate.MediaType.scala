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
package gesticulate

import scala.caps

import scala.language.dynamics

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import fulminate.*

object MediaType:
  given inspectable: MediaType is Inspectable = mt => t"""media"${mt}""""

  given showable: MediaType is Showable =
    mt => t"${mt.basic}${mt.parameters.map { (p: (Text, Text)) => t"; ${p(0)}=${p(1)}" }.join}"

  given encodable: MediaType is Encodable in Text = _.show
  // Laundered pure: the resolution-scoped tactic shares the instance's given-resolution
  // lifetime, and wisteria-derived codecs summon `Decodable in Text` field instances
  // against pure expected types inside macro splices (see rep/DECISIONS.md).
  given decodable: (tactic: Tactic[MediaType.Error])
  =>  MediaType is Decodable in Text =
    caps.unsafe.unsafeAssumePure(Media.parse(_))

  given formenctype: ("formenctype" is GenericHtmlAttribute[MediaType]):
    def name: Text = t"formenctype"
    def serialize(mediaType: MediaType): Text = mediaType.show

  given media: ("media" is GenericHtmlAttribute[MediaType]):
    def name: Text = t"media"
    def serialize(mediaType: MediaType): Text = mediaType.show

  given enctype: ("enctype" is GenericHtmlAttribute[MediaType]):
    def name: Text = t"enctype"
    def serialize(mediaType: MediaType): Text = mediaType.show

  given htype: ("htype" is GenericHtmlAttribute[MediaType]):
    def name: Text = t"type"
    def serialize(mediaType: MediaType): Text = mediaType.show

  def unapply(value: Text): Option[MediaType] = safely(Media.parse(value)).let(Some(_)).or(None)

  inline given interpolable: MediaType is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   MediaType =

      ${gesticulate.internal.mediaInterpolator[parts]('insertions)}

  // MediaTypeError → MediaType.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NotOneSlash              extends Reason(1)
      case MissingParam             extends Reason(2)
      case InvalidGroup             extends Reason(3)
      case InvalidChar(char: Char)  extends Reason(4)
      case InvalidSuffix(suffix: Text) extends Reason(5)

      def message: Text = this match
        case NotOneSlash       => txt"a media type should always contain exactly one '/' character"
        case MissingParam      => txt"a terminal ';' suggests that a parameter is missing"
        case InvalidChar(char) => txt"the character '$char' is not allowed"
        case InvalidSuffix(s)  => txt"the suffix '$s' is not recognized"

        case InvalidGroup =>
          val list = Array.unsafeFrozen(Media.Group.values).readable.toList.map(_.name)
          txt"the type must be one of: ${list.join(t", ", t" or ")}"

  case class Error(value: Text, reason: MediaType.Error.Reason)(using Diagnostics)
  extends fulminate.Error(353, reason.number)
    ( m"the value $value is not a valid media type; ${reason.message}" )

case class MediaType
  ( group:      Media.Group,
    subtype:    Media.Subtype,
    suffixes:   List[Media.Suffix] = Nil,
    parameters: List[(Text, Text)] = Nil )
extends Dynamic:
  private def suffixString: Text = suffixes.map { (s: Media.Suffix) => t"+${s.name}" }.join
  def basic: Text = t"${group.name}/${subtype.name}$suffixString"
  def base: MediaType = MediaType(group, subtype, suffixes)
  def at(name: Text): Optional[Text] = parameters.seek(_(0) == name).let(_(1))

  def applyDynamicNamed(apply: "apply")(kvs: (String, Text)*): MediaType =
    copy(parameters = (parameters.stdlib ::: kvs.toList.map(_.show -> _)).to(List))
