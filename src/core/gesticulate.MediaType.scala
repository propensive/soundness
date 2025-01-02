/*
    Gesticulate, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gesticulate

import anticipation.*
import contextual.*
import contingency.*
import gossamer.{where as _, *}
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics
//import language.experimental.captureChecking

case class MediaType
   (group:      Media.Group,
    subtype:    Media.Subtype,
    suffixes:   List[Media.Suffix] = Nil,
    parameters: List[(Text, Text)] = Nil)
extends Dynamic:

  private def suffixString: Text = suffixes.map { s => t"+${s.name}" }.join
  def basic: Text = t"${group.name}/${subtype.name}$suffixString"
  def base: MediaType = MediaType(group, subtype, suffixes)

  def at(name: Text): Optional[Text] = parameters.where(_(0) == name).let(_(1))

  def applyDynamicNamed(apply: "apply")(kvs: (String, Text)*): MediaType =
    copy(parameters = parameters ::: kvs.map(_.show -> _).to(List))

object MediaType:
  given MediaType is Inspectable = mt => t"""media"${mt}""""
  given ("content-type" is GenericHttpRequestParam[MediaType]) as contentType = showable.text(_)
  given ("accept" is GenericHttpRequestParam[MediaType]) as accept = showable.text(_)

  given MediaType is Showable as showable =
    mt => t"${mt.basic}${mt.parameters.map { p => t"; ${p(0)}=${p(1)}" }.join}"

  given MediaType is Encodable in Text = _.show
  given (using Tactic[MediaTypeError]) => Decoder[MediaType] = Media.parse(_)

  given ("formenctype" is GenericHtmlAttribute[MediaType]) as formenctype:
    def name: Text = t"formenctype"
    def serialize(mediaType: MediaType): Text = mediaType.show

  given ("media" is GenericHtmlAttribute[MediaType]) as media:
    def name: Text = t"media"
    def serialize(mediaType: MediaType): Text = mediaType.show

  given ("enctype" is GenericHtmlAttribute[MediaType]) as enctype:
    def name: Text = t"enctype"
    def serialize(mediaType: MediaType): Text = mediaType.show

  given ("htype" is GenericHtmlAttribute[MediaType]) as htype:
    def name: Text = t"type"
    def serialize(mediaType: MediaType): Text = mediaType.show

  def unapply(value: Text): Option[MediaType] = safely(Some(Media.parse(value))).or(None)
