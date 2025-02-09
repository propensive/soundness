/*
    Telekinesis, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import anticipation.*
import fulminate.*
import gossamer.*
import proscenium.*
import spectacular.*

import language.dynamics

erased trait Http

object Http:

  object Method:
    given formmethod: ("formmethod" is GenericHtmlAttribute[Method]):
      def name: Text = t"formmethod"
      def serialize(method: Method): Text = method.show

    given method: ("method" is GenericHtmlAttribute[Method]):
      def name: Text = t"method"
      def serialize(method: Method): Text = method.show

    given communicable: Method is Communicable = method => Message(method.show.upper)

    given Method is Showable =
      case method    => method.toString.tt.upper

    given Decoder[Method] = _.upper match
      case t"HEAD"    => Http.Head
      case t"POST"    => Http.Post
      case t"PUT"     => Http.Put
      case t"DELETE"  => Http.Delete
      case t"CONNECT" => Http.Connect
      case t"OPTIONS" => Http.Options
      case t"TRACE"   => Http.Trace
      case t"PATCH"   => Http.Patch
      case t"GET"     => Http.Get
      case _          => Http.Get

  sealed trait Method(tracked val payload: Boolean):
    def unapply(request: HttpRequest): Boolean = request.method == this

  case object Get extends Method(false)
  case object Head extends Method(false)
  case object Post extends Method(true)
  case object Put extends Method(true)
  case object Delete extends Method(false)
  case object Connect extends Method(false)
  case object Options extends Method(false)
  case object Trace extends Method(false)
  case object Patch extends Method(false)
