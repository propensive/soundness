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
import coaxial.*
import fulminate.*
import nettlesome.*
import prepositional.*
import vacuous.*

object Fetchable:
  given httpUrl: [UrlType <: HttpUrl] => UrlType is Fetchable onto Origin["http" | "https"] =
    new Fetchable:
      type Self = UrlType
      type Target = Origin["http" | "https"]

      def target(httpUrl: UrlType): Origin["http" | "https"] = httpUrl.origin
      def text(httpUrl: UrlType): Text = httpUrl.pathText

      def hostname(httpUrl: UrlType): Hostname = httpUrl.host.or:
        panic(m"The HTTP URL does not have a hostname")

  given unixSocket: DomainSocketEndpoint is Fetchable onto DomainSocket =
    new Fetchable:
      type Self = DomainSocketEndpoint
      type Target = DomainSocket

      def target(endpoint: DomainSocketEndpoint): DomainSocket = endpoint.socket
      def text(endpoint: DomainSocketEndpoint): Text = endpoint.path
      def hostname(endpoint: DomainSocketEndpoint): Hostname = Localhost

trait Fetchable:
  type Self
  type Target
  def target(value: Self): Target
  def text(value: Self): Text
  def hostname(value: Self): Hostname
