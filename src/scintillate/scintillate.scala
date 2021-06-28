/*
    Honeycomb, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

import scintillate.*
import rudiments.*

package integration.scintillate:
  given action[T]: Attribute["action", Uri, T] = _.toString
  given cite[T]: Attribute["cite", Uri, T] = _.toString
  given data[T]: Attribute["data", Uri, T] = _.toString
  given name[T]: Attribute["name", RequestParam[?], T] = _.key
  given formaction[T]: Attribute["formaction", Uri, T] = _.toString
  given formenctype[T]: Attribute["formenctype", MediaType, T] = _.toString
  given formmethod[T]: Attribute["formmethod", Method, T] = _.toString
  given href[T]: Attribute["href", Uri, T] = _.toString
  given enctype[T]: Attribute["enctype", MediaType, T] = _.toString
  given manifest[T]: Attribute["manifest", Uri, T] = _.toString
  given media[T]: Attribute["media", MediaType, T] = _.toString
  given poster[T]: Attribute["poster", Uri, T] = _.toString
  given src[T]: Attribute["src", Uri, T] = _.toString

  given typeName[T]: Attribute["typeName", MediaType, T] with
    override def rename: Option[String] = Some("type")
    def convert(value: MediaType): String = value.toString

  given SimpleHandler[HtmlDoc] =
    SimpleHandler("text/html; charset=utf-8", html => LazyList(HtmlDoc.serialize(html).bytes))