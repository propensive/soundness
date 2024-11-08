/*
    Telekinesis, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import rudiments.*
import fulminate.*
import spectacular.*
import anticipation.*

object HttpMethod:
  given ("formmethod" is GenericHtmlAttribute[HttpMethod]) as formmethod:
    def name: Text = t"formmethod"
    def serialize(method: HttpMethod): Text = method.show

  given ("method" is GenericHtmlAttribute[HttpMethod]) as method:
    def name: Text = t"formmethod"
    def serialize(method: HttpMethod): Text = method.show

  given HttpMethod is Communicable as communicable = method => Message(method.show.upper)

enum HttpMethod:
  case Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch
