/*
    Telekinesis, version 0.25.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import spectacular.*

object HttpVersion:
  given HttpVersion is Showable as showable =
    case 0.9 => t"HTTP/0.9"
    case 1.0 => t"HTTP/1.0"
    case 1.1 => t"HTTP/1.1"
    case 2.0 => t"HTTP/2"
    case 3.0 => t"HTTP/3"

  def parse(text: Text): HttpVersion = text match
    case t"HTTP/0.9"             => 0.9
    case t"HTTP/1.1"             => 1.1
    case t"HTTP/2" | t"HTTP/2.0" => 2.0
    case t"HTTP/3" | t"HTTP/3.0" => 3.0
    case _                       => 1.0

type HttpVersion = 0.9 | 1.0 | 1.1 | 2.0 | 3.0
