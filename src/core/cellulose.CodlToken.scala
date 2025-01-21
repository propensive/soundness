/*
    Cellulose, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import anticipation.*
import contextual.*
import gossamer.*
import spectacular.*, booleanStyles.trueFalse

enum CodlToken:
  case Indent, Peer, Blank, Argument
  case Outdent(n: Int)
  case Item(text: Text, line: Int, col: Int, block: Boolean = false)
  case Comment(text: Text, line: Int, col: Int)
  case Error(error: CodlError)
  case Body(stream: LazyList[Char])

object CodlToken:
  given CodlToken is Inspectable =
    case Indent                       => t"Indent"
    case Peer                         => t"Peer"
    case Blank                        => t"Blank"
    case Argument                     => t"Argument"
    case Body(_)                      => t"Body(...)"
    case Outdent(n)                   => t"Outdent($n)"
    case Item(text, line, col, block) => t"Item($text, $line, $col, $block)"
    case Comment(text, line, col)     => t"Comment($text, $line, $col)"
    case Error(error)                 => t"Error(${error.message})"
