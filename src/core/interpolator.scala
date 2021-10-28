/*
    Punctuation, version 0.12.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package punctuation

import rudiments.*
import contextual.*
import gossamer.*

object Md:
  enum Input:
    case Block(content: Txt)
    case Inline(content: Txt)

  object Input:
    given Insertion[Input, Txt] = Input.Inline(_)
  
  object Interpolator extends contextual.Interpolator[Input, Input, Markdown[Markdown.Ast.Node]]:
    
    def complete(state: Input): Markdown[Markdown.Ast.Node] = state match
      case Input.Inline(state) =>
        try Markdown.parseInline(state) catch case BadMarkdownError(_) =>
          throw InterpolationError(s"the markdown could not be parsed")
      
      case Input.Block(state)  =>
        try Markdown.parse(state) catch case BadMarkdownError(msg) =>
          throw InterpolationError(s"the markdown could not be parsed; $msg")
  
    def initial: Input = Input.Inline(str"")
    def skip(state: Input): Input = state

    def insert(state: Input, value: Input): Input = value match
      case Input.Block(content)  => state match
        case Input.Block(state)    => Input.Block(str"$state\n$content\n")
        case Input.Inline(state)   => Input.Block(str"$state\n$content")
      case Input.Inline(content) => state match
        case Input.Block(state)    => Input.Block(str"$state\n$content\n")
        case Input.Inline(state)   => Input.Inline(str"$state$content")
     
    def parse(state: Input, next: String): Input = state match
      case Input.Inline(state) =>
        try
          Markdown.parseInline(state+next)
          Input.Inline(state+next)
        catch
          case BadMarkdownError(_) =>
            try
              Markdown.parse(state+next)
              Input.Block(state+next)
            catch
              case BadMarkdownError(_) => throw InterpolationError(s"the markdown could not be parsed")

      case Input.Block(state) =>
        try
          Markdown.parse(state+next)
          Input.Block(state+next)
        catch case BadMarkdownError(_) => throw InterpolationError(s"the markdown could not be parsed")
