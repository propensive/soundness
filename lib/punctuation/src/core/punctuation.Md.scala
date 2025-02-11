/*
    Punctuation, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gossamer.*
import vacuous.*

import scala.compiletime.*

import errorDiagnostics.empty

type Md = Markdown[Markdown.Ast.Block]

object Md:
  enum Input:
    case Block(content: Text)
    case Inline(content: Text)

  object Input:
    given Insertion[Input, Text] = Input.Inline(_)

  object Interpolator extends contextual.Interpolator[Input, Input, Markdown[Markdown.Ast.Node]]:

    def complete(state: Input): Markdown[Markdown.Ast.Node] = state match
      case Input.Inline(state) =>
        safely(Markdown.parseInline(state)).or:
          throw InterpolationError(m"the markdown could not be parsed")

      case Input.Block(state)  =>
        safely(Markdown.parse(state)).or:
          throw InterpolationError(m"the markdown could not be parsed")

    def initial: Input = Input.Inline(t"")
    def skip(state: Input): Input = state

    def insert(state: Input, value: Input): Input = value match
      case Input.Block(content)  => state match
        case Input.Block(state)    => Input.Block(t"$state\n$content\n")
        case Input.Inline(state)   => Input.Block(t"$state\n$content")

      case Input.Inline(content) => state match
        case Input.Block(state)    => Input.Block(t"$state\n$content\n")
        case Input.Inline(state)   => Input.Inline(t"$state$content")

    def parseInline(text: Text): Optional[Input.Inline] = safely:
      Markdown.parseInline(text)
      Input.Inline(text)

    def parseBlock(text: Text): Optional[Input.Block] = safely:
      Markdown.parse(text)
      Input.Block(text)

    def parse(state: Input, next: Text): Input = state match
      case Input.Inline(state) =>
        parseInline(t"$state$next").or(parseBlock(t"$state$next")).or:
          throw InterpolationError(m"the markdown could not be parsed")

      case Input.Block(state) =>
        safely:
          Markdown.parse(t"$state$next")
          Input.Block(t"$state$next")

        . or:
            throw InterpolationError(m"the markdown could not be parsed")
