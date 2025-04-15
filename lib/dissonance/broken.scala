/*
    Dissonance, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dissonance

import anticipation.*
import contingency.*
import fulminate.*
import rudiments.*

case class CasualDiffError(reason: CasualDiffError.Reason, line: Int)(using Diagnostics)
extends Error(m"the diff could not be read because $reason at line $line")

object CasualDiffError:
  enum Reason:
    case BadLineStart(content: Text)
    case DoesNotMatch(content: Text)

  given communicable: Reason is Communicable =
    case Reason.BadLineStart(content) =>
      m"the line $content did not begin with either ${"'+ '".tt}, ${"'- '".tt} or ${"'  '".tt}"

    case Reason.DoesNotMatch(content) =>
      m"the line $content could not be found in the document"

case class Replace(context: List[Text], original: List[Text], replacement: List[Text])

object CasualDiff:
  def parse(stream: Stream[Text])(using Tactic[CasualDiffError]): CasualDiff =
    def recur
         (stream:      Stream[Text],
          context:     List[Text],
          original:    List[Text],
          replacement: List[Text],
          done:        List[Replace],
          lineNo:      Int)
    :     List[Replace] =

      stream match
        case head #:: tail =>
          if head.s.startsWith("  ") then
            if !original.isEmpty || !replacement.isEmpty
            then
              val replace = Replace(context.reverse, original.reverse, replacement.reverse)
              recur(tail, List(head.s.drop(2).tt), Nil, Nil, replace :: done, lineNo + 1)
            else recur(tail, head.s.drop(2).tt :: context, Nil, Nil, done, lineNo + 1)
          else if head.s.startsWith("+ ") then
            recur(tail, context, original, head.s.drop(2).tt :: replacement, done, lineNo + 1)
          else if head.s.startsWith("- ") then
            if !replacement.isEmpty
            then
              val replace = Replace(context.reverse, original.reverse, replacement.reverse)
              recur(tail, Nil, List(head.s.drop(2).tt), Nil, replace :: done, lineNo + 1)
            else recur(tail, Nil, head.s.drop(2).tt :: original, Nil, done, lineNo + 1)
          else raise
                (CasualDiffError(CasualDiffError.Reason.BadLineStart(head), lineNo),
                 recur(tail, context, original, replacement, done, lineNo + 1))

        case _ =>
          (Replace(context.reverse, original.reverse, replacement.reverse) :: done).reverse

    CasualDiff(recur(stream, Nil, Nil, Nil, Nil, 1))

case class CasualDiff(replacements: List[Replace]):
  def patch(original: Iterable[Text]): Stream[Text] raises CasualDiffError =
    def recur(stream: Stream[Text], focus: List[Text], todo: List[Replace], lineNo: Int)
    :     Stream[Text] =
      todo match
        case Nil =>
          Stream()

        case Replace(Nil, original, replacement) :: tail => focus match
          case Nil => tail match
            case Replace(context, next, _) :: tail =>
              val lineNo2 = lineNo + original.length + replacement.length
              replacement.to(Stream) #::: recur(stream, next, todo.tail, lineNo2)

            case Nil =>
              replacement.to(Stream) #::: stream

          case line :: rest => stream match
            case head #:: tail =>
              if head == line then recur(tail, rest, todo, lineNo)
              else original.dropRight(focus.length).to(Stream) #::: head #::
                  recur(tail, original, todo, lineNo)

            case _ =>
              val lineNo2 = lineNo + original.length - focus.length
              abort(CasualDiffError(CasualDiffError.Reason.DoesNotMatch(line), lineNo2))

        case Replace(line :: rest, original, replacement) :: todoTail => stream.runtimeChecked match
          case head #:: tail =>
            head #:: {
              if head != line then recur(tail, focus, todo, lineNo + 1)
              else recur(tail, focus, Replace(rest, original, replacement) :: todoTail, lineNo + 1)
            }

    if replacements.isEmpty then original.to(Stream)
    else recur(original.to(Stream), replacements.head.original, replacements, 1)

  def serialize: Stream[Text] = replacements.to(Stream).flatMap:
    case Replace(context, original, replacement) =>
      (context.to(Stream).map("  "+_) #::: original.to(Stream).map("- "+_) #:::
          replacement.to(Stream).map("+ "+_)).map(_.tt)

*/
