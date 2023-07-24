/*
    Dissonance, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import anticipation.*

case class DiffError() extends Error(msg"the diff did not correspond to the input")

case class Replace(original: List[Text], replacement: List[Text])

object CasualDiff:
  def parse(stream: LazyList[Text]): CasualDiff throws DiffError =
    def recur(stream: LazyList[Text], original: List[Text], replacement: List[Text], done: List[Replace]): List[Replace] = stream match
      case head #:: tail =>
        if head.s.startsWith("+ ") then recur(tail, original, head.s.drop(2).tt :: replacement, done)
        else if head.s.startsWith("- ") then
          if !replacement.isEmpty then recur(tail, List(head.s.drop(2).tt), Nil, Replace(original.reverse, replacement.reverse) :: done)
          else recur(tail, head.s.drop(2).tt :: original, Nil, done)
        else throw DiffError()
      
      case _ =>
        (Replace(original.reverse, replacement.reverse) :: done).reverse

    CasualDiff(recur(stream, Nil, Nil, Nil))

case class CasualDiff(replacements: List[Replace]):
  def apply(original: LazyList[Text]): LazyList[Text] throws DiffError =
    def recur(stream: LazyList[Text], focus: List[Text], todo: List[Replace]): LazyList[Text] = todo match
      case Nil =>
        LazyList()
        
      case Replace(original, replacement) :: tail => focus match
        case Nil => tail match
          case Replace(next, _) :: tail =>
            replacement.to(LazyList) #::: recur(stream, next, todo.tail)
          
          case Nil =>
            replacement.to(LazyList) #::: stream

        case line :: rest => stream match
          case head #:: tail =>
            if head == line then recur(tail, rest, todo)
            else original.dropRight(focus.length).to(LazyList) #::: head #:: recur(tail, original, todo)
          
          case _ =>
            throw DiffError()
    
    if replacements.isEmpty then original else recur(original, replacements.head.original, replacements)
            
