/*

    Litterateur, version 0.4.0. Copyright 2019-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package litterateur

import rudiments.*
import contextual.*

enum MdInput:
  case Block(content: String)
  case Inline(content: String)

object MdInterpolator extends Interpolator[MdInput, String, MdNode.Document]:
  def complete(state: String): MdNode.Document = Markdown.parse(state)
  def initial: String = ""
  
  def insert(state: String, value: Option[MdInput]): String = value match
    case Some(MdInput.Block(content))  => str"$state\n$content\n"
    case Some(MdInput.Inline(content)) => str"$state$content"
    case None                          => ""
   
  def parse(state: String, next: String): String = state+next

given Insertion[String, String] = identity(_)