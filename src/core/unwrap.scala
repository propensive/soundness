/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

extension (string: String)
  def unwrap: Text =
    val buf: StringBuilder = StringBuilder()
    
    def recur(lines: List[String], break: Boolean): Text = lines match
      case Nil =>
        Text(buf.toString)
      case line :: tail =>
        if line.forall(_.isWhitespace) then recur(tail, true) else
          buf.append(if !break then " " else "\n")
          buf.append(line.trim.nn.replaceAll("\\s+", " "))
          recur(tail, false)
    
    recur(string.split("\n").nn.map(_.nn).to(List), false)