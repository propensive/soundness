/*
    Xylophone, version 0.1.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package xylophone

import rudiments.*
import gossamer.*

enum Ast:
  case Element(name: XmlName, children: Seq[Ast], attributes: Map[XmlName, String] = Map(),
                   namespaces: List[Namespace] = Nil)
  case Comment(content: String)
  case ProcessingInstruction(target: String, content: String)
  case Text(content: String)
  case CData(content: String)
  case Root(content: Ast*)

  override def toString(): String = this match
    case Element(name, children, attributes, _) =>
      val inside = children.map(_.toString).join
      val attributeString = attributes.map { (k, v) => str"${k.toString}=$v" }.join(" ", " ", "")
      
      str"<${name.toString}${attributeString}>$inside</${name.toString}>"

    case Comment(content) =>
      str"<!--$content-->"

    case ProcessingInstruction(target, content) =>
      str"<?$target $content?>"

    case Text(content) =>
      content

    case CData(content) =>
      str"<![CDATA[${content.toString}]]>"

    case Root(content*) =>
      str"""<?xml version = "1.0"?>${content.map(_.toString).join}"""
