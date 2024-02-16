/*
    Xylophone, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import anticipation.*
import gossamer.*
import spectacular.*

object Ast:
  given Show[Ast] =
    case Comment(content)                       => t"<!--$content-->"
    case ProcessingInstruction(target, content) => t"<?$target $content?>"
    case Textual(content)                       => content
    case CData(content)                         => t"<![CDATA[$content]]>"
    case Root(content*)                         => t"""<?xml version = "1.0"?>${content.map(_.show).join}"""
    
    case Element(name, children, attributes, _) =>
      val inside = children.map(_.show).join
      val attributeString = attributes.map { case (k, v) => t"${k.show}=$v" }.join(t" ", t" ", t"")
      
      t"<${name.show}${attributeString}>$inside</${name.show}>"

enum Ast:
  case Element
      (name: XmlName, children: List[Ast], attributes: Map[XmlName, Text] = Map(),
          namespaces: List[Namespace] = Nil)
  case Comment(content: Text)
  case ProcessingInstruction(target: Text, content: Text)
  case Textual(content: Text)
  case CData(content: Text)
  case Root(content: Ast*)
