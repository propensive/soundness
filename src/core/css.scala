/*
    Cataract, version 0.1.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cataract

import rudiments.*

import language.dynamics

case class Stylesheet(rules: Rule*)
  

case class Style(properties: CssProperty*):
  override def toString(): String = properties.map(_.toString).join("\n")

case class Rule(selector: Selector, style: Style):
  override def toString(): String =
    val rules = style.properties.map(_.toString).join("; ")
    str"${selector.value} { $rules }"

case class CssProperty(key: String, value: String):
  override def toString(): String = str"$key: $value"

object Testing:
  val css = Css(border = "hello", paddingLeft = "10em")

@main
def run(): Unit =
  println(Testing.css)