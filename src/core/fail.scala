/*
    Fulminate, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package fulminate

import scala.quoted.*

import language.experimental.captureChecking

def fail
    (using quotes: Quotes)
    (message: Message, pos: quotes.reflect.Position | Null = null)
    : Nothing =
  import quotes.reflect.*
  import dotty.tools.dotc.config.Settings.Setting.value
  
  val useColor: Boolean = quotes match
    case quotes: runtime.impl.QuotesImpl =>
      value(quotes.ctx.settings.color)(using quotes.ctx) != "never"
    
    case _ =>
      false

  val pkg = Symbol.spliceOwner.owner.fullName.split("\\.").nn.map(_.nn).to(List).takeWhile(_.head.isLower).mkString(".")
  
  val text =
    if useColor
    then s"${27.toChar}[38;2;0;190;255m${27.toChar}[1m$pkg${27.toChar}[0m ${message.colorText}"
    else s"$pkg: ${message.text}" 
  
  if pos == null then report.errorAndAbort(text) else report.errorAndAbort(text, pos)
