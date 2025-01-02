/*
    Spectacular, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import anticipation.*
import denominative.*
import digression.*
import fulminate.*
import rudiments.*
import vacuous.*

import language.experimental.captureChecking

trait Showable extends TextConversion:
  type Self

object Showable:
  given Specializable is Showable as specializable = value =>
    value.getClass.nn.getName.nn.split("\\.").nn.last.nn.dropRight(1).toLowerCase.nn.tt

  given Ordinal is Showable as zerary = ordinal => s"${ordinal.n0}.₀".tt

  given StackTrace is Showable = stack =>
    val methodWidth = stack.frames.map(_.method.method.s.length).maxOption.getOrElse(0)
    val classWidth = stack.frames.map(_.method.className.s.length).maxOption.getOrElse(0)
    val fileWidth = stack.frames.map(_.file.s.length).maxOption.getOrElse(0)

    val fullClass = s"${stack.component}.${stack.className}".tt
    val init = s"$fullClass: ${stack.message}".tt

    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.method.className.s.endsWith("#")
        val drop = if obj then 1 else 0
        val file = (" "*(fileWidth - frame.file.s.length))+frame.file
        val dot = if obj then ".".tt else "#".tt
        val className = frame.method.className.s.dropRight(drop)
        val classPad = (" "*(classWidth - className.length)).tt
        val method = frame.method.method
        val methodPad = (" "*(methodWidth - method.s.length)).tt
        val line = frame.line.let(_.show).or("?".tt)

        s"$msg\n  at $classPad$className$dot$method$methodPad $file:$line".tt

    stack.cause.lay(root): cause =>
      s"$root\ncaused by:\n$cause".tt
