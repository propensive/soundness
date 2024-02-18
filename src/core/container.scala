/*
    Superlunary, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package superlunary

import jacinta.*
import gossamer.*
import inimitable.*
import anticipation.*
import anthology.*
import fulminate.*
import contingency.*
import spectacular.*
import hellenism.*, classloaders.threadContext

import scala.quoted.*

given Scalac[3.4] = Scalac[3.4](List())

case class Example(name: Text, count: Long)

@main
def run(): Unit =
  given Raises[JsonAccessError] = errorHandlers.throwUnsafely
  given Raises[ScalacError] = errorHandlers.throwUnsafely

  def offset(input: Long): Text = remote.dispatch:
    '{
      t"${System.currentTimeMillis - ${System.currentTimeMillis.put}}"
    }

  def fn(message: Example): Text = remote.dispatch:
    '{
      t"Time: ${System.currentTimeMillis - ${message.count.put}}"
    }

  println(offset(System.currentTimeMillis))