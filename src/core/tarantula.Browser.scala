/*
    Tarantula, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package tarantula

import anticipation.*, durationApi.javaLong
import cataclysm.*
import contingency.*
import eucalyptus.*
import fulminate.*
import gastronomy.*
import gesticulate.*
import gossamer.*
import guillotine.*
import hallucination.*
import hieroglyph.*, charEncoders.utf8
import honeycomb.*
import jacinta.*, jsonPrinters.minimal, dynamicJsonAccess.enabled
import monotonous.*
import nettlesome.*
import parasite.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*

import strategies.throwUnsafely

import unsafeExceptions.canThrowAny

trait Browser(name: Text):
  transparent inline def browser = this

  case class Server(port: Int, value: Process[Label, Text]):
    def stop(): Unit logs Text = browser.stop(this)

  def launch(port: Int)(using WorkingDirectory, Monitor): Server logs Text
  def stop(server: Server): Unit logs Text

  def session[ResultType](port: Int = 4444)(block: (session: WebDriver#Session) ?=> ResultType)
     (using WorkingDirectory, Monitor)
          : ResultType logs Text =

    val server = launch(port)
    try block(using WebDriver(server).startSession()) finally server.stop()
