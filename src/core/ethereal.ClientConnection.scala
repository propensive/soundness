/*
    Ethereal, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ethereal

import anticipation.*
import contingency.*
import parasite.*
import profanity.*
import rudiments.*
import turbulence.*

import java.net as jn
import java.io as ji

import language.experimental.pureFunctions

case class ClientConnection[BusType <: Matchable](pid: Pid):
  val stderr: Promise[ji.OutputStream] = Promise()
  val signals: Spool[Signal] = Spool()
  val bus: Spool[BusType] = Spool()
  val terminatePid: Promise[Pid] = Promise()
  val exitPromise: Promise[Exit] = Promise()
  def receive(message: BusType): Unit = bus.put(message)
  val socket: Promise[jn.Socket] = Promise()
  def close(): Unit = safely(socket.await(1000L).close())
