/*
    Anticipation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import language.experimental.into

object Realm:
  def make(name: into Text): Realm = new Realm(name)

case class Realm(name: into Text)

trait GenericLogger:
  type Self
  def logFine(log: Self, realm: Realm, message: => Text): Unit
  def logInfo(log: Self, realm: Realm, message: => Text): Unit
  def logWarn(log: Self, realm: Realm, message: => Text): Unit
  def logFail(log: Self, realm: Realm, message: => Text): Unit

trait SimpleLogger:
  def logFine(realm: Realm, message: => Text): Unit
  def logInfo(realm: Realm, message: => Text): Unit
  def logWarn(realm: Realm, message: => Text): Unit
  def logFail(realm: Realm, message: => Text): Unit

object GenericLogger:
  given SimpleLogger is GenericLogger:
    def logFine(log: SimpleLogger, realm: Realm, message: => Text): Unit =
      log.logFine(realm, message)

    def logInfo(log: SimpleLogger, realm: Realm, message: => Text): Unit =
      log.logInfo(realm, message)

    def logWarn(log: SimpleLogger, realm: Realm, message: => Text): Unit =
      log.logWarn(realm, message)

    def logFail(log: SimpleLogger, realm: Realm, message: => Text): Unit =
      log.logFail(realm, message)
