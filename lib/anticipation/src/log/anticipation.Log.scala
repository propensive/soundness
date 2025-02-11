/*
    Anticipation, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

object Log:
  def fine[MessageType: Loggable](message: MessageType)(using realm: Realm): Unit =
    MessageType.log(Level.Fine, realm, System.currentTimeMillis, message)

  def info[MessageType: Loggable](message: MessageType)(using realm: Realm): Unit =
    MessageType.log(Level.Info, realm, System.currentTimeMillis, message)

  def warn[MessageType: Loggable](message: MessageType)(using realm: Realm): Unit =
    MessageType.log(Level.Warn, realm, System.currentTimeMillis, message)

  def fail[MessageType: Loggable](message: MessageType)(using realm: Realm): Unit =
    MessageType.log(Level.Fail, realm, System.currentTimeMillis, message)
