/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import scala.quoted.*

def fail
    (using quotes: Quotes)
    (message: Message, pos: Maybe[quotes.reflect.Position] = Unset)
    : Nothing =
  import quotes.reflect.*
  
  val text = message.richText
  val pkg = Thread.currentThread.nn.getStackTrace.nn(2).nn.getClassName.nn.split("\\.").nn(0).nn
  pos.mm(report.errorAndAbort(pkg+": "+text, _)).or(report.errorAndAbort(pkg+": "+text))
