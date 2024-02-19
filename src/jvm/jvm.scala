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

import guillotine.*
import anthology.*
import eucalyptus.*
import contingency.*
import rudiments.*
import anticipation.*
import ambience.*, systemProperties.virtualMachine

object remote extends Dispatcher:
  type Result[OutputType] = OutputType

  val scalac: Scalac[3.4] = Scalac[3.4](List())

  protected def invoke[OutputType](dispatch: Dispatch[OutputType]): OutputType =
    import workingDirectories.virtualMachine
    import logging.silent
    
    dispatch.remote: input =>
      val cmd = sh"java -classpath ${dispatch.classpath()} ${dispatch.mainClass} $input"
      unsafely(cmd.exec[Text]())
