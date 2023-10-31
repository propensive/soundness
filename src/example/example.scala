/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import perforate.*
import spectacular.*
import parasite.*
import gossamer.*
import escapade.*
import anticipation.*
import profanity.*
import rudiments.*
import turbulence.*

@main
def fury(): Unit =
  import parameterInterpretation.posixParameters
  import errorHandlers.throwUnsafely
  Daemon.listen:

    val Lang = Flag[Language](t"lang", false, List('l'))

    val language = parameters(Lang)
    
    execute:
      Out.println(arguments.debug)
      supervise:
        terminal:
          Out.println(language.debug)
          ExitStatus.Ok

object Language:
  given Suggestions[Language] = () => Language.values.map: language =>
    Suggestion(language.toString.tt.lower, language.toString.tt.upper)
  
  given Decoder[Language] = text => valueOf(text.lower.capitalize.s)

enum Language:
  case En, Fr, De, Es