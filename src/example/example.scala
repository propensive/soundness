/*
    Spectral, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectral

import perforate.*
import spectacular.*
import gossamer.*
import anticipation.*
import ambience.*, systemProperties.jvm
import profanity.*
import parasite.*
import rudiments.*
import exoskeleton.*
import turbulence.*

import executives.completions

@main
def example(): Unit =
  import errorHandlers.throwUnsafely
  import parameterInterpretation.posix
    
  daemon[Int]:

    val Lang = Flag[Language](t"language", false, List('L'), t"the two-letter code of the language")
    val Size = Flag[Text](t"size", false, List('S'), t"big, medium or small")
    val Age = Flag[Int](t"age", false, List('a'), t"the number of years")
    val Color = Flag[Text]('c', false, List(), t"the color")
    val Verbose = Switch('v', false, List(t"verbose"), t"Verbose output")

    Age()
    Size()
    Color()
    Verbose()
    Lang()

    execute:
      TabCompletions.install(Shell.Fish, t"launcher", false)
      TabCompletions.install(Shell.Bash, t"launcher", false)
      
      supervise:
        terminal:
          Out.println(t"Beginning")
          Out.println(Age().debug)
          Out.println(Size().debug)
          Out.println(Color().debug)
          Out.println(Lang().debug)
          Out.println(Properties.spectral.fpath[Text]())
          Out.println(Properties.spectral.script[Text]())
          Out.println(t"scriptPath=${service.scriptPath}")
          Out.println(t"commandPath=${service.commandPath.or(t"no")}")
          import booleanStyles.yesNo
          Out.println(t"onPath=${service.scriptIsOnPath.show}")

          ExitStatus.Ok

object Language:
  given Suggestions[Language] = () => Language.values.map: language =>
    Suggestion(language.toString.tt.lower, language.toString.tt.upper)
  
  given Decoder[Language] = text => valueOf(text.lower.capitalize.s)

enum Language:
  case En, Fr, De, Es

