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
import anticipation.*
import profanity.*
import rudiments.*
import turbulence.*

import executives.completions

@main
def fury(): Unit =
  import parameterInterpretation.posixParameters
  import errorHandlers.throwUnsafely
  
  daemon:
    val Lang = Flag[Language](t"speech", false, List('s'), t"the two-letter code of the language")
    val Size = Flag[Text](t"size", false, List('S'), t"big, medium or small")
    val Age = Flag[Int](t"age", false, List('a'), t"the number of years")
    val Color = Flag[Text]('c', false, List(), t"the color")
    val SubcommandAction = Subcommand[Action](0)
    
    SubcommandAction() match
      case Unset =>
        Lang()
      
      case Action.Run   =>
        Lang()
        Size()
      
      case Action.Build =>
        Size()
        Color()
      
      case _ =>
        Age()
        

    execute:
      Out.println(t"language = ${Lang().debug}")
      Out.println(t"size = ${Size().debug}")
      Out.println(t"age = ${Age().debug}")
    
      supervise:
        terminal:
          tty.events.foreach:
            case Keypress.CharKey('Q') => summon[DaemonClient].shutdown()
            case other                 => Out.println(other.debug)

          ExitStatus.Ok

object Language:
  given Suggestions[Language] = () => Language.values.map: language =>
    Suggestion(language.toString.tt.lower, language.toString.tt.upper)
  
  given Decoder[Language] = text => valueOf(text.lower.capitalize.s)

enum Language:
  case En, Fr, De, Es

object Action:
  given Suggestions[Action] = () => Action.values.map: action =>
    Suggestion(action.toString.tt.lower, t"Do the ${action.toString.tt} action")
  
  given Decoder[Action] = text => valueOf(text.lower.capitalize.s)

enum Action:
  case Run, Build, Fire, Cheat
