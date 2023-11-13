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

import language.experimental.captureChecking

def contextMessage
    (module: Maybe[String] = Unset,
     typeclass: Maybe[String] = Unset,
     param: Maybe[String] = Unset,
     reason: Maybe[String] = Unset,
     suggest: Maybe[String] = Unset)
    (options: ((String, String, String) | (String, String) | String)*)
    : String =

  def italic: String = "\u001b[3m"
  def code: String = "\u001b[2m"
  def bold: String = "\u001b[1m"
  def reset: String = "\u001b[0m"
  
  def prefix: String = module match
    case name: String => bold+name+reset+": "
    case _            => ""
  
  def typeclassName = typeclass match
    case name: String => code+name+reset
    case _            => "typeclass"
  
  def paramName: String = param match
    case name: String => " for the "+code+param+reset+" type parameter"
    case _            => ""
  
  def suggestion: String = suggest match
    case name: String => "\n\nFor many purposes, importing "+code+name+reset+" provides a good default."
    case _            => ""

  def padWidth: Int =
    options.map:
      case (ctx, _)    => ctx.length
      case ctx: String => ctx.length
      case (ctx, _, _) => ctx.length
    .max

  def formatOptions: String = options.map:
    case (context, help, required) =>
      val requirements = "(also needs a contextual "+required.map(code+_+reset).mkString(", ")
      
      "  import "+context+(" "*(padWidth - context.length))+"—— "+italic+help+reset+"\n"+
          italic+requirements+reset
    
    case (context, help) =>
      code+"  import "+context+(" "*(padWidth - context.length))+"—— "+italic+help+reset
    
    case context: String =>
      code+"  import "+context+reset
  .mkString("\n")

  val optionsText = options.length match
    case 0 => ""
    case 1 => "\n\nThis import may fix the problem:\n\n"+formatOptions
    case _ => "\n\nOne of the following imports may fix the problem:\n\n"+formatOptions
  
  prefix+"a contextual "+typeclassName+" instance is required"+paramName+suggestion+optionsText
