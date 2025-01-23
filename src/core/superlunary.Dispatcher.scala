/*
    Superlunary, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anthology.*
import anticipation.*
import contingency.*
import digression.*
import fulminate.*
import galilei.*
import gossamer.*
import hellenism.*
import inimitable.*
import jacinta.*
import prepositional.*
import rudiments.*
import serpentine.*, pathNavigation.linux
import spectacular.*
import vacuous.*

import scala.compiletime.*
import scala.quoted.*
import scala.reflect.Selectable.reflectiveSelectable

object Dispatcher:
  private var cache: Map[Codepoint, (Path, Text => Text)] = Map()

trait Dispatcher:
  type Result[OutputType]
  protected def scalac: Scalac[?]
  protected def invoke[OutputType](dispatch: Dispatch[OutputType]): Result[OutputType]

  inline def dispatch[OutputType: Decodable in Json]
     (body: References ?=> Quotes ?=> Expr[OutputType])
     [ScalacVersionType <: Scalac.All]
     (using codepoint: Codepoint, classloader: Classloader)
  :     Result[OutputType] raises CompilerError =

    import strategies.throwUnsafely
    val uuid = Uuid()

    val references = new References()

    val (out, fn): (Path, Text => Text) =
      if Dispatcher.cache.contains(codepoint) then
        val settings: staging.Compiler.Settings =
          staging.Compiler.Settings.make(None, scalac.commandLineArguments.map(_.s))

        given compiler: staging.Compiler = staging.Compiler.make(classloader.java)(using settings)

        staging.withQuotes:
          '{ (array: List[Json]) =>
            ${
              references.setRef('array)
              body(using references)
            }
          }
        Dispatcher.cache(codepoint)
      else
        val out: Path = % / p"home" / p"propensive" / p"tmp" / p"staging" / Name(uuid.show)
        val settings: staging.Compiler.Settings =
          staging.Compiler.Settings.make(Some(out.encode.s), scalac.commandLineArguments.map(_.s))

        given compiler: staging.Compiler = staging.Compiler.make(classloader.java)(using settings)

        val fn: Text => Text = staging.run:
          val fromList: Expr[List[Json] => Text] = '{ (array: List[Json]) =>
            ${
              references.setRef('array)
              body(using references)
            }.json.encode
          }

          '{ text => $fromList(text.decode[Json].as[List[Json]]) }

        Dispatcher.cache = Dispatcher.cache.updated(codepoint, (out, fn))
        (out, fn)

    val classpath = classloaders.threadContext.classpath.absolve match
      case classpath: LocalClasspath => LocalClasspath(classpath.entries :+ ClasspathEntry.Directory(out.encode))

    invoke[OutputType]
     (Dispatch
       (out,
        classpath, () => fn(references()).decode[Json].as[OutputType],
        (fn: Text => Text) => fn(references()).decode[Json].as[OutputType]))
