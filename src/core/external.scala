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

import jacinta.*
import serpentine.*, hierarchies.unix
import anticipation.*
import spectacular.*
import digression.*
import rudiments.*
import vacuous.*
import fulminate.*
import gossamer.*
import anthology.*
import galilei.*
import inimitable.*
import contingency.*
import hellenism.*

import scala.compiletime.*
import scala.quoted.*
import scala.reflect.Selectable.reflectiveSelectable

object DispatchRunner:
  def run(input: String): String =
    val cls = Class.forName("Generated$Code$From$Quoted").nn
    val instance = cls.getDeclaredConstructor().nn.newInstance().nn
    val callable = instance.asInstanceOf[{ def apply(): String => String }]

    callable()(input)
    
  def main(args: Array[String]): Unit =
    val out = System.out.nn
    System.setErr(null)
    System.setOut(null)
    val params = args(0)
    out.println(run(args(0)))

class References():
  private var ref: Optional[Expr[List[Json]]] = Unset
  private var allocations: List[Json] = List()

  def setRef(expr: Expr[List[Json]]): Unit = ref = expr
  def array: Expr[List[Json]] = ref.vouch(using Unsafe)
  def current: Int = allocations.length
  
  def allocate[ValueType](value: => ValueType)(using JsonEncoder[ValueType]): Int =
    allocations.length.also { allocations ::= value.json }
  
  def apply(): Text = allocations.reverse.json.encode

extension [ValueType](value: ValueType)(using Quotes)
  inline def put(using references: References): Expr[ValueType] = '{
    import errorHandlers.throwUnsafely
    ${references.array}(${ToExpr.IntToExpr(references.allocate[ValueType](value))}).as[ValueType]
  }

object Dispatcher:
  private var cache: Map[Codepoint, (Path, Text => Text)] = Map()

trait Dispatcher:
  type Result[OutputType]
  protected def scalac: Scalac[?]
  protected def invoke[OutputType](dispatch: Dispatch[OutputType]): Result[OutputType]
  
  inline def dispatch[OutputType: JsonDecoder](body: References ?=> Quotes ?=> Expr[OutputType])
      [ScalacVersionType <: ScalacVersions]
      (using codepoint: Codepoint, classloader: Classloader)
          : Result[OutputType] raises ScalacError =

    import errorHandlers.throwUnsafely
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
        val out: Unix.Path = % / p"home" / p"propensive" / p"tmp" / p"staging" / PathName(uuid.show)
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

          '{ text => $fromList(text.decodeAs[Json].as[List[Json]]) }

        Dispatcher.cache = Dispatcher.cache.updated(codepoint, (out, fn))
        (out, fn)
    
    val classpath = (classloaders.threadContext.classpath: @unchecked) match
      case LocalClasspath(entries) => LocalClasspath(entries :+ ClasspathEntry.Directory(out.encode))
    
    invoke[OutputType](Dispatch(out, classpath, () => fn(references()).decodeAs[Json].as[OutputType],
        (fn: Text => Text) => fn(references()).decodeAs[Json].as[OutputType]))

case class Dispatch[OutputType]
    (path: Path, classpath: LocalClasspath, local: () => OutputType, remote: (Text => Text) => OutputType):

  def mainClass: Text = t"superlunary.DispatchRunner"
