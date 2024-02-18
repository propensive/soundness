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

import jacinta.*, jsonPrinters.minimal
import serpentine.*, hierarchies.unix
import anticipation.*, filesystemInterfaces.galileiApi
import spectacular.*
import digression.*
import guillotine.*
import turbulence.*
import rudiments.*
import vacuous.*
import fulminate.*
import ambience.*, systemProperties.virtualMachine
import gossamer.*
import anthology.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import inimitable.*
import hieroglyph.*, charDecoders.utf8, charEncoders.utf8
import contingency.*
import eucalyptus.*
import hellenism.*

import scala.compiletime.*
import scala.collection.mutable as scm
import scala.quoted.*
import scala.reflect.Selectable.reflectiveSelectable

object RemoteRunner:
  def main(args: Array[String]): Unit =
    val className = args(0)
    val params = args(1)
    val cls = Class.forName(className).nn
    val runnable = cls.newInstance()
    println(runnable.asInstanceOf[{ def apply(): String => String }]()(params))

class References():
  private var ref: Optional[Expr[List[Json]]] = Unset
  private var allocations: List[Json] = List()

  def setRef(expr: Expr[List[Json]]): Unit = ref = expr
  def array: Expr[List[Json]] = ref.vouch(using Unsafe)
  def current: Int = allocations.length
  
  def allocate[ValueType](value: => ValueType)(using JsonEncoder[ValueType]): Int =
    allocations.length.also { allocations ::= value.json }
  
  def apply(): Text = allocations.reverse.json.encode

inline given embed
    [ValueType]
    (using references: References, encoder: JsonEncoder[ValueType])
    (using Type[ValueType])
    : ToExpr[ValueType] =

  new ToExpr[ValueType]:
    def apply(value: ValueType)(using quotes: Quotes): Expr[ValueType] =
      import quotes.reflect.*
      val j: Json = value.json
      val l: List[Json] = List(j)
      '{
        import errorHandlers.throwUnsafely
        ${references.array}(${ToExpr.IntToExpr(references.allocate[ValueType](value))}).as[ValueType]
      }

object Remote:
  private var cache: Map[Codepoint, (Path, Text => Text)] = Map()

trait Remote:
  type Result[OutputType]
  def invoke[OutputType](context: InvocationContext[OutputType]): Result[OutputType]
  
  inline def apply[OutputType: JsonDecoder]
      (body: References ?=> Quotes ?=> Expr[OutputType])
      [ScalacVersionType <: ScalacVersions]
      (using codepoint: Codepoint, classloader: Classloader, scalac: Scalac[ScalacVersionType])
      : Result[OutputType] raises ScalacError =
    import errorHandlers.throwUnsafely
    val uuid = Uuid()
    
    val references = new References()
  
    val (out, fn): (Path, Text => Text) =
      if Remote.cache.contains(codepoint) then
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
        Remote.cache(codepoint)
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

        Remote.cache = Remote.cache.updated(codepoint, (out, fn))
        (out, fn)
    
    val classpath = classloaders.threadContext.classpath match
      case LocalClasspath(entries) => LocalClasspath(entries :+ ClasspathEntry.Directory(out.encode))
    
    invoke[OutputType](InvocationContext(out, classpath, () => fn(references()).decodeAs[Json].as[OutputType],
        (fn: Text => Text) => fn(references()).decodeAs[Json].as[OutputType]))

case class InvocationContext
    [OutputType]
    (path: Path, classpath: LocalClasspath, local: () => OutputType, remote: (Text => Text) => OutputType):
  def className: Text = t"Generated$$Code$$From$$Quoted"

object remote extends Remote:
  type Result[OutputType] = OutputType

  def invoke[OutputType](context: InvocationContext[OutputType]): OutputType =
    import workingDirectories.virtualMachine
    import logging.silent
    
    context.remote: input =>
      val cmd = sh"java -classpath ${context.classpath()} superlunary.RemoteRunner ${context.className} $input"
      unsafely(cmd.exec[Text]())
