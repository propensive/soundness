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
import anticipation.*
import spectacular.*
import guillotine.*
import turbulence.*
import rudiments.*
import vacuous.*
import fulminate.*
import ambience.*, systemProperties.virtualMachine
import gossamer.*
import inimitable.*
import hieroglyph.*, charDecoders.utf8, charEncoders.utf8
import perforate.*
import eucalyptus.*
import hellenism.*

import scala.compiletime.*
import scala.quoted.*, staging.*
import scala.reflect.Selectable.reflectiveSelectable

inline def external
    [InputType: JsonSerializer: JsonDeserializer, OutputType]
    (body: Quotes ?=> Expr[InputType => OutputType])
    (using Raises[CompileError])
    : InputType => OutputType =

  val dest = Uuid().show
  val settings: Compiler.Settings = Compiler.Settings.make(Some("/home/propensive/tmp/staging/"+dest), List())
  given compiler: Compiler = Compiler.make(getClass.nn.getClassLoader.nn)(using settings)

  try run: (quotes: Quotes) ?=>
    import quotes.reflect.*
    println("compiling...")
    val entries: List[Text] = new Classloader(getClass.nn.getClassLoader.nn).classpath.or(???).entries.collect:
      case ClasspathEntry.Jarfile(file)        => file
      case ClasspathEntry.Directory(directory) => directory
    
    val classpath: Text = (entries :+ t"/home/propensive/tmp/staging/$dest").join(t":")

    '{ (input: InputType) =>
      object External:
        def run(input: String): String =
          unsafely: (raises) ?=>
            import hieroglyph.charEncoders.utf8
            val readable = summonInline[Readable[Text, Bytes]]
            val in = input.tt
            val json = Json.parse(in)(using readable, raises)
            val deserializer = summonInline[JsonDeserializer[InputType]]
            val param = json.as[InputType](using deserializer)

            val serializer = summonInline[JsonSerializer[OutputType]]

            MinimalSerializer.serialize($body(param).json(using serializer).root).s
      
      val cp: Text = ${Expr(classpath)}
      
      unsafely: raises ?=>
        import hieroglyph.charEncoders.utf8
        val wd: WorkingDirectory = workingDirectories.virtualMachine
        val log: Log[Text] = logging.silent
        val serializer = summonInline[JsonSerializer[InputType]]
        val in = MinimalSerializer.serialize(input.json(using serializer).root).s
        val cmd = Command(t"java", t"-classpath", cp, t"superlunary.SandboxRunner", t"Generated$$Code$$From$$Quoted$$External$$2$$", in)
        val output = cmd.exec[Text]()(using wd, log)

        val readable = summonInline[Readable[Text, Bytes]]
        val json = Json.parse(output)(using readable, raises)
        val deserializer = summonInline[JsonDeserializer[OutputType]]
        json.as[OutputType](using deserializer)
    }
  
  catch case error: dotty.tools.dotc.reporting.UnhandledError =>
    println("Thrown "+error)
    abort(CompileError())

case class CompileError() extends Error(msg"A compilation error occurred")

object SandboxRunner:
  def main(args: Array[String]): Unit =
    val className = args(0)
    val params = args(1)
    val cls = Class.forName(className).nn
    val runnable = cls.newInstance()
    println(runnable.asInstanceOf[{ def run(params: String): String }].run(params))
