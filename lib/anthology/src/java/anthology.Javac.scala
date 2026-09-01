                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package anthology

import java.net as jn
import java.util as ju
import javax.tools as jt

import scala.jdk.CollectionConverters.*
import scala.util.control as suc

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import digression.*
import gossamer.*
import hellenism.*
import parasite.*
import prepositional.*
import rudiments.{map, to}
import vacuous.*

object Javac:
  private var Javac: jt.JavaCompiler = jt.ToolProvider.getSystemJavaCompiler().nn

  def refresh(): Unit = Javac = jt.ToolProvider.getSystemJavaCompiler().nn
  def compiler(): jt.JavaCompiler = Javac

  // JavacOption → Javac.Option
  case class Option(flags: Text*)

case class Javac(options: List[Javac.Option]):
  case class JavaSource(name: Text, code: Text)
  extends jt.SimpleJavaFileObject
    ( jn.URI.create(t"string:///$name".s), jt.JavaFileObject.Kind.SOURCE ):
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = code.s

  def apply(classpath: LocalClasspath)[path: Abstractable across Paths to Text]
    ( sources: Map[Text, Text], out: path )
    ( using System, Monitor, Probate )
  :   CompileProcess logs CompileEvent raises Compiler.Error =

    Log.info(CompileEvent.Start)
    val process: CompileProcess = CompileProcess()

    val diagnostics = new jt.DiagnosticListener[jt.JavaFileObject]:
      def report(diagnostic: jt.Diagnostic[? <: jt.JavaFileObject] | Null): Unit =
        if diagnostic != null then
          val importance = diagnostic.getKind match
            case jt.Diagnostic.Kind.ERROR             => Importance.Error
            case jt.Diagnostic.Kind.WARNING           => Importance.Warning
            case jt.Diagnostic.Kind.MANDATORY_WARNING => Importance.Warning
            case _                                    => Importance.Info

          val span: Optional[Span] =
            if diagnostic.getPosition == jt.Diagnostic.NOPOS then Unset else
              Span.line
                ( diagnostic.getLineNumber.toInt.z,
                  diagnostic.getColumnNumber.toInt.z,
                  (diagnostic.getEndPosition - diagnostic.getPosition).toInt )

          process.put:
            Notice
              ( importance,
                "name".tt,
                diagnostic.getMessage(ju.Locale.getDefault()).nn.tt,
                span )

    val options = List(t"-classpath", classpath(), t"-d", out.generic)
    // `.stdlib`: `javac`'s `getTask` takes a `java.lang.Iterable`, which `asJava` needs a stdlib
    // collection to build.
    val javaSources = sources.to[List].map(JavaSource(_, _)).stdlib.asJava
    Log.info(CompileEvent.Running(List(t"javac", options.join(t" "))))

    async:
      try
        val success: Boolean =
          process.put(CompileProgress(0.1, t"javac"))

          // `.stdlib`: as above — `asJava` needs a stdlib collection for `getTask`'s Java API.
          Javac.compiler()
          . getTask(null, null, diagnostics, options.map(_.s).stdlib.asJava, null, javaSources)
          . nn.call().nn.booleanValue()

        if success then process.put(CompileProgress(1.0, t"javac"))

        process.put(if success then CompileResult.Success else CompileResult.Failure)

      catch case suc.NonFatal(error) =>
        Javac.refresh()
        Log.fail(CompileEvent.CompilerCrash)
        process.put(CompileResult.Crash(error.stackTrace))

    process
