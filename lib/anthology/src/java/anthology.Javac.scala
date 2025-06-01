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
┃    Soundness, version 0.32.0.                                                                    ┃
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

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import gossamer.*
import hellenism.*
import parasite.*
import prepositional.*
import vacuous.*

import scala.jdk.CollectionConverters.*
import scala.util.control as suc

import javax.tools as jt
import java.util as ju
import java.net as jn

object Javac:
  private var Javac: jt.JavaCompiler = jt.ToolProvider.getSystemJavaCompiler().nn
  def refresh(): Unit = Javac = jt.ToolProvider.getSystemJavaCompiler().nn
  def compiler(): jt.JavaCompiler = Javac

case class Javac(options: List[JavacOption]):
  case class JavaSource(name: Text, code: Text)
  extends jt.SimpleJavaFileObject
            ( jn.URI.create(t"string:///$name".s), jt.JavaFileObject.Kind.SOURCE ):
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = code.s

  def apply(classpath: LocalClasspath)[path: Abstractable across Paths into Text]
        ( sources: Map[Text, Text], out: path )
        ( using SystemProperties, Monitor, Codicil )
  : CompileProcess logs CompileEvent raises CompilerError =

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

            val codeRange: Optional[CodeRange] =
              if diagnostic.getPosition == jt.Diagnostic.NOPOS then Unset else
                CodeRange
                  ( diagnostic.getLineNumber.toInt,
                    diagnostic.getColumnNumber.toInt,
                    diagnostic.getLineNumber.toInt,
                    (diagnostic.getColumnNumber + diagnostic.getEndPosition
                    - diagnostic.getPosition).toInt )

            process.put:
              Notice
                ( importance,
                  "name".tt,
                  diagnostic.getMessage(ju.Locale.getDefault()).nn.tt,
                  codeRange )

      val options = List(t"-classpath", classpath(), t"-d", out.generic)
      val javaSources = sources.map(JavaSource(_, _)).asJava
      Log.info(CompileEvent.Running(List(t"javac", options.join(t" "))))

      async:
        try
          val success =
            process.put(CompileProgress(0.1, t"javac"))

            Javac.compiler()
            . getTask(null, null, diagnostics, options.map(_.s).asJava, null, javaSources)
            . nn.call().nn

          if success then process.put(CompileProgress(1.0, t"javac"))

          process.put(if success then CompileResult.Success else CompileResult.Failure)

        catch case suc.NonFatal(error) =>
          Javac.refresh()
          Log.warn(CompileEvent.CompilerCrash)
          process.put(CompileResult.Crash(error.stackTrace))

      process
