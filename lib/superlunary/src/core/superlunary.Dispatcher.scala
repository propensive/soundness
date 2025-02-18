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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package superlunary

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import galilei.*
import hellenism.*
import inimitable.*
import jacinta.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*

import scala.quoted.*

object Dispatcher:
  private var cache: Map[Codepoint, (Path, Text => Text)] = Map()

trait Dispatcher:
  type Result[OutputType]
  protected def scalac: Scalac[?]
  protected def invoke[OutputType](dispatch: Dispatch[OutputType]): Result[OutputType]

  type Carrier

  def encoder[ValueType: Type](using Quotes): Expr[ValueType => Text]

  def decoder(using Quotes): Expr[Text => List[Carrier]]

  inline def encode(values: List[Carrier]): Text
  inline def decode[ValueType](value: Text): ValueType

  inline def dispatch[OutputType]
     (body: References[Carrier] ?=> Quotes ?=> Expr[OutputType])
     [ScalacVersionType <: Scalac.All]
     (using codepoint:   Codepoint,
            classloader: Classloader,
            properties:  SystemProperties,
            directory:   TemporaryDirectory)
  :     Result[OutputType] raises CompilerError =
    try
      import strategies.throwUnsafely
      val uuid = Uuid()

      val references = new References[Carrier]()

      val (out, fn): (Path, Text => Text) =
        if Dispatcher.cache.contains(codepoint) then
          val settings: staging.Compiler.Settings =
            staging.Compiler.Settings.make(None, scalac.commandLineArguments.map(_.s))

          given compiler: staging.Compiler = staging.Compiler.make(classloader.java)(using settings)

          staging.withQuotes:
            '{ (array: List[Carrier]) => ${references.set('array) yet body(using references)} }

          Dispatcher.cache(codepoint)

        else
          val out: Path on Linux = temporaryDirectory[Path on Linux] / Name(uuid.show)
          val settings: staging.Compiler.Settings =
            staging.Compiler.Settings.make(Some(out.encode.s), scalac.commandLineArguments.map(_.s))

          given compiler: staging.Compiler = staging.Compiler.make(classloader.java)(using settings)

          val fn: Text => Text = staging.run:
            val fromList: Expr[List[Carrier] => Text] =
              '{  (array: List[Carrier]) =>
                    ${encoder[OutputType]}(${references.set('array) yet body(using references)})  }

            '{ text => $fromList($decoder(text)) }

          Dispatcher.cache = Dispatcher.cache.updated(codepoint, (out, fn))
          (out, fn)

      val classpath = classloaders.threadContext.classpath match
        case classpath: LocalClasspath =>
          LocalClasspath(classpath.entries :+ ClasspathEntry.Directory(out.encode))

        case _ =>
          LocalClasspath:
            ClasspathEntry.Directory(out.encode)
            :: Properties.java.`class`.path().decode[LocalClasspath].entries

      invoke[OutputType]
       (Dispatch
         (out,
          classpath,
          () => decode[OutputType](fn(encode(references()))),
          (fn: Text => Text) => decode[OutputType](fn(encode(references())))))

    catch case throwable: Throwable =>
      println("Failed, somehow")
      throwable.printStackTrace()
      ???

trait JsonDispatcher extends Dispatcher:
  type Carrier = Json
  def encoder[ValueType: Type](using Quotes): Expr[ValueType => Text] =
    '{  (value: ValueType) => value.json.encode  }

  inline def encode(value: List[Json]): Text = value.json.encode

  def decoder(using Quotes): Expr[Text => List[Json]] =
    '{  (text: Text) => unsafely(text.decode[Carrier].as[List[Json]])  }

  inline def decode[ValueType](value: Text): ValueType =
    unsafely(value.decode[Carrier].as[ValueType])
