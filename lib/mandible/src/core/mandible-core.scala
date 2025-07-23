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
┃    Soundness, version 0.40.0.                                                                    ┃
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
package mandible

import java.lang.classfile as jlc
import java.lang.classfile.instruction as jlci

import scala.reflect.*
import scala.quoted.*

import anthology.*
import anticipation.*
import contingency.*
import digression.*
import escritoire.*
import escapade.*
import fulminate.*
import galilei.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import inimitable.*
import iridescence.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import tableStyles.minimal
import textMetrics.uniform
import columnAttenuation.ignore
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.disabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.disabled


def disassemble(using codepoint: Codepoint)(code0: Quotes ?=> Expr[Any])(using TemporaryDirectory)
     (using classloader: Classloader)
: Bytecode =

    val uuid = Uuid()
    val out: Path on Linux = unsafely(temporaryDirectory[Path on Linux]/uuid)
    val scalac: Scalac[3.6] = Scalac[3.6](List(scalacOptions.experimental))

    val settings: staging.Compiler.Settings =
      staging.Compiler.Settings.make(Some(out.encode.s), scalac.commandLineArguments.map(_.s))

    given compiler: staging.Compiler = staging.Compiler.make(classloader.java)(using settings)

    unsafely:
      val file: Path on Linux = out/"Generated$$Code$$From$$Quoted.class"
      val code: Quotes ?=> Expr[Unit] = '{ def _code(): Unit = $code0 }
      staging.run(code)
      val classfile: Classfile = new Classfile(file.open(_.read[Bytes]))
      classfile.methods.find(_.name == t"_code$$1").map(_.bytecode).get.vouch.embed(codepoint)


case class ClassfileError()(using Diagnostics)
extends Error(m"there was an error reading the classfile")
