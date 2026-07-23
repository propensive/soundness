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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import java.nio.file as jnf

import scala.util.control as suc

import anticipation.*
import contingency.*
import digression.*
import galilei.*
import hellenism.*
import prepositional.*
import serpentine.*
import vacuous.*

object Linkage:
  // The packaging link family: JAR files, assembled by `Bundler` from a compilation's output.
  // `Form` is the JAR's filename within the output directory.
  given jar: (Linkage[Artifact.Jar] from Universe.Classfile):
    type Origin = Universe.Classfile
    private[anthology] type Form = Text
    private[anthology] def initial: Text = "main.jar".tt

    private[anthology] def link
      ( form:        Text,
        compilation: Compilation[Universe.Classfile],
        entryPoints: List[Linker.EntryPoint],
        out:         Path on Linux )
    :   Path on Linux logs LinkEvent raises LinkError =

      val main: Optional[Fqcn] = entryPoints match
        case Nil         => Unset
        case List(entry) => entry.mainClass
        case _           => abort(LinkError(LinkError.Reason.ManyEntryPoints))

      try
        jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))

        val entries =
          Classpath.Directory(compilation.out) :: compilation.classpath.entries

        unsafely(Bundler.assemble(LocalClasspath(entries*), out / form, main))

      catch case suc.NonFatal(error) =>
        abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))

  // A library JAR is available in every universe, packaging whatever the compilation
  // emitted—classfiles, and any `.sjsir` or `.nir` alongside them—for downstream assembly.
  // Entry points do not apply and are ignored.
  given library: [universe <: Universe] => (Linkage[Artifact.Library[universe]] from universe):
    type Origin = universe
    private[anthology] type Form = Text
    private[anthology] def initial: Text = "main.jar".tt

    private[anthology] def link
      ( form:        Text,
        compilation: Compilation[universe],
        entryPoints: List[Linker.EntryPoint],
        out:         Path on Linux )
    :   Path on Linux logs LinkEvent raises LinkError =

      try
        jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))
        val entries = List(Classpath.Directory(compilation.out))
        unsafely(Bundler.assemble(LocalClasspath(entries*), out / form, Unset))

      catch case suc.NonFatal(error) =>
        abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))

// Determines how an artifact is linked from a compilation in its origin universe: the
// underlying linker-configuration type (`Form`), the configuration the artifact mandates, and
// the link pipeline itself. Instances exist only for artifacts that are currently producible,
// and those whose prerequisites can be absent are conditional upon evidence of them: the WASI
// instance (in the `link` module) upon a `WasiToolchain` and a `WitWorld`, and the
// native-binary instance (in the `nir` module) upon probing the C toolchain.
trait Linkage[artifact <: Artifact] extends Provenance[artifact]:
  private[anthology] type Form
  private[anthology] def initial: Form

  private[anthology] def link
    ( form:        Form,
      compilation: Compilation[Origin],
      entryPoints: List[Linker.EntryPoint],
      out:         Path on Linux )
  :   Path on Linux logs LinkEvent raises LinkError
