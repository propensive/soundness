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

import scala.compiletime.asMatchable

import anticipation.*
import gossamer.*

object Artifact:
  // The tiers of a toolchain's nodes, as data: the kind a format is declared with.
  enum Kind:
    case Source, Ir, Application

  // A format compilers consume: a source language such as Scala, Java or Kotlin.
  trait Source extends Artifact:
    final def kind: Kind = Kind.Source

  // A format holding open-world, pre-link content: an intermediate representation such as
  // classfiles, `.sjsir` or `.nir`, in which libraries compose.
  trait Ir extends Artifact:
    final def kind: Kind = Kind.Ir

  // A closed format a build produces for a host to run: an executable JAR, an APK, a JavaScript
  // bundle or a native binary.
  trait Application extends Artifact:
    final def kind: Kind = Kind.Application

  // An executable JAR, bound to the JDK: the application node reached from `Classfile` through
  // `jarEdges`. Nested here, as `Artifact.Jar`, since `soundness` exports zeppelin's `Jar`.
  object Jar extends Artifact.Application:
    def id: Text = t"jar"

  // A machine-code executable, bound to one operating system and architecture's C library: one
  // application node per target triple, since a binary for one triple is as distinct from a
  // binary for another as a JAR is from a JavaScript bundle—sharing a linker does not make them
  // interchangeable. Nested here, as `Artifact.Binary`, since `soundness` exports monotonous's
  // `Binary`.
  case class Binary(triple: Triple) extends Artifact.Application:
    def id: Text = t"binary-${triple.text}"

  // A format declared as data, by its kind and name, rather than defined in code: as a registry
  // declares the forms no Soundness release knows (a `dockerfile` or `proto` source, a universe
  // with its own carrier, a deliverable with its own host contract). Identity is by kind and name,
  // so a declared format is the same toolchain node as the built-in of that kind and name, and
  // edges registered for either apply to both.
  def apply(kind: Kind, id: Text): Artifact = kind match
    case Kind.Source      => source(id)
    case Kind.Ir          => ir(id)
    case Kind.Application => application(id)

  def source(id: Text): Source = Declared.Source(id)
  def ir(id: Text): Ir = Declared.Ir(id)
  def application(id: Text): Application = Declared.Application(id)

  private object Declared:
    class Source(val id: Text) extends Artifact.Source:
      override def toString: String = s"Artifact.source($id)"

    class Ir(val id: Text) extends Artifact.Ir:
      override def toString: String = s"Artifact.ir($id)"

    class Application(val id: Text) extends Artifact.Application:
      override def toString: String = s"Artifact.application($id)"

// A node of a `Toolchain`: a source language, an intermediate representation or an application
// format. Identity is by kind and `id`, whether the format is built in or declared as data
// (`Artifact(kind, id)`), so a format's user-visible parameters—a JavaScript artifact's module
// system, a WASI artifact's interface version, a native binary's target triple—must be part of
// its `id`, making each parameterization a distinct node. The tiers constrain edges: tools consume
// any format, but only ever produce intermediate representations or applications.
trait Artifact:
  def id: Text
  def kind: Artifact.Kind

  final override def equals(that: Any): Boolean = that.asMatchable match
    case that: Artifact => (this eq that) || kind == that.kind && id == that.id
    case _            => false

  final override def hashCode: Int = kind.ordinal*31 + id.##
