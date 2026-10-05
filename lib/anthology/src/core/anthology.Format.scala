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

object Format:
  // The tiers of a toolchain's nodes, as data: the kind a format is declared with.
  enum Kind:
    case Source, Ir, Application

  // A format compilers consume: a source language such as Scala, Java or Kotlin.
  trait Source extends Format:
    final def kind: Kind = Kind.Source

  // A format holding open-world, pre-link content: an intermediate representation such as
  // classfiles, `.sjsir` or `.nir`, in which libraries compose.
  trait Ir extends Format:
    final def kind: Kind = Kind.Ir

  // A closed format a build produces for a host to run: an executable JAR, an APK, a JavaScript
  // bundle or a native binary.
  trait Application extends Format:
    final def kind: Kind = Kind.Application

  // A format declared as data, by its kind and name, rather than defined in code: as a registry
  // declares the forms no Soundness release knows (a `dockerfile` or `proto` source, a universe
  // with its own carrier, a deliverable with its own host contract). Identity is by kind and name,
  // so a declared format is the same toolchain node as the built-in of that kind and name, and
  // edges registered for either apply to both.
  def apply(kind: Kind, id: Text): Format = kind match
    case Kind.Source      => source(id)
    case Kind.Ir          => ir(id)
    case Kind.Application => application(id)

  def source(id: Text): Source = Declared.Source(id)
  def ir(id: Text): Ir = Declared.Ir(id)
  def application(id: Text): Application = Declared.Application(id)

  private object Declared:
    class Source(val id: Text) extends Format.Source:
      override def toString: String = s"Format.source($id)"

    class Ir(val id: Text) extends Format.Ir:
      override def toString: String = s"Format.ir($id)"

    class Application(val id: Text) extends Format.Application:
      override def toString: String = s"Format.application($id)"

// A node of a `Toolchain`: a source language, an intermediate representation or an application
// format. Identity is by kind and `id`, whether the format is built in or declared as data
// (`Format(kind, id)`), so a format's user-visible parameters—a JavaScript artifact's module
// system, a WASI artifact's interface version, a native binary's target triple—must be part of
// its `id`, making each parameterization a distinct node. The tiers constrain edges: tools consume
// any format, but only ever produce intermediate representations or applications. Unexported:
// `soundness` already exports zephyrine's `Format`.
@unexported
trait Format:
  def id: Text
  def kind: Format.Kind

  final override def equals(that: Any): Boolean = that.asMatchable match
    case that: Format => (this eq that) || kind == that.kind && id == that.id
    case _            => false

  final override def hashCode: Int = kind.ordinal*31 + id.hashCode
