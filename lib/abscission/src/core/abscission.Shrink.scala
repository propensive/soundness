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
package abscission

import scala.util.control as suc

import anticipation.*
import contingency.*
import digression.*
import fulminate.*
import galilei.*
import hellenism.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import vacuous.*

object Shrink:
  // ShrinkError → Shrink.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Unsound(problems: List[Text]) extends Reason(1)
      case Failed(trace: StackTrace)     extends Reason(2)

    given communicable: Reason is Communicable =
      case Reason.Unsound(problems) =>
        val count: Int = problems.fold(0): (count, _) => count + 1

        problems match
          case problem :: Nil => m"a problem was found: $problem"
          case problem :: _   => m"$count problems were found, of which the first was: $problem"
          case _              => m"a problem was found"

      case Reason.Failed(_) =>
        m"the shrinker terminated abnormally"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(146, 1, reason.number)
    ( m"the classpath could not be shrunk because $reason" )

  // A package of the classpath, with the number of classes it has there, and the number of
  // those which the closed world keeps.
  case class Package(name: Text, classes: Int, kept: Int)

  // What shrinking a classpath did. `units` are the units which could be recompiled from their
  // TASTy, and `read` those which were, because something reached them. `classes` and `size`
  // are the classpath's, and `kept` and `written` the closed world's; of the classes kept,
  // `recompiled` came from TASTy, `shells` are only a name and its parents, and `copied` are
  // as they were on the classpath. `members` and `removed` count the methods and fields of the
  // recompiled classes. `pinned` are the units which were not recompiled, by request or
  // because they could not be, and `notes` say why of the latter. `packages` are the packages
  // of the classpath, in the order of their names.
  case class Report
    ( units:      Int,
      read:       Int,
      classes:    Int,
      size:       Memory,
      kept:       Int,
      written:    Memory,
      recompiled: Int,
      shells:     Int,
      copied:     Int,
      members:    Int,
      removed:    Int,
      pinned:     List[Fqcn],
      notes:      List[Text],
      packages:   List[Package],
      reasons:    Map[Text, Text] ):

    // Why a class is in the closed world: the chain of classes, each of which refers to the
    // one before it, back to one that was recompiled or was an entry point.
    def why(name: Fqcn): List[Fqcn] =
      def recur(name: Text, seen: Set[Text]): List[Text] = reasons.at(name) match
        case referrer: Text if !seen.has(referrer) => name :: recur(referrer, seen + Set(name))
        case _                                     => List(name)

      recur(name.text, Set()).map { name => safely(Fqcn(name)) }.compact

// How a classpath is shrunk to the closed world its entry points reach. Classes which this
// compiler compiled are recompiled from their TASTy without the members nothing reaches, and
// the rest are kept or left out a class at a time, by the references bytecode makes.
//
// `options` are the compiler options the classes were compiled with, beyond Soundness's own.
// `keep` and `pin` are patterns of class names, in which `*` stands for part of a name and `**`
// for any number of packages: a class which matches `keep` is kept with every member, as one
// which reflection reaches must be, and a unit whose top-level class matches `pin` is not
// recompiled, but copied as it is, with everything it refers to. With `services`, every
// implementation a `META-INF/services` file names is an entry point. With `fine`, a library
// which has no TASTy — one of Java or Kotlin — is taken a class at a time, where it is
// otherwise taken whole as soon as any class of it is reached, since such a library may load
// its own classes by name.
case class Shrink
  ( options:  List[Text] = Nil,
    keep:     List[Text] = Nil,
    pin:      List[Text] = Nil,
    services: Boolean    = true,
    fine:     Boolean    = false ):

  // Writes the closed world of `classpath` which `entryPoints` reach to the directory `out`.
  def apply(classpath: LocalClasspath, entryPoints: List[Fqcn], out: Path on Local)
  :   Shrink.Report raises Shrink.Error =

    val entries: List[Text] = classpath.entries.bind:
      case Classpath.Entry.Directory(directory) => List(directory)
      case Classpath.Entry.Jar(jar)             => List(jar)
      case _                                    => Nil

    val request: Shrinker.Request =
      Shrinker.Request
        ( entries.map(_.s).stdlib,
          out.encode.s,
          options.map(_.s).stdlib,
          entryPoints.map(_.text.s).stdlib,
          keep.map(_.s).stdlib,
          pin.map(_.s).stdlib,
          services,
          fine )

    val report: Shrinker.Report =
      try Shrinker.shrink(request) catch case suc.NonFatal(error) =>
        abort(Shrink.Error(Shrink.Error.Reason.Failed(error.stackTrace)))

    if !report.problems.isEmpty then
      abort(Shrink.Error(Shrink.Error.Reason.Unsound(report.problems.to(List).map(_.tt))))

    val packages: List[Shrink.Package] =
      report.packages.to(List).map: (name, classes, kept) => Shrink.Package(name.tt, classes, kept)

    Shrink.Report
      ( report.units,
        report.read,
        report.classes,
        Memory(report.bytes),
        report.kept,
        Memory(report.written),
        report.recompiled,
        report.shells,
        report.copied,
        report.members,
        report.removed,
        report.pinned.to(List).map { name => safely(Fqcn(name.tt)) }.compact,
        report.notes.to(List).map(_.tt),
        packages,
        report.reasons.to(List).map { (name, referrer) => name.tt -> referrer.tt }.to[Map] )
