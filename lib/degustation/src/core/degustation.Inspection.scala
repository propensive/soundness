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
package degustation

import scala.quoted.Quotes
import scala.tasty.inspector as sti

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import vacuous.*

import Inspection.Error.Reason

// Drives the compiler's own TASTy unpickler — via the tasty-inspector wrapper — over a set of
// `.tasty` files against a dependency classpath, and atomizes the API surface found there. The
// classpath must carry everything the inspected files reference, including the Scala library;
// at assembly and publish time the buildpath supplies it.
object Inspection:

  def atomize(tastyFiles: List[Text], classpath: List[Text])
  :   List[ScalaAtom] raises Inspection.Error =

    var result: scala.List[ScalaAtom] = scala.Nil
    var failure: Optional[Text] = Unset

    object inspector extends sti.Inspector:
      def inspect(using quotes: Quotes)(tastys: scala.List[sti.Tasty[quotes.type]]): Unit =
        try result = Atomizer.atomize(tastys.map(_.ast))
        catch case error: Exception =>
          failure = Text(error.getMessage.nn)

    val ok =
      try
        sti.TastyInspector.inspectAllTastyFiles
          (tastyFiles.stdlib.map(_.s), scala.Nil, classpath.stdlib.map(_.s))(inspector)

      catch case error: Exception =>
        abort(Inspection.Error(Reason.InspectionFailed(Text(error.getMessage.nn))))

    failure.let: detail => abort(Inspection.Error(Reason.Unencodable(detail)))
    if !ok then abort(Inspection.Error(Reason.InspectionFailed(t"the compiler reported errors")))

    List.from(result)

  // DegustationError → Inspection.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case InspectionFailed(detail: Text)  extends Reason(1)
      case Unencodable(construct: Text)    extends Reason(2)
      case DuplicateKey(key: Text)         extends Reason(3)

    given communicable: Reason is Communicable =
      case Reason.InspectionFailed(detail) => m"the TASTy could not be inspected: $detail"
      case Reason.Unencodable(construct)   => m"no canonical encoding is defined for $construct"
      case Reason.DuplicateKey(key)        => m"the key $key was produced twice"

  case class Error(reason: Inspection.Error.Reason)(using Diagnostics)
  extends fulminate.Error(642, reason.number)(m"the Scala discipline failed because $reason")
