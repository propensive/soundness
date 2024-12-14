package galilei

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

object Linux:
  object Root

  abstract class Root() extends serpentine.Root(t"/", t"/", Case.Sensitive):
    type Platform = Linux

  object RootSingleton extends Root()

  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  given (using Tactic[PathError]) => Linux is Radical from Root as radical =
    new Radical:
      type Self = Linux
      type Source = Root

      def rootLength(path: Text): Int = 1
      def rootText(root: Source): Text = t"/"

      def root(path: Text): Source =
        if path.at(Prim) == '/' then %
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet %

  given (using Tactic[NameError]) => Linux is Navigable by Name[Linux] under Rules as navigable =
    new Navigable:
      type Self = Linux
      type Operand = Name[Linux]
      type Constraint = Rules

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."
      def element(element: Text): Name[Linux] = Name(element)
      def elementText(element: Name[Linux]): Text = element.text
      def caseSensitivity: Case = Case.Sensitive

erased trait Linux extends Posix
