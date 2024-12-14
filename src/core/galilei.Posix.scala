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

object Posix:
  abstract class Root() extends serpentine.Root(t"/", t"/", Case.Sensitive):
    type Platform = Posix

  object RootSingleton extends Root()

  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  given (using Tactic[PathError]) => Posix is Radical from (Root on Posix) as radical = new Radical:
    type Self = Posix
    type Source = Root on Posix

    def rootLength(path: Text): Int = 1
    def rootText(root: Source): Text = t"/"

    def root(path: Text): Source =
      if path.at(Prim) == '/' then Posix.RootSingleton
      else raise(PathError(PathError.Reason.InvalidRoot, path)) yet Posix.RootSingleton

  given (using Tactic[NameError]) => Posix is Navigable by Name[Posix] under Rules as navigable =
    new Navigable:
      type Self = Posix
      type Operand = Name[Posix]
      type Constraint = Rules

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Name[Posix] = Name(element)
      def elementText(element: Name[Posix]): Text = element.text
      def caseSensitivity: Case = Case.Sensitive

erased trait Posix extends Filesystem
