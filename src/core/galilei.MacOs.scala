package galilei

import contingency.*
import prepositional.*
import rudiments.*
import nomenclature.*
import serpentine.*
import denominative.*
import gossamer.*
import vacuous.*
import anticipation.*

object MacOs:
  object Root

  abstract class Root() extends serpentine.Root(t"/", t"/", Case.Preserving):
    type Platform = MacOs
  
  object RootSingleton extends Root()

  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  given (using Tactic[PathError], Tactic[NameError]) => MacOs is Navigable by Name[MacOs] from
      Root under Rules as navigable =
    new Navigable:
      type Self = MacOs
      type Operand = Name[MacOs]
      type Source = Root
      type Constraint = Rules

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Name[MacOs] = Name(element)
      def rootLength(path: Text): Int = 1
      def elementText(element: Name[MacOs]): Text = element.text
      def rootText(root: Source): Text = t"/"
    
      def root(path: Text): Source =
        if path.at(Prim) == '/' then $
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet $
      
      def caseSensitivity: Case = Case.Preserving

erased trait MacOs extends Posix
