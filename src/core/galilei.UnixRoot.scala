package galilei

import anticipation.*
import denominative.*
import nomenclature.*
import prepositional.*
import rudiments.*
import contingency.*
import serpentine.*
import gossamer.*

object UnixRoot:
  given (using Tactic[PathError], Tactic[NameError]) => Unix is Navigable by
      Name[Unix] from UnixRoot under
      MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] as navigable =
    new Navigable:
      type Self = Unix
      type Operand = Name[Unix]
      type Source = UnixRoot
      type Constraint = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."]

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Name[Unix] = Name(element)
      def rootLength(path: Text): Int = 1
      def elementText(element: Name[Unix]): Text = element.text
      def rootText(root: Source): Text = t"/"
    
      def root(path: Text): Source =
        if path.at(Prim) == '/' then %
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet %
      
      def caseSensitivity: Case = Case.Sensitive

abstract class UnixRoot() extends Root(t"/", t"/", Case.Sensitive):
  type Platform = Unix

object UnixRootSingleton extends UnixRoot()