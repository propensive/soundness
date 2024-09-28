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

object Unix:
  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  given (using Tactic[PathError], Tactic[NameError]) => Unix is Navigable by Name[Unix] from
      UnixRoot under Rules as navigable =
    new Navigable:
      type Self = Unix
      type Operand = Name[Unix]
      type Source = UnixRoot
      type Constraint = Rules

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

erased trait Unix extends Filesystem
