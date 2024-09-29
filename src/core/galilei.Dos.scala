package galilei

import contingency.*
import prepositional.*
import rudiments.*
import nomenclature.*
import serpentine.*
import denominative.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import vacuous.*
import spectacular.*
import anticipation.*

erased trait Dos extends Filesystem

object Dos:
  type Rules = MustMatch["[^.]{1,8}(\\.[^.]{1,3})?"] & MustNotContain[" "] & MustMatch["[!-~]*"]

  given (using Tactic[PathError], Tactic[NameError]) => Dos is Navigable by Name[Dos] from
      DosDrive under Rules as navigable =
    new Navigable:
      type Operand = Name[Dos]
      type Self = Dos
      type Source = DosDrive
      type Constraint = Rules

      val separator: Text = t"\\"
      val parentElement: Text = t".."
      val selfText: Text = t"."
      
      def root(path: Text): DosDrive =
        if path.length < 3
        then raise(PathError(PathError.Reason.InvalidRoot, path)) yet DosDrive('Z')
        else unsafely(path.at(Prim).vouch).upper.pipe: letter =>
          if path.slice(Sec ~ Ter) == t":\\" && 'A' <= letter <= 'Z' then DosDrive(letter)
          else raise(PathError(PathError.Reason.InvalidRoot, path)) yet DosDrive('Z')

      def element(element: Text): Name[Dos] = Name(element)
      def rootLength(path: Text): Int = 3
      def elementText(element: Name[Dos]): Text = element.text
      def rootText(drive: Source): Text = t"${drive.letter}:\\"
      def caseSensitivity: Case = Case.Upper
