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

erased trait Windows extends Filesystem

object Windows:
  type Rules = MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
      MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
      MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "] &
      MustNotMatch["(?i)CON(\\.[^.]+)?"] & MustNotEqual["(?i)PRN(\\.[^.]+)?"] &
      MustNotEqual["(?i)AUX(\\.[^.]+)?"] & MustNotEqual["(?i)NUL(\\.[^.]+)?"] &
      MustNotEqual["(?i)COM[0-9](\\.[^.]+)?"] & MustNotEqual["(?i)LPT[0-9](\\.[^.]+)?"]

  given (using Tactic[PathError], Tactic[NameError]) => Windows is Navigable by Name[Windows] from
      WindowsDrive under Rules as navigable =
    new Navigable:
      type Operand = Name[Windows]
      type Self = Windows
      type Source = WindowsDrive
      type Constraint = Rules

      val separator: Text = t"\\"
      val parentElement: Text = t".."
      val selfText: Text = t"."
      
      def root(path: Text): WindowsDrive =
        if path.length < 3
        then raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')
        else unsafely(path.at(Prim).vouch).upper.pipe: letter =>
          if path.slice(Sec ~ Ter) == t":\\" && 'A' <= letter <= 'Z' then WindowsDrive(letter)
          else raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')

      def element(element: Text): Name[Windows] = Name(element)
      def rootLength(path: Text): Int = 3
      def elementText(element: Name[Windows]): Text = element.text
      def rootText(drive: Source): Text = t"${drive.letter}:\\"
      def caseSensitivity: Case = Case.Preserving
