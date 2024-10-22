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

  given (using Tactic[NameError])
      => Windows is Navigable by Name[Windows] under Rules as navigable =
    new Navigable:
      type Operand = Name[Windows]
      type Self = Windows
      type Constraint = Rules

      val separator: Text = t"\\"
      val parentElement: Text = t".."
      val selfText: Text = t"."
      def element(element: Text): Name[Windows] = Name(element)
      def elementText(element: Name[Windows]): Text = element.text
      def caseSensitivity: Case = Case.Preserving

  given (using Tactic[PathError]) => Windows is Radical from WindowsDrive as radical = new Radical:
    type Self = Windows
    type Source = WindowsDrive

    def root(path: Text): WindowsDrive =
      if path.length < 3
      then raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')
      else unsafely(path.at(Prim).vouch).upper.pipe: letter =>
        if path.segment(Sec ~ Ter) == t":\\" && 'A' <= letter <= 'Z' then WindowsDrive(letter)
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')

    def rootLength(path: Text): Int = 3
    def rootText(drive: Source): Text = t"${drive.letter}:\\"
