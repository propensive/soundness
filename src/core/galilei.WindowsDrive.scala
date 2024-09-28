package galilei

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import hieroglyph.*
import nomenclature.*
import hypotenuse.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import serpentine.*

import scala.compiletime.*

object WindowsDrive:
  given (using Tactic[PathError], Tactic[NameError]) => Windows is Navigable by
      Name[Windows] under MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
          MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
          MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "] &
          MustNotEqual["CON"] & MustNotEqual["PRN"] & MustNotEqual["AUX"] & MustNotEqual["NUL"] as navigable =
    new Navigable:
      type Operand = Name[Windows]
      type Self = Windows

      type Constraint = MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
          MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
          MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "] &
          MustNotEqual["CON"] & MustNotEqual["PRN"] & MustNotEqual["AUX"] & MustNotEqual["NUL"]

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

      def rootText(drive: Root on Windows): Text = drive match
        case drive: WindowsDrive => t"${drive.letter}:\\"

case class WindowsDrive(letter: Char) extends Root(t"$letter:\\"):
  type Platform = Windows
