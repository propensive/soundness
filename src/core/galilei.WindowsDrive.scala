package galilei

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import symbolism.*
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
  // given [ElementType](using navigable: WindowsDrive is Navigable by ElementType)
  //     => WindowsDrive is Divisible by ElementType into (Path on WindowsDrive by ElementType) =
  //   new Divisible:
  //     type Self = WindowsDrive
  //     type Result = Path on WindowsDrive by ElementType
  //     type Operand = ElementType

  //     def divide(drive: WindowsDrive, child: ElementType): Path on WindowsDrive by ElementType =
  //       new Path:
  //         type Platform = WindowsDrive
  //         type Operand = ElementType
  //         val root: WindowsDrive = drive
  //         val descent: List[ElementType] = List(child)

  given (using Tactic[PathError], Tactic[NameError]) => Windows is Navigable by
      Name[Windows] under MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
          MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
          MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "] as navigable =
    new Navigable:
      type Operand = Name[Windows]
      type Self = Windows

      type Constraint = MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
          MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
          MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "]

      val separator: Text = t"\\"
      val parentElement: Text = t".."
      val selfText: Text = t"."
      
      def prefix(path: Text): WindowsDrive =
        if path.length < 3
        then raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')
        else unsafely(path.at(Prim).vouch).upper.pipe: letter =>
          if path.slice(Sec ~ Ter) == t":\\" && 'A' <= letter <= 'Z' then WindowsDrive(letter)
          else raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')

      def element(element: Text): Name[Windows] = Name(element)
      def prefixLength(path: Text): Int = 3
      def elementText(element: Name[Windows]): Text = element.text
      def rootText(drive: Root on Windows): Text = drive match
        case drive: WindowsDrive => t"${drive.letter}:\\"

case class WindowsDrive(letter: Char) extends Root:
  type Operand = Name[Windows]
  type Platform = Windows
  def root = this
