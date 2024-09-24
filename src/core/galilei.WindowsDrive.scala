package galilei

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import symbolism.*
import hieroglyph.*
import hypotenuse.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import serpentine.*

import scala.compiletime.*

object WindowsDrive:
  given (using Tactic[PathError]) => WindowsDrive is Navigable as navigable:
    type Operand = Text
    val separator: Text = t"\\"
    val parentElement: Text = t".."
    val selfText: Text = t"."
    
    def prefix(path: Text): WindowsDrive =
      if path.length < 3
      then raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')
      else unsafely(path.at(Prim).vouch).upper.pipe: letter =>
        if path.slice(Sec ~ Ter) == t":\\" && 'A' <= letter <= 'Z' then WindowsDrive(letter)
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')

    def element(element: Text): Text = element
    def prefixLength(path: Text): Int = 3
    def elementText(element: Text): Text = element
    def rootText(drive: WindowsDrive): Text = t"${drive.letter}:\\"

  given [ElementType](using navigable: WindowsDrive is Navigable by ElementType)
      => WindowsDrive is Divisible by ElementType into (Path on WindowsDrive by ElementType) =
    new Divisible:
      type Self = WindowsDrive
      type Result = Path on WindowsDrive by ElementType
      type Operand = ElementType

      def divide(drive: WindowsDrive, child: ElementType): Path on WindowsDrive by ElementType =
        new Path:
          type Platform = WindowsDrive
          type Operand = ElementType
          val root: WindowsDrive = drive
          val descent: List[Operand] = List(child)

case class WindowsDrive(letter: Char):
  def precedes(path: Path on WindowsDrive): Boolean = path.root.letter == letter
