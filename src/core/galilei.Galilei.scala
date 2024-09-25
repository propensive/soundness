package galilei

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import symbolism.*
import rudiments.*
import vacuous.*
import serpentine.*

import scala.reflect.*

object Galilei:
  @targetName("UnixRoot")
  object % extends Pathlike:
    protected def pathRoot: AnyRef = this
    protected def pathDescent: List[Any] = Nil

    def precedes(path: Path on %.type): Boolean = true

    given [ElementType](using navigable: %.type is Navigable by ElementType)
        => %.type is Divisible by ElementType into (Path on %.type by ElementType) =
      new Divisible:
        type Self = %.type
        type Result = Path on %.type by ElementType
        type Operand = ElementType

        def divide(root: %.type, child: ElementType): Path on %.type by ElementType =
          new Path:
            type Platform = %.type
            type Operand = ElementType
            val root: %.type = %
            val descent: List[Operand] = List(child)

    given (using Tactic[PathError]) => %.type is Navigable by Text as navigable =
      new Navigable:
        type Self = %.type
        type Operand = Text

        val separator: Text = t"/"
        val parentElement: Text = t".."
        val selfText: Text = t"."

        def element(element: Text): Text = element
        def prefixLength(path: Text): Int = 1
        def elementText(element: Text): Text = element
        def rootText(root: %.type): Text = t"/"
      
        def prefix(path: Text): %.type =
          if path.at(Prim) == '/' then %
          else raise(PathError(PathError.Reason.InvalidRoot, path)) yet %
