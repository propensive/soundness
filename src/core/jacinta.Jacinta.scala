package jacinta

import compiletime.*

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object Jacinta:
  opaque type JsonPointer = List[Text | Ordinal]

  object JsonPointer:
    given JsonPointer is Showable = pointer =>
      def recur(elements: List[Ordinal | Text], result: Text): Text =
        (elements.asMatchable: @unchecked) match
          case Nil                   => if result.empty then t"." else result
          case Zerary(index) :: tail => recur(tail, t"[${index.n0}]$result")
          case (key: Text) :: tail   => recur(tail, t".$key$result")

      recur(pointer.reverse, t"")

    def apply(elements: List[Text | Ordinal] = Nil): JsonPointer = elements

  extension (path: JsonPointer)
    @targetName("child")
    infix def / (child: Text | Ordinal): JsonPointer = child :: path

    def parent: Optional[JsonPointer] = if path.isEmpty then Unset else JsonPointer(path.tail)
