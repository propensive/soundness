package jacinta

import compiletime.*

import anticipation.*
import gossamer.*
import rudiments.*
import denominative.*
import spectacular.*
import vacuous.*

object Jacinta:
  opaque type JsonPath = List[Text | Ordinal]

  object JsonPath:
    given JsonPath is Showable = jsonPath =>
      def recur(elements: List[Ordinal | Text], result: Text): Text =
        (elements.asMatchable: @unchecked) match
          case Nil                   => if result.empty then t"." else result
          case Zerary(index) :: tail => recur(tail, t"[${index.n0}]$result")
          case (key: Text) :: tail   => recur(tail, t".$key$result")

      recur(jsonPath.reverse, t"")

    def apply(elements: List[Text | Ordinal] = Nil): JsonPath = elements

  extension (path: JsonPath)
    @targetName("child")
    infix def / (child: Text | Ordinal): JsonPath = child :: path

    def parent: Optional[JsonPath] = if path.isEmpty then Unset else JsonPath(path.tail)
