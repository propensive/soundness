package jacinta

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object Jacinta:
  opaque type JsonPath = List[Text | Int]

  object JsonPath:
    given JsonPath is Showable = jsonPath =>
      def recur(elements: List[Int | Text], result: Text): Text = (elements: @unchecked) match
        case Nil                  => if result.empty then t"." else result
        case (index: Int) :: tail => recur(tail, t"[$index]$result")
        case (key: Text) :: tail  => recur(tail, t".$key$result")

      recur(jsonPath.reverse, t"")

    def apply(elements: List[Text | Int] = Nil): JsonPath = elements

  extension (path: JsonPath)
    @targetName("child")
    infix def / (child: Text | Int): JsonPath = child :: path

    def parent: Optional[JsonPath] = if path.isEmpty then Unset else JsonPath(path.tail)
