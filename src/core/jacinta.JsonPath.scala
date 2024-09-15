package jacinta

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object JsonPath:
  given JsonPath is Showable = jsonPath =>
    def recur(elements: List[Int | Text], result: Text): Text = elements match
      case Nil                  => if result.empty then t"." else result
      case (index: Int) :: tail => recur(tail, t"[$index]$result")
      case (key: Text) :: tail  => recur(tail, t".$key$result")

    recur(jsonPath.elements, t"")


case class JsonPath(elements: List[Int | Text] = Nil):
  @targetName("child")
  infix def / (child: Int | Text): JsonPath = JsonPath(child :: elements)

  def parent: Optional[JsonPath] = if elements.isEmpty then Unset else JsonPath(elements.tail)
