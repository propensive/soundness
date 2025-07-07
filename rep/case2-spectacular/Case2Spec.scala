package spectacular
import soundness.*
case class Foo(x: Int, y: Text)
enum Bar:
  case One, Two
object Case2Spec:
  val o: Optional[Text] = Optional(t"x")
