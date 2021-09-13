package gossamer

opaque type Text = String

object Text:
  def apply(str: String): Text = str

  extension (text: Text)
    def +(other: Text) = text+other
    def slice(index: Int) = text.substring(index)
    def slice(from: Int, to: Int) = text.substring(from, to)
    def string: String = text

trait Show[T]:
  def show(value: T): Text

object Show:
  given Show[Int] = num => Text(num.toString)
  given Show[Short] = num => Text(num.toString)
  given Show[Long] = num => Text(num.toString)
  given Show[Byte] = num => Text(num.toString)
  given Show[Boolean] = if _ then Text("true") else Text("false")