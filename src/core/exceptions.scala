package rudiments

case class ErrorMessage[+T <: Tuple](text: Seq[Text], parts: T)

abstract class Error[T <: Tuple](val msg: ErrorMessage[T], val cause: Maybe[Error[?]] = Unset)
extends Exception():
  this: Error[T] =>
  def fullClass: List[Text] = List(getClass.nn.getName.nn.split("\\.").nn.map(_.nn).map(Text(_))*)
  def className: Text = fullClass.last
  def component: Text = fullClass.head

  override def getMessage: String = component.s+": "+message
  override def getCause: Exception | Null = cause.option.getOrElse(null)

  def message: Text =
    def recur[T <: Tuple](tuple: T, text: Seq[Text], value: String = ""): String = tuple match
      case EmptyTuple   => value+text.headOption.getOrElse(Text(""))
      case head *: tail => recur(tail, text.tail, value+text.head+head.toString)

    Text(recur(msg.parts, msg.text))

  def explanation: Maybe[Text] = Unset

object Mistake:
  def apply(error: Exception): Mistake =
    Mistake(s"rudiments: an ${error.getClass.getName} exception was thrown when this was not "+
        s"believed to be possible; the error was '${error.getMessage}'")

case class Mistake(message: String) extends java.lang.Error(message)
