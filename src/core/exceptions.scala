package rudiments

import language.experimental.captureChecking

case class ErrorMessage(textParts: List[Text], subs: List[Text]):
  def fold
      [ResultType]
      (initial: ResultType)
      (textFn: (ResultType, Text) -> ResultType, subFn: (ResultType, Text) -> ResultType)
      : ResultType =
    def recur(textParts: List[Text], subs: List[Text], current: ResultType): ResultType = subs match
      case Nil         => textFn(current, textParts.head)
      case sub :: subs => recur(textParts.tail, subs, subFn(textFn(current, textParts.head), sub))

    recur(textParts, subs, initial)
  
  def text: Text = Text(fold("")(_+_, _+_))

transparent abstract class Error(val message: ErrorMessage, val cause: Maybe[Error] = Unset)
extends Exception():
  this: Error =>
  def fullClass: List[Text] = List(getClass.nn.getName.nn.split("\\.").nn.map(_.nn).map(Text(_))*)
  def className: Text = fullClass.last
  def component: Text = fullClass.head

  override def getMessage: String = component.s+": "+message.text
  override def getCause: Exception | Null = cause.option.getOrElse(null)
  
  def explanation: Maybe[Text] = Unset

object Mistake:
  def apply(error: Exception): Mistake =
    Mistake(s"rudiments: an ${error.getClass.getName} exception was thrown when this was not "+
        s"believed to be possible; the error was '${error.getMessage}'")

case class Mistake(message: String) extends java.lang.Error(message)

case class StreamCutError(total: ByteSize)
extends Error(ErrorMessage(List(Text("the stream was cut prematurely after "), Text("")),
    List(total.text)))
