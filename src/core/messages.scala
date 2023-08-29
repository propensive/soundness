package fulminate

import anticipation.*

import scala.compiletime.*

import language.experimental.captureChecking

object Message:
  def apply(value: Text): Message = Message(List(value))

  transparent inline def make
      [TupleType <: Tuple]
      (inline subs: TupleType, done: List[Message])
      : List[Message] =
    inline erasedValue[TupleType] match
      case _: (messageType *: tailType) => (subs: @unchecked) match
        case message *: tail =>
          val message2 = message.asInstanceOf[messageType]
          val show = summonInline[MessageShow[messageType]]
          make[tailType](tail.asInstanceOf[tailType], show.message(message2) :: done)

      case _ =>
        done.reverse

case class Message(textParts: List[Text], subs: List[Message] = Nil):
  def fold[RenderType](initial: RenderType)(append: (RenderType, Text, Int) -> RenderType): RenderType =
    def recur(done: RenderType, textTodo: List[Text], subsTodo: List[Message], level: Int): RenderType =
      subsTodo match
        case Nil =>
          append(done, textTodo.head, level)
        
        case sub :: subs =>
          val prefix = recur(append(done, textTodo.head, level), sub.textParts, sub.subs, level + 1)
          recur(prefix, textTodo.tail, subs, level)

    recur(initial, textParts, subs, 0)

  def text: Text = unwrap(fold[String]("") { (acc, next, level) => acc+next })

  def colorText: Text = unwrap:
    fold[String](""): (acc, next, level) =>
      val esc = 27.toChar
      if next.s.isEmpty then acc else level match
        case 0 => acc+next
        case 1 => acc+s"$esc[3m"+next+s"$esc[0m"
        case _ => acc+s"$esc[3m$esc[1m"+next+s"$esc[0m"
  
  def unwrap(string: String): Text =
    val buf: StringBuilder = StringBuilder()
    
    def recur(lines: List[String], break: Boolean): Text = lines match
      case Nil =>
        buf.toString.trim.nn.tt
      
      case line :: tail =>
        if line.forall(_.isWhitespace) then recur(tail, true) else
          buf.append(if !break then " " else "\n")
          buf.append(line.trim.nn.replaceAll("\\s+", " "))
          recur(tail, false)
    
    recur(string.split("\n").nn.map(_.nn).to(List), false)

extension (inline context: StringContext)
  transparent inline def msg[ParamType](inline subs: ParamType = EmptyTuple): Message =
    inline subs.asMatchable match
      case tuple: Tuple =>
        Message(context.parts.map(Text(_)).to(List), Message.make[tuple.type](tuple, Nil))
      
      case other =>
        Message(context.parts.map(Text(_)).to(List), List(summonInline[MessageShow[other.type]].message(other)))
