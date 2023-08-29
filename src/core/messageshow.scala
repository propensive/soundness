package fulminate

import anticipation.*

import language.experimental.captureChecking

object MessageShow:
  given text: MessageShow[Text] = Message(_)
  given string: MessageShow[String] = string => Message(string.tt)
  given char: MessageShow[Char] = char => Message(char.toString.tt) // Escape this
  given int: MessageShow[Int] = int => Message(int.toString.tt)
  given long: MessageShow[Long] = long => Message(long.toString.tt)
  given message: MessageShow[Message] = identity(_)

  given listMessage: MessageShow[List[Message]] = messages =>
    Message(List.fill(messages.size)("\n - ".tt) ::: List("".tt), messages)

trait MessageShow[-ValueType]:
  def message(value: ValueType): Message


