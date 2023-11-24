package fulminate

import anticipation.*

import scala.annotation.*

case class EscapeError(initMessage: Message) extends Error(initMessage)

object TextEscapes:
  def standardEscape
      (text: Text, cur: Int, esc: Boolean)
      : (Int, Int, Boolean) throws EscapeError =
    text.s.charAt(cur) match
      case '\\' if !esc => (-1, cur + 1, true)
      case '\\'         => ('\\', cur + 1, false)
      case 'n' if esc   => ('\n', cur + 1, false)
      case 'r' if esc   => ('\r', cur + 1, false)
      case 'f' if esc   => ('\f', cur + 1, false)
      case 'b' if esc   => ('\b', cur + 1, false)
      case 't' if esc   => ('\t', cur + 1, false)
      case 'u' if esc   => (parseUnicode(text.s.slice(cur + 1, cur + 5)), cur + 5, false)
      case 'e' if esc   => ('\u001b', cur + 1, false)
      case '"' if esc   => ('"', cur + 1, false)
      case '\'' if esc  => ('\'', cur + 1, false)
      case ch if esc    => throw EscapeError(Message(List("the character ".tt, " should not be escaped".tt), List(Message(ch.toString.tt))))
      case ch           => (ch, cur + 1, false)

  private def parseUnicode(chars: String): Char throws EscapeError =
    if chars.length < 4
    then throw EscapeError(Message("the unicode escape is incomplete".tt))
    else Integer.parseInt(chars, 16).toChar

  def escape(text: Text): Text throws EscapeError =
    val buf: StringBuilder = StringBuilder()
    
    @tailrec
    def recur(cur: Int = 0, esc: Boolean): Unit =
      if cur < text.s.length
      then
        val (char, idx, escape) = standardEscape(text, cur, esc)
        if char >= 0 then buf.append(char.toChar)
        recur(idx, escape)
      else if esc then throw EscapeError(Message("the final character cannot be an escape".tt))
    
    recur(0, false)
    
    buf.toString.tt
      
