package nettlesome

import gossamer.*
import rudiments.*
import fulminate.*
import perforate.*
import anticipation.*
import contextual.*
import spectacular.*

object HostnameError:
  enum Reason:
    case LongDnsLabel(label: Text)
    case LongHostname
    case InvalidChar(char: Char)
    case EmptyDnsLabel(n: Int)
    case InitialDash(label: Text)

  object Reason:
    given MessageShow[Reason] =
      case LongDnsLabel(label) => msg"the DNS label $label is longer than 63 characters"
      case LongHostname        => msg"the hostname is longer than 253 characters"
      case InvalidChar(char)   => msg"the character $char is not allowed in a hostname"
      case EmptyDnsLabel(n)    => msg"a DNS label cannot be empty"
      case InitialDash(label)  => msg"the DNS label $label begins with a dash which is not allowed"

import HostnameError.Reason.*

case class HostnameError(reason: HostnameError.Reason)
extends Error(msg"the hostname is not valid because $reason")

object DnsLabel:
  given show: Show[DnsLabel] = _.text

case class DnsLabel(text: Text)

object Hostname:
  given Show[Hostname] = _.parts.map(_.show).join(t".")
  
  def parse(text: Text): Hostname raises HostnameError =
    val buffer: StringBuilder = StringBuilder()

    def recur(index: Int, parts: List[DnsLabel]): Hostname = safely(text(index)) match
      case '.' | Unset =>
        val label = buffer.toString.tt
        if label.empty then raise(HostnameError(EmptyDnsLabel(parts.length)))(())
        if label.length > 63 then raise(HostnameError(LongDnsLabel(label)))(())
        if label.starts(t"-") then raise(HostnameError(InitialDash(label)))(())
        val parts2 = DnsLabel(label) :: parts
        buffer.clear()
        if index < text.length then recur(index + 1, parts2) else
          if parts2.map(_.text.length + 1).sum > 254 then raise(HostnameError(LongHostname))(())
          Hostname(parts2.reverse*)
      
      case char: Char =>
        if char == '-' || ('A' <= char <= 'Z') || ('a' <= char <= 'z') || char.isDigit
        then buffer.append(char)
        else raise(HostnameError(InvalidChar(char)))(())
        recur(index + 1, parts)
    recur(0, Nil)

case class Hostname(parts: DnsLabel*) extends Shown[Hostname]
