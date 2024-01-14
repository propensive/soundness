/*
    Nettlesome, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import gossamer.*
import rudiments.*
import hypotenuse.*
import vacuous.*
import fulminate.*
import perforate.*
import anticipation.*
import contextual.*
import spectacular.*

import scala.quoted.*

object HostnameError:
  enum Reason:
    case LongDnsLabel(label: Text)
    case LongHostname
    case InvalidChar(char: Char)
    case EmptyDnsLabel(n: Int)
    case InitialDash(label: Text)

  object Reason:
    given Communicable[Reason] =
      case LongDnsLabel(label) => msg"the DNS label $label is longer than 63 characters"
      case LongHostname        => msg"the hostname is longer than 253 characters"
      case InvalidChar(char)   => msg"the character $char is not allowed in a hostname"
      case EmptyDnsLabel(n)    => msg"a DNS label cannot be empty"
      case InitialDash(label)  => msg"the DNS label $label begins with a dash which is not allowed"

import HostnameError.Reason.*

case class HostnameError(reason: HostnameError.Reason)
extends Error(msg"the hostname is not valid because $reason")

object Hostname:
  given Show[Hostname] = _.dnsLabels.map(_.show).join(t".")
  
  def expand(context: Expr[StringContext])(using Quotes): Expr[Hostname] = failCompilation:
    Expr(Hostname.parse(context.valueOrAbort.parts.head.tt))

  given toExpr: ToExpr[Hostname] with
    def apply(hostname: Hostname)(using Quotes): Expr[Hostname] =
      val labels = Varargs:
        hostname.dnsLabels.map: label =>
          '{DnsLabel(${Expr(label.text)})}
      
      '{Hostname($labels*)}

  def parse(text: Text): Hostname raises HostnameError =
    val buffer: StringBuilder = StringBuilder()

    def recur(index: Int, dnsLabels: List[DnsLabel]): Hostname = safely(text(index)) match
      case '.' | Unset =>
        val label = buffer.text
        if label.empty then raise(HostnameError(EmptyDnsLabel(dnsLabels.length)))(())
        if label.length > 63 then raise(HostnameError(LongDnsLabel(label)))(())
        if label.starts(t"-") then raise(HostnameError(InitialDash(label)))(())
        val dnsLabels2 = DnsLabel(label) :: dnsLabels
        buffer.clear()
        
        if index < text.length then recur(index + 1, dnsLabels2) else
          if dnsLabels2.map(_.text.length + 1).sum > 254 then raise(HostnameError(LongHostname))(())
          Hostname(dnsLabels2.reverse*)
      
      case char: Char =>
        if char == '-' || ('A' <= char <= 'Z') || ('a' <= char <= 'z') || char.isDigit
        then buffer.append(char)
        else raise(HostnameError(InvalidChar(char)))(())
        recur(index + 1, dnsLabels)
    
    recur(0, Nil)

case class Hostname(dnsLabels: DnsLabel*) extends Shown[Hostname]
