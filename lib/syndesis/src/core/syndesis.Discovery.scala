                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package syndesis

import scala.caps

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*

import Discovery.Error.Reason.*

// The vocabulary of DNS-based service discovery (RFC 6763) and the seam behind it: a service
// type, an instance of it, the TXT record's key/value pairs, what an advertisement describes,
// what a resolution yields, and the `Backend` that advertises, browses and resolves — the
// socket-based mDNS responder in `Mdns`, or a system responder in a later backend. The names
// involved are computed when a value is constructed, under its validation, so that every later
// use of them is total.
object Discovery:
  val local: Dns.Name = dns"local"

  // A name from validated parts; a DNS limit breached in the process is reported in DNS-SD
  // terms, naming the offending text.
  private def named(text: Text, labels: List[Text]): Dns.Name raises Discovery.Error =
    recover:
      case error: Dns.Error =>
        given diagnostics: Diagnostics = error.diagnostics
        abort(Discovery.Error(InvalidName(text)))

    . protect(Dns.Name(labels*))

  // ── Service types ───────────────────────────────────────────────────────────────────────────
  object Service:
    // The meta-query name under which every service type on the link is enumerated (RFC 6763
    // §9).
    val enumeration: Dns.Name = dns"_services._dns-sd._udp.local"

    // `_fury._tcp`: the name is a registered service name of up to fifteen characters (RFC 6763
    // §7), without its leading underscore.
    def apply(name: Text, protocol: Tcp.type | Udp.type, subtypes: List[Text] = Nil)
    :   Service raises Discovery.Error =

      if name.nil || name.length > 15 || name.starts(t"_")
      then abort(Discovery.Error(InvalidService(name)))

      val dnsName = named(name, List(t"_$name", protocolLabel(protocol), t"local"))
      new Service(name, protocol, subtypes)(dnsName)

    // The inverse of `dnsName`: `_fury._tcp.local` is the fury service over TCP.
    def parse(name: Dns.Name): Optional[Service] = name.labels match
      case service :: protocol :: domain :: Nil if Dns.Name.fold(domain) == t"local" =>
        val protocol2: Optional[Tcp.type | Udp.type] = Dns.Name.fold(protocol) match
          case t"_tcp" => Tcp
          case t"_udp" => Udp
          case _       => Unset

        if !service.starts(t"_") then Unset
        else protocol2.let: protocol => safely(Service(service.skip(1), protocol))

      case _ => Unset

    private def protocolLabel(protocol: Tcp.type | Udp.type): Text = protocol match
      case Tcp => t"_tcp"
      case Udp => t"_udp"

    given showable: Service is Showable = service =>
      t"_${service.name}.${protocolLabel(service.protocol)}"

  case class Service private (name: Text, protocol: Tcp.type | Udp.type, subtypes: List[Text])
    ( val dnsName: Dns.Name ):

    // `_printer._sub._fury._tcp.local`: the name a subtype's instances are browsed under.
    def subtypeName(subtype: Text): Dns.Name raises Discovery.Error =
      named(subtype, t"_$subtype" :: t"_sub" :: dnsName.labels)

    // A port of this service's transport, from the bare number an SRV record carries.
    private[syndesis] def port(number: Int): Port raises Port.Error = protocol match
      case Tcp => Port[Tcp](number)
      case Udp => Port[Udp](number)

  // ── Instances ───────────────────────────────────────────────────────────────────────────────
  object Instance:
    // The label is one DNS label of UTF-8 (RFC 6763 §4.1.1): dots and spaces included, no
    // escaping, up to 63 bytes.
    def apply(label: Text, service: Service): Instance raises Discovery.Error =
      if label.nil then abort(Discovery.Error(InvalidName(label)))
      new Instance(label, service)(named(label, label :: service.dnsName.labels))

    // The inverse of `dnsName`, as a PTR record's target arrives.
    def parse(name: Dns.Name): Optional[Instance] = name.labels match
      case Nil => Unset

      case label :: _ =>
        name.parent.let(Service.parse(_)).let: service => safely(Instance(label, service))

    // The next name to try after a conflict (RFC 6762 §9): `Foo` → `Foo (2)` → `Foo (3)`.
    private[syndesis] def next(label: Text): Text =
      val open = label.s.lastIndexOf(" (")

      val counted: Optional[Int] =
        if open < 0 || !label.ends(t")") then Unset
        else safely(label.s.substring(open + 2, label.s.length - 1).nn.tt.as[Int])

      val base: Text = if open < 0 then label else label.s.substring(0, open).nn.tt
      counted.let { count => t"$base (${(count + 1).show})" }.or(t"$label (2)")

    // The presentation form of RFC 6763 §4.3: the DNS name, with the label's dots escaped.
    given showable: Instance is Showable = _.dnsName.show

  case class Instance private (label: Text, service: Service)(val dnsName: Dns.Name):
    def renamed: Instance raises Discovery.Error = Instance(Instance.next(label), service)

  // ── TXT records ─────────────────────────────────────────────────────────────────────────────
  object Txt:
    val empty: Txt = new Txt(Nil)

    // Keys are printable ASCII without `=`, and each `key=value` string fits the 255-octet
    // character-string (RFC 6763 §6.4). Duplicates are kept: the first occurrence of a key is
    // the one that counts (§6.4), as receivers read them.
    def apply(entries: (Text, Optional[Text])*): Txt raises Discovery.Error =
      entries.each: (key, value) =>
        def printable(char: Char): Boolean = char >= 0x20 && char <= 0x7e && char != '='
        if key.nil || !key.chars.all(printable) then abort(Discovery.Error(InvalidTxt(key)))

        if string(key, value).in[Data](using codepages.utf8Codepage).length > 255
        then abort(Discovery.Error(InvalidTxt(key)))

      new Txt(List.from(entries))

    // From the strings of a received TXT record: `key=value`, or `key` alone for a key without
    // a value (`Unset`); an empty string, the placeholder of an empty record, is skipped.
    def parse(strings: List[Text]): Txt =
      val entries = strings.filter(!_.nil).map: string =>
        string.cut(t"=", 2) match
          case key :: value :: Nil => (key, value)
          case _                   => (string, Unset)

      new Txt(entries)

    private def string(key: Text, value: Optional[Text]): Text =
      value.let { value => t"$key=$value" }.or(key)

    private def quoted(string: Text): Text = t"\"$string\""
    given showable: Txt is Showable = txt => txt.strings.map(quoted).join(t" ")

  case class Txt private (entries: List[(Text, Optional[Text])]):
    // The value of a key, if present (or `Unset` for a key present without a value, which
    // `has` distinguishes); keys compare without regard to ASCII case (RFC 6763 §6.4).
    def apply(key: Text): Optional[Text] =
      entries.filter { case (key2, _) => Dns.Name.fold(key2) == Dns.Name.fold(key) }.prim.let(_(1))

    def has(key: Text): Boolean =
      entries.exists { case (key2, _) => Dns.Name.fold(key2) == Dns.Name.fold(key) }

    // The strings of the TXT record: one per entry, or a single empty string for none (RFC
    // 6763 §6.1).
    def strings: List[Text] = entries match
      case Nil => List(t"")
      case _   => entries.map(Txt.string)

  // ── Advertising and resolving ───────────────────────────────────────────────────────────────
  // What a service instance advertises: its name on the network, the port it serves on, its TXT
  // entries, and the host name its address records are published under (this host's, unless
  // given). `priority` and `weight` are the SRV record's, for a service with several instances.
  case class Description
    ( instance: Text,
      port:     Port,
      txt:      Txt                = Txt.empty,
      host:     Optional[Hostname] = Unset,
      priority: Int                = 0,
      weight:   Int                = 0 )

  // What resolving an instance yields: where it is and what it says about itself.
  case class Resolution
    ( instance:  Instance,
      host:      Dns.Name,
      port:      Port,
      txt:       Txt,
      addresses: List[Ipv4 | Ipv6] ):

    // The instance's addresses as endpoints, to connect to in turn.
    def endpoints: List[Endpoint[Port]] = addresses.map: address =>
      val remote = address.absolve match
        case ipv4: (Ipv4 @unchecked) => ipv4.show
        case ipv6: Ipv6              => ipv6.show

      Endpoint(remote, port)

  // What a browser sees: an instance appearing on the network, and one leaving it (or its
  // records expiring). A change to an instance's details shows up by resolving it again.
  object Event:
    given showable: Event is Showable =
      case Found(instance) => t"found ${instance.show}"
      case Lost(instance)  => t"lost ${instance.show}"

  enum Event:
    case Found(instance: Instance)
    case Lost(instance: Instance)

  // ── The seam ────────────────────────────────────────────────────────────────────────────────
  // Each operation is a loan: the advertisement or browser lives for the block, and ends with
  // it — or with the `Monitor` the backend runs under, whose cancellation unwinds the block.
  trait Backend:
    def advertise[result](service: Service, description: Description)
      ( block: Advertisement ?=> result )
    :   result raises Discovery.Error

    def browse[result](service: Service)(block: Browser ?=> result): result raises Discovery.Error

    def resolve[duration: Abstractable across Durations to Long]
      ( instance: Instance, timeout: duration )
    :   Resolution raises Discovery.Error

  // A running advertisement: `instance` is the name finally claimed, after any renaming a
  // conflict forced.
  class Advertisement private[syndesis] (val instance: Instance) extends caps.ExclusiveCapability

  // A running browse: its events, as they happen, and the instances it currently knows of.
  class Browser private[syndesis] (relay: Relay[Event], snapshot: () => Set[Instance])
  extends caps.ExclusiveCapability:
    def events: Chain[Event] = relay.chain
    def instances: Set[Instance] = snapshot()

  // ── Errors ──────────────────────────────────────────────────────────────────────────────────
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Conflict(name)       => m"the name $name could not be claimed after repeated conflicts"
        case Timeout(instance)    => m"the instance $instance did not resolve before the timeout"
        case InvalidName(text)    => m"the name $text is not a valid DNS-SD name"
        case InvalidTxt(key)      => m"the TXT entry $key is not valid"
        case InvalidService(name) => m"the service name $name is not valid"
        case Unavailable          => m"no service discovery backend could be started"

    enum Reason(val number: Int) extends Clarification:
      case Conflict(name: Text)       extends Reason(1)
      case Timeout(instance: Text)    extends Reason(2)
      case InvalidName(text: Text)    extends Reason(3)
      case InvalidTxt(key: Text)      extends Reason(4)
      case InvalidService(name: Text) extends Reason(5)
      case Unavailable                extends Reason(6)

  case class Error(reason: Discovery.Error.Reason)(using Diagnostics)
  extends fulminate.Error(54, reason.number)(m"service discovery failed because $reason")
