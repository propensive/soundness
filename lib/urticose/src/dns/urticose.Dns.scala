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
package urticose

import java.net as jn

import scala.compiletime.asMatchable

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*, codepages.utf8Codepage
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import Dns.Error.Reason.*

// The DNS vocabulary of RFC 1035, typed over urticose's addresses and ports: names, the numbers
// (types, classes, opcodes, response codes), questions, resource records with their typed data,
// and the message that carries them. mDNS (RFC 6762) adds two bits to the wire format — a
// question's unicast-response request and a record's cache-flush flag — which surface as plain
// `Boolean` fields defaulting to `false`, so unicast use never sees them. The wire codec lives in
// `DnsWire`; this file is the pure vocabulary.
object Dns:
  private[urticose] def utf8(text: Text): Data = text.in[Data]

  // ── Names ───────────────────────────────────────────────────────────────────────────────────
  object Name:
    val Root: Name = Name(Nil)
    val local: Name = Name(List(t"local"))

    // Labels are taken verbatim: a DNS-SD instance label may contain dots, spaces and any UTF-8
    // (RFC 6763 §4.1.1), so only `parse` splits on dots. Length limits are enforced by `parse`
    // and by the encoder, not here, so a `Name` is total to construct.
    def apply(labels: Text*): Name = Name(List.from(labels))
    def apply(hostname: Hostname): Name = Name(List.from(hostname.dnsLabels.map(_.text)))

    // RFC 4343: only ASCII letters fold; every other character compares exactly.
    private[urticose] def fold(label: Text): Text =
      label.s.map { char => if 'A' <= char && char <= 'Z' then (char + 32).toChar else char }.tt

    // Character by character rather than by `sub`, whose regex-backed replacement would read
    // the backslashes it is meant to insert.
    private def escape(label: Text): Text =
      val builder: TextBuilder = TextBuilder()

      label.chars.each: char =>
        if char == '.' || char == '\\' then builder.append('\\')
        builder.append(char)

      builder()

    // The presentation form of RFC 1035 §5.1 and RFC 4343 §2.1: labels separated by dots, with
    // `\.` and `\\` escaping a literal dot or backslash, `\DDD` a decimal character code, and
    // an optional trailing dot; `.` (or nothing) is the root.
    def parse(text: Text): Name raises Dns.Error =
      if text == t"" || text == t"." then Root else
        val builder: TextBuilder = TextBuilder()

        def digit(index: Ordinal): Int = text(index) match
          case char: Char if char.isDigit => char - '0'
          case _                          => abort(Dns.Error(BadEscape(text)))

        def finish(labels: List[Text]): List[Text] =
          val label = builder()
          if label.nil then abort(Dns.Error(EmptyLabel(text)))
          if utf8(label).length > 63 then abort(Dns.Error(LongLabel(label)))
          builder.clear()
          label :: labels

        def complete(labels: List[Text]): Name =
          val name = Name(labels.reverse)
          if name.octets > 255 then abort(Dns.Error(LongName(name.show)))
          name

        // `open` is whether the builder holds a label still to be finished at the end: it does
        // not after a dot, so a trailing dot adds no empty label.
        def recur(index: Ordinal, labels: List[Text], open: Boolean): Name = text(index) match
          case '\\' => text(index + 1) match
            case char: Char if char.isDigit =>
              val value = digit(index + 1)*100 + digit(index + 2)*10 + digit(index + 3)
              if value > 255 then abort(Dns.Error(BadEscape(text)))
              builder.append(value.toChar)
              recur(index + 4, labels, true)

            case char: Char =>
              builder.append(char)
              recur(index + 2, labels, true)

            case _ =>
              abort(Dns.Error(BadEscape(text)))

          case '.' =>
            recur(index + 1, finish(labels), false)

          case char: Char =>
            builder.append(char)
            recur(index + 1, labels, true)

          case _ =>
            complete(if open then finish(labels) else labels)

        recur(Prim, Nil, false)

    given showable: Name is Showable = name =>
      if name.labels == Nil then t"." else name.labels.map(escape).join(t".")

    given inspectable: [name <: Name] => name is Inspectable = showable.text(_)
    given encodable: Name is Encodable in Text = showable.text(_)

    given decodable: (tactic: Tactic[Dns.Error]) => ((Name is Decodable in Text)^{tactic}) =
      parse(_)

  // Equality folds ASCII case (RFC 4343), as DNS compares names, while `labels` keeps the case
  // a name was written with, which mDNS asks to be preserved (RFC 6762 §16).
  case class Name(labels: List[Text]):
    private[urticose] lazy val folded: List[Text] = labels.map(Name.fold)

    def + (suffix: Name): Name = Name(List.concat(labels, suffix.labels))
    def prefix(label: Text): Name = Name(label :: labels)

    def parent: Optional[Name] = labels match
      case Nil       => Unset
      case _ :: tail => Name(tail)

    def endsWith(suffix: Name): Boolean =
      def recur(name: List[Text], suffix: List[Text]): Boolean = suffix match
        case Nil => true

        case head :: tail => name match
          case head2 :: tail2 => head == head2 && recur(tail2, tail)
          case Nil            => false

      recur(folded.reverse, suffix.folded.reverse)

    // The length on the wire, uncompressed: each label's octets plus its length byte, plus the
    // terminating zero.
    def octets: Int = labels.map(utf8(_).length + 1).total + 1

    override def equals(that: Any): Boolean = that.asMatchable match
      case that: Name => folded == that.folded
      case _          => false

    override def hashCode: Int = folded.hashCode

  // ── The numbers ─────────────────────────────────────────────────────────────────────────────
  object Type:
    val A: Type = Type(1)
    val Ns: Type = Type(2)
    val Cname: Type = Type(5)
    val Soa: Type = Type(6)
    val Ptr: Type = Type(12)
    val Mx: Type = Type(15)
    val Txt: Type = Type(16)
    val Aaaa: Type = Type(28)
    val Srv: Type = Type(33)
    val Opt: Type = Type(41)
    val Nsec: Type = Type(47)
    val Any: Type = Type(255)

    // The mnemonic, or the generic `TYPE1234` form of RFC 3597 §5 for an unknown number.
    given showable: Type is Showable = _.number match
      case 1   => t"A"
      case 2   => t"NS"
      case 5   => t"CNAME"
      case 6   => t"SOA"
      case 12  => t"PTR"
      case 15  => t"MX"
      case 16  => t"TXT"
      case 28  => t"AAAA"
      case 33  => t"SRV"
      case 41  => t"OPT"
      case 47  => t"NSEC"
      case 255 => t"ANY"
      case n   => t"TYPE${n.show}"

    given inspectable: [rtype <: Type] => rtype is Inspectable = showable.text(_)

  case class Type(number: Int)

  // A record's class; the top bit of the field on the wire is not part of it, but an mDNS flag
  // (except in an `OPT` record, where the field carries the UDP payload size, `Record#udpPayload`).
  object NetClass:
    val Internet: NetClass = NetClass(1)
    val Any: NetClass = NetClass(255)

    given showable: NetClass is Showable = _.number match
      case 1   => t"IN"
      case 255 => t"ANY"
      case n   => t"CLASS${n.show}"

    given inspectable: [netClass <: NetClass] => netClass is Inspectable = showable.text(_)

  case class NetClass(number: Int)

  object Opcode:
    val Query: Opcode = Opcode(0)
    val Notify: Opcode = Opcode(4)
    val Update: Opcode = Opcode(5)

  case class Opcode(number: Int)

  object Rcode:
    val NoError: Rcode = Rcode(0)
    val FormErr: Rcode = Rcode(1)
    val ServFail: Rcode = Rcode(2)
    val NxDomain: Rcode = Rcode(3)
    val NotImp: Rcode = Rcode(4)
    val Refused: Rcode = Rcode(5)

    given showable: Rcode is Showable = _.number match
      case 0 => t"NOERROR"
      case 1 => t"FORMERR"
      case 2 => t"SERVFAIL"
      case 3 => t"NXDOMAIN"
      case 4 => t"NOTIMP"
      case 5 => t"REFUSED"
      case n => t"RCODE${n.show}"

  case class Rcode(number: Int)

  // ── Questions and records ───────────────────────────────────────────────────────────────────
  // `unicast` is mDNS's "QU" bit (RFC 6762 §5.4): the querier accepts a unicast response.
  object Question:
    given showable: Question is Showable = question =>
      t"${question.name.show} ${question.netClass.show} ${question.rtype.show}"

  case class Question
    ( name: Name, rtype: Type, netClass: NetClass = NetClass.Internet, unicast: Boolean = false )

  // The typed data of a record. Octet-carrying cases (`Txt`, `Opt`, `Unknown`) compare their
  // bytes structurally, since `Data` itself compares by reference; the others are plain case
  // classes. `Srv`'s port is a bare number because the transport is a naming convention of the
  // owner name (`_tcp`/`_udp`), which the data does not see.
  object Rdata:
    private def octets(data: Data): scala.collection.immutable.ArraySeq[Byte] =
      scala.collection.immutable.ArraySeq.unsafeWrapArray(Array.unsafeJvm(data))

    case class A(address: Ipv4) extends Rdata
    case class Aaaa(address: Ipv6) extends Rdata
    case class Ptr(target: Name) extends Rdata
    case class Cname(target: Name) extends Rdata
    case class Ns(target: Name) extends Rdata
    case class Mx(preference: Int, exchange: Name) extends Rdata
    case class Srv(priority: Int, weight: Int, port: Int, target: Name) extends Rdata

    case class Soa
      ( mname:   Name,
        rname:   Name,
        serial:  Long,
        refresh: Int,
        retry:   Int,
        expire:  Int,
        minimum: Int )
    extends Rdata

    object Txt:
      def apply(strings: Text*): Txt = Txt(List.from(strings.map(utf8(_))))

    // The character-strings of RFC 1035 §3.3.14, each at most 255 octets; DNS-SD reads them as
    // `key=value` pairs (RFC 6763 §6). An empty list encodes as one empty string.
    case class Txt(strings: List[Data]) extends Rdata:
      def texts: List[Text] = strings.map(_.utf8)

      override def equals(that: Any): Boolean = that.asMatchable match
        case that: Txt => strings.map(octets) == that.strings.map(octets)
        case _         => false

      override def hashCode: Int = strings.map(octets).hashCode

    // The EDNS(0) options of RFC 6891 §6.1.2, as `(code, data)` pairs; the record's other
    // fields are reinterpreted, see `Record.opt`.
    case class Opt(options: List[(Int, Data)]) extends Rdata:
      override def equals(that: Any): Boolean = that.asMatchable match
        case that: Opt => comparable == that.comparable
        case _         => false

      override def hashCode: Int = comparable.hashCode
      private def comparable = options.map { case (code, data) => (code, octets(data)) }

    // A type this vocabulary does not interpret, carried as its wire octets (RFC 3597) so that
    // it round-trips.
    case class Unknown(override val rtype: Type, data: Data) extends Rdata:
      override def equals(that: Any): Boolean = that.asMatchable match
        case that: Unknown => rtype == that.rtype && octets(data) == octets(that.data)
        case _             => false

      override def hashCode: Int = (rtype, octets(data)).hashCode

    given showable: Rdata is Showable =
      case A(address)              => address.show
      case Aaaa(address)           => address.show
      case Ptr(target)             => target.show
      case Cname(target)           => target.show
      case Ns(target)              => target.show
      case Mx(pref, name)          => t"${pref.show} ${name.show}"
      case Srv(p, w, port, target) => t"${p.show} ${w.show} ${port.show} ${target.show}"
      case Txt(strings)            => strings.map { string => t"\"${string.utf8}\"" }.join(t" ")
      case Opt(options)            => options.map { option => option(0).show }.join(t"OPT ", t" ")
      case Unknown(_, data)        => t"\\# ${data.length.show}"

      case Soa(mname, rname, serial, refresh, retry, expire, minimum) =>
        val timings = t"${refresh.show} ${retry.show} ${expire.show} ${minimum.show}"
        t"${mname.show} ${rname.show} ${serial.show} $timings"

  sealed trait Rdata:
    def rtype: Type = this match
      case _: Rdata.A             => Type.A
      case _: Rdata.Aaaa          => Type.Aaaa
      case _: Rdata.Ptr           => Type.Ptr
      case _: Rdata.Cname         => Type.Cname
      case _: Rdata.Ns            => Type.Ns
      case _: Rdata.Mx            => Type.Mx
      case _: Rdata.Srv           => Type.Srv
      case _: Rdata.Soa           => Type.Soa
      case _: Rdata.Txt           => Type.Txt
      case _: Rdata.Opt           => Type.Opt
      case unknown: Rdata.Unknown => unknown.rtype

  // `flush` is mDNS's cache-flush bit (RFC 6762 §10.2): the sender's record set for this name
  // and type replaces whatever a receiver has cached.
  object Record:
    // An EDNS(0) `OPT` pseudo-record (RFC 6891 §6.1): the class field carries the sender's UDP
    // payload size and the TTL field the extended RCODE, version and the DO bit.
    def opt(udpPayload: Int = 1232, dnssecOk: Boolean = false, options: List[(Int, Data)] = Nil)
    :   Record =

      Record(Name.Root, if dnssecOk then 0x8000 else 0, Rdata.Opt(options), NetClass(udpPayload))

    given showable: Record is Showable = record =>
      val flush = if record.flush then t" (flush)" else t""
      val header = t"${record.name.show} ${record.ttl.show} ${record.netClass.show}"
      t"$header ${record.rtype.show} ${record.rdata.show}$flush"

  case class Record
    ( name:     Name,
      ttl:      Int,
      rdata:    Rdata,
      netClass: NetClass = NetClass.Internet,
      flush:    Boolean = false ):

    def rtype: Type = rdata.rtype

    // The identity of a record in a cache: everything but its TTL and flush bit.
    def key: (Name, Type, Rdata) = (name, rtype, rdata)

    def udpPayload: Optional[Int] = rdata match
      case _: Rdata.Opt => netClass.number
      case _            => Unset

    def extendedRcode: Optional[Int] = rdata match
      case _: Rdata.Opt => (ttl >>> 24) & 0xff
      case _            => Unset

    def dnssecOk: Optional[Boolean] = rdata match
      case _: Rdata.Opt => (ttl & 0x8000) != 0
      case _            => Unset

  // ── Messages ────────────────────────────────────────────────────────────────────────────────
  case class Flags
    ( response:           Boolean = false,
      opcode:             Opcode  = Opcode.Query,
      authoritative:      Boolean = false,
      truncated:          Boolean = false,
      recursionDesired:   Boolean = false,
      recursionAvailable: Boolean = false,
      authentic:          Boolean = false,
      checkingDisabled:   Boolean = false,
      rcode:              Rcode   = Rcode.NoError )

  object Message:
    def query(id: Int, questions: List[Question], recursionDesired: Boolean = true): Message =
      Message(id, Flags(recursionDesired = recursionDesired), questions)

    // A reply to `query`: the same ID and questions, echoing its recursion request.
    def response
      ( query:         Message,
        answers:       List[Record],
        authoritative: Boolean      = false,
        rcode:         Rcode        = Rcode.NoError,
        authority:     List[Record] = Nil,
        additional:    List[Record] = Nil )
    :   Message =

      val flags =
        Flags
          ( response         = true,
            opcode           = query.flags.opcode,
            authoritative    = authoritative,
            recursionDesired = query.flags.recursionDesired,
            rcode            = rcode )

      Message(query.id, flags, query.questions, answers, authority, additional)

    given showable: Message is Showable = message =>
      val kind = if message.flags.response then t"response" else t"query"
      val questions = message.questions.map(_.show).join(t"[", t", ", t"]")
      val answers = message.answers.map(_.show).join(t"[", t", ", t"]")
      t"$kind ${message.id.show} ${message.flags.rcode.show} $questions $answers"

  case class Message
    ( id:         Int,
      flags:      Flags,
      questions:  List[Question] = Nil,
      answers:    List[Record]   = Nil,
      authority:  List[Record]   = Nil,
      additional: List[Record]   = Nil )

  // ── Resolution through the platform's resolver ──────────────────────────────────────────────
  def resolve(name: Name): List[Ipv4 | Ipv6] raises Dns.Error =
    import scala.collection.immutable.ArraySeq

    val addresses: List[jn.InetAddress | Null] =
      try List.from(ArraySeq.unsafeWrapArray(jn.InetAddress.getAllByName(name.show.s).nn))
      catch case _: jn.UnknownHostException => abort(Dns.Error(Unresolved(name.show)))

    addresses.map: address =>
      // Every `InetAddress` is four or sixteen bytes, so one decoder accepts it.
      val data = Array.unsafeFrozen(address.nn.getAddress.nn)
      safely(data.as[Ipv4]).or(safely(data.as[Ipv6])).or(Ipv4.Localhost)

  def resolve(hostname: Hostname): List[Ipv4 | Ipv6] raises Dns.Error = resolve(Name(hostname))

  // ── Errors ──────────────────────────────────────────────────────────────────────────────────
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Truncated(offset)       => m"the message ends at offset $offset, inside a field"
        case BadPointer(offset)      => m"the name pointer at offset $offset points forward"
        case BadLabel(offset, kind)  => m"the label at offset $offset has unsupported type $kind"
        case LongLabel(label)        => m"the label $label is longer than 63 octets"
        case LongName(name)          => m"the name $name is longer than 255 octets"
        case EmptyLabel(text)        => m"the name $text contains an empty label"
        case BadEscape(text)         => m"the name $text contains a malformed escape sequence"
        case BadRdata(rtype, offset) => m"the $rtype record data at offset $offset is malformed"
        case LongString(length)      => m"a character-string of $length octets exceeds 255"
        case Trailing(offset)        => m"the message has trailing bytes from offset $offset"
        case Oversize(count)         => m"the count $count does not fit in a 16-bit field"
        case Unresolved(name)        => m"the name $name could not be resolved"
        case Timeout                 => m"no response arrived before the timeout"
        case Mismatch(id)            => m"the response with ID $id does not answer the query"

    enum Reason(val number: Int) extends Clarification:
      case Truncated(offset: Int)             extends Reason(1)
      case BadPointer(offset: Int)            extends Reason(2)
      case BadLabel(offset: Int, kind: Int)   extends Reason(3)
      case LongLabel(label: Text)             extends Reason(4)
      case LongName(name: Text)               extends Reason(5)
      case EmptyLabel(text: Text)             extends Reason(6)
      case BadEscape(text: Text)              extends Reason(7)
      case BadRdata(rtype: Type, offset: Int) extends Reason(8)
      case LongString(length: Int)            extends Reason(9)
      case Trailing(offset: Int)              extends Reason(10)
      case Oversize(count: Int)               extends Reason(11)
      case Unresolved(name: Text)             extends Reason(12)
      case Timeout                            extends Reason(13)
      case Mismatch(id: Int)                  extends Reason(14)

  case class Error(reason: Dns.Error.Reason)(using Diagnostics)
  extends fulminate.Error(53, reason.number)(m"the DNS message is not valid because $reason")
