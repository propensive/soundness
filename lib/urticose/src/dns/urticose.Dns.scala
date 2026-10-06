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

import scala.caps
import scala.collection.mutable as scm
import scala.compiletime.asMatchable
import scala.quoted.*

import anticipation.*
import contingency.*
import denominative.*, dysasymptotics.linearSize
import distillate.*
import fulminate.*
import gigantism.Lifts
import gossamer.*
import hieroglyph.*, codepages.utf8Codepage
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*
import zephyrine.*

import Dns.Error.Reason.*

// The DNS vocabulary of RFC 1035, typed over urticose's addresses and ports: names, the numbers
// (types, classes, opcodes, response codes), questions, resource records with their typed data,
// and the message that carries them. mDNS (RFC 6762) adds two bits to the wire format — a
// question's unicast-response request and a record's cache-flush flag — which surface as plain
// `Boolean` fields defaulting to `false`, so unicast use never sees them. The wire codec lives in
// `Wire`, at the end.
object Dns:
  private[urticose] def utf8(text: Text): Data = text.in[Data]

  // ── Names ───────────────────────────────────────────────────────────────────────────────────
  object Name:
    val Root: Name = new Name(Nil)
    val local: Name = new Name(List(t"local"))

    // Labels are taken verbatim: a DNS-SD instance label may contain dots, spaces and any UTF-8
    // (RFC 6763 §4.1.1), so only `parse` splits on dots. The limits — 63 octets to a label, 255
    // to the name — hold from construction, so that encoding a name is total.
    def apply(labels: Text*): Name raises Dns.Error = checked(List.from(labels))

    // A hostname's labels already satisfy the limits.
    def apply(hostname: Hostname): Name = new Name(List.from(hostname.dnsLabels.map(_.text)))

    private[urticose] def checked(labels: List[Text]): Name raises Dns.Error =
      def check(labels: List[Text]): Unit = labels match
        case Nil => ()

        case label :: tail =>
          if label.nil then abort(Dns.Error(EmptyLabel(labels.map(_.show).join(t"."))))
          if utf8(label).length > 63 then abort(Dns.Error(LongLabel(label)))
          check(tail)

      check(labels)
      val name = new Name(labels)
      if name.octets > 255 then abort(Dns.Error(LongName(name.show)))
      name

    given toExpr: ToExpr[Name]:
      def apply(name: Name)(using Quotes): Expr[Name] =
        // Hoisted from the `map` below: a quote (with its implicit `ToExpr` search) inside a
        // combinator lambda in a macro risks the `wildApprox` crash.
        def liftLabel(label: Text): Expr[Text] = '{${Expr(label.s)}.tt}

        val labels = Lifts.varargs(name.labels.map(liftLabel))
        '{Name.unchecked(List($labels*))}

    // For names whose labels are known to satisfy the limits: the wire parser's and the
    // interpolator's, which checked them already.
    private[urticose] def unchecked(labels: List[Text]): Name = new Name(labels)

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
          builder.clear()
          label :: labels

        def complete(labels: List[Text]): Name = checked(labels.reverse)

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
  // a name was written with, which mDNS asks to be preserved (RFC 6762 §16). Constructed only
  // through the companion, which holds the length limits.
  case class Name private[urticose] (labels: List[Text]):
    private[urticose] lazy val folded: List[Text] = labels.map(Name.fold)

    // Composition can breach the 255-octet limit, so it is checked like construction.
    def + (suffix: Name): Name raises Dns.Error = Name.checked(List.concat(labels, suffix.labels))
    def prefix(label: Text): Name raises Dns.Error = Name.checked(label :: labels)

    def parent: Optional[Name] = labels match
      case Nil       => Unset
      case _ :: tail => Name.unchecked(tail)

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
      def apply(strings: Text*): Txt raises Dns.Error = Txt(List.from(strings.map(utf8(_))))

      // The limit holds from construction, so that encoding is total.
      def apply(strings: List[Data]): Txt raises Dns.Error =
        strings.each: string =>
          if string.length > 255 then abort(Dns.Error(LongString(string.length)))

        new Txt(strings)

      private[urticose] def unchecked(strings: List[Data]): Txt = new Txt(strings)

    // The character-strings of RFC 1035 §3.3.14, each at most 255 octets; DNS-SD reads them as
    // `key=value` pairs (RFC 6763 §6). An empty list encodes as one empty string.
    final class Txt private (val strings: List[Data]) extends Rdata:
      def texts: List[Text] = strings.map(_.utf8)

      override def equals(that: Any): Boolean = that.asMatchable match
        case that: Txt => strings.map(octets) == that.strings.map(octets)
        case _         => false

      override def hashCode: Int = strings.map(octets).hashCode
      override def toString: String = s"Txt(${strings.map(_.utf8).join(t", ")})"

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
      case txt: Txt                => txt.strings.map { string => t"\"${string.utf8}\"" }.join(t" ")
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

    // The data in its uncompressed wire form, which RFC 6762 §8.2.1 compares lexicographically
    // to break a tie between two hosts probing the same name at once.
    def rdataBytes: Data = Wire.rdata(rdata)

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

    // Encoding is total: the limits a message could breach are held by `Name` and `Txt` from
    // construction. Decoding fails through a resolution-scoped tactic, as `Asn1`'s does.
    given encodable: Message is Encodable in Data = Wire.write(_)

    given decodable: (tactic: Tactic[Dns.Error]^)
    =>  ( (Message is Decodable in Data)^{tactic, caps.any} ) =
      Wire.parse(_)

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

  // ── The wire format ──────────────────────────────────────────────────────────────────────────
  // The RFC 1035 wire format: a strict reader and an append-only writer. The reader follows name
  // compression pointers only backwards, which is what the RFC allows and what proves that a walk
  // terminates; the writer compresses the owner names and the names inside the well-known types
  // of RFC 1035 §4.1.4, and never inside `SRV`, `TXT`, `OPT` or an unknown type (RFC 2782, RFC
  // 3597), though the reader accepts a compressed `SRV` target as mDNS senders emit (RFC 6762
  // §18.14). Writing is total: the limits are held by `Name` and `Rdata.Txt`.
  private[urticose] object Wire:
    def parse(data: Data): Message raises Error =
      val parser = new Parser(data)
      val message = parser.message()
      if parser.offset < data.length then abort(Error(Error.Reason.Trailing(parser.offset)))
      message

    def write(message: Message): Data =
      Producer.collect[Data](512): out => Writer(0, scm.HashMap(), true).message(message)(using out)

    // A record's data in its uncompressed form: the bytes RFC 6762 §8.2.1 compares to break a
    // tie between simultaneous probes.
    def rdata(rdata: Rdata): Data =
      Producer.collect[Data](64): out => Writer(0, scm.HashMap(), false).rdata(rdata)(using out)

    final class Parser private[urticose] (data: Data) extends caps.Mutable:
      // Exposed to the `parse` entry point only, so that it can detect trailing bytes.
      var offset: Int = 0

      private inline def need(count: Int)(using Tactic[Error]): Unit =
        if data.length - offset < count then abort(Error(Error.Reason.Truncated(offset)))

      // Proof: `need(1)` on the line above.
      private inline update def u8()(using Tactic[Error]): Int =
        need(1)
        (data.readUnchecked(offset) & 0xff).also(offset += 1)

      private update def u16()(using Tactic[Error]): Int =
        need(2)
        B16(data, offset).u16.int.also(offset += 2)

      private update def u32()(using Tactic[Error]): Long =
        val high = u16().toLong
        (high << 16) | u16().toLong

      private update def u64()(using Tactic[Error]): Long =
        val high = u32()
        (high << 32) | u32()

      private def copy(from: Int, count: Int): Data =
        val result = new scala.Array[Byte](count)
        System.arraycopy(Array.unsafeJvm(data), from, result, 0, count)
        Array.unsafeFrozen(result)

      private update def bytes(count: Int)(using Tactic[Error]): Data =
        need(count)
        copy(offset, count).also(offset += count)

      // Labels are read from `offset`. The first pointer met records where the name's caller
      // resumes, and reading continues at the pointer's target; each target must precede the
      // pointer that names it (RFC 1035 §4.1.4: a "prior occurrence"), so every hop moves
      // strictly backwards and the walk terminates. A state-terminated loop; every read is
      // bounds-checked on the line before it.
      update def name()(using Tactic[Error]): Name =
        var labels: List[Text] = Nil
        var position: Int = offset
        var resume: Int = -1
        var octets: Int = 1
        var done: Boolean = false

        while !done do
          if position >= data.length then abort(Error(Error.Reason.Truncated(position)))
          val head = data.readUnchecked(position) & 0xff

          if head == 0 then
            position += 1
            done = true
          else if (head & 0xc0) == 0xc0 then
            if position + 1 >= data.length then abort(Error(Error.Reason.Truncated(position)))
            val target = ((head & 0x3f) << 8) | (data.readUnchecked(position + 1) & 0xff)
            if target >= position then abort(Error(Error.Reason.BadPointer(position)))
            if resume < 0 then resume = position + 2
            position = target
          else if (head & 0xc0) != 0 then
            abort(Error(Error.Reason.BadLabel(position, head & 0xc0)))
          else
            octets += head + 1

            if octets > 255 then
              abort(Error(Error.Reason.LongName(Name.unchecked(labels.reverse).show)))

            if position + 1 + head > data.length then abort(Error(Error.Reason.Truncated(position)))
            labels = copy(position + 1, head).utf8 :: labels
            position += 1 + head

        offset = if resume < 0 then position else resume
        Name.unchecked(labels.reverse)

      update def question()(using Tactic[Error]): Question =
        val name = this.name()
        val rtype = Type(u16())
        val classField = u16()
        Question(name, rtype, NetClass(classField & 0x7fff), (classField & 0x8000) != 0)

      update def record()(using Tactic[Error]): Record =
        val name = this.name()
        val rtype = Type(u16())
        val classField = u16()
        val ttl = u32()
        val length = u16()
        need(length)
        val start = offset
        val end = offset + length
        val rdata = this.rdata(rtype, start, end)
        if offset != end then abort(Error(Error.Reason.BadRdata(rtype, start)))

        // An `OPT` record's class field is its UDP payload size, so its top bit is not a flag.
        val flush = rtype != Type.Opt && (classField & 0x8000) != 0
        val netClass = NetClass(if rtype == Type.Opt then classField else classField & 0x7fff)

        // A TTL with the top bit set is treated as zero (RFC 2181 §8).
        Record(name, if ttl > Int.MaxValue then 0 else ttl.toInt, rdata, netClass, flush)

      private update def rdata(rtype: Type, start: Int, end: Int)(using Tactic[Error])
      :   Rdata =

        val length = end - start

        def strings(acc: List[Data]): List[Data] =
          if offset >= end then acc.reverse else
            val count = u8()
            if offset + count > end then abort(Error(Error.Reason.BadRdata(rtype, start)))
            strings(bytes(count) :: acc)

        def options(acc: List[(Int, Data)]): List[(Int, Data)] =
          if offset >= end then acc.reverse else
            val code = u16()
            val count = u16()
            if offset + count > end then abort(Error(Error.Reason.BadRdata(rtype, start)))
            options((code, bytes(count)) :: acc)

        rtype.number match
          case 1 =>
            if length != 4 then abort(Error(Error.Reason.BadRdata(rtype, start)))
            Rdata.A(Ipv4(u8(), u8(), u8(), u8()))

          case 28 =>
            if length != 16 then abort(Error(Error.Reason.BadRdata(rtype, start)))
            val high = u64()
            Rdata.Aaaa(Ipv6(high, u64()))

          case 12 => Rdata.Ptr(name())
          case 5  => Rdata.Cname(name())
          case 2  => Rdata.Ns(name())

          case 15 =>
            val preference = u16()
            Rdata.Mx(preference, name())

          case 33 =>
            val priority = u16()
            val weight = u16()
            val port = u16()
            Rdata.Srv(priority, weight, port, name())

          case 6 =>
            val mname = name()
            val rname = name()
            val serial = u32()
            val refresh = u32().toInt
            val retry = u32().toInt
            val expire = u32().toInt
            Rdata.Soa(mname, rname, serial, refresh, retry, expire, u32().toInt)

          case 16 => Rdata.Txt.unchecked(strings(Nil))
          case 41 => Rdata.Opt(options(Nil))
          case _  => Rdata.Unknown(rtype, bytes(length))

      update def message()(using Tactic[Error]): Message =
        val id = u16()
        val bits = u16()
        val questionCount = u16()
        val answerCount = u16()
        val authorityCount = u16()
        val additionalCount = u16()

        val flags =
          Flags
            ( response           = (bits & 0x8000) != 0,
              opcode             = Opcode((bits >>> 11) & 0xf),
              authoritative      = (bits & 0x0400) != 0,
              truncated          = (bits & 0x0200) != 0,
              recursionDesired   = (bits & 0x0100) != 0,
              recursionAvailable = (bits & 0x0080) != 0,
              authentic          = (bits & 0x0020) != 0,
              checkingDisabled   = (bits & 0x0010) != 0,
              rcode              = Rcode(bits & 0xf) )

        def questions(count: Int): List[Question] =
          if count == 0 then Nil else
            val head = question()
            head :: questions(count - 1)

        def records(count: Int): List[Record] =
          if count == 0 then Nil else
            val head = record()
            head :: records(count - 1)

        val questions2 = questions(questionCount)
        val answers = records(answerCount)
        val authority = records(authorityCount)
        Message(id, flags, questions2, answers, authority, records(additionalCount))

    // Writes through the producer each method is given (a mutable class may not hold one),
    // tracking the position itself (the producer has no count) from `start`, so that a record's
    // data, written through a producer of its own, shares the message's compression `table` of
    // absolute offsets. `compressing` is off for the canonical uncompressed form.
    final class Writer private[urticose]
      ( start: Int, table: scm.HashMap[List[Text], Int], compressing: Boolean )
    extends caps.Mutable:

      private var written: Int = start

      private update def u8(value: Int)(using out: Producer.Bytes^): Unit =
        out.push((value & 0xff).toByte)
        written += 1

      private update def u16(value: Int)(using out: Producer.Bytes^): Unit =
        u8(value >>> 8)
        u8(value)

      private update def u32(value: Long)(using out: Producer.Bytes^): Unit =
        u8((value >>> 24).toInt)
        u8((value >>> 16).toInt)
        u8((value >>> 8).toInt)
        u8(value.toInt)

      private update def data(data: Data)(using out: Producer.Bytes^): Unit =
        out.put(data)
        written += data.length

      // Each suffix already written (and within pointer range) is replaced by a pointer to it;
      // every suffix written here is recorded for later names, pointer range permitting.
      private update def name(name: Name, compress: Boolean)(using out: Producer.Bytes^)
      :   Unit =

        def recur(labels: List[Text], folded: List[Text]): Unit = (labels, folded) match
          case (label :: tail, key :: keys) =>
            val known = if compressing && compress then table.getOrElse(folded, -1) else -1

            if known >= 0 then u16(0xc000 | known) else
              if written < 0x4000 && !table.contains(folded) then table(folded) = written
              val bytes = utf8(label)
              u8(bytes.length)
              data(bytes)
              recur(tail, keys)

          case _ => u8(0)

        recur(name.labels, name.folded)

      update def question(question: Question)(using out: Producer.Bytes^): Unit =

        name(question.name, true)
        u16(question.rtype.number)
        u16(question.netClass.number | (if question.unicast then 0x8000 else 0))

      update def record(record: Record)(using out: Producer.Bytes^): Unit =

        name(record.name, true)
        u16(record.rtype.number)

        val classField =
          if record.rtype == Type.Opt then record.netClass.number
          else record.netClass.number | (if record.flush then 0x8000 else 0)

        u16(classField)
        u32(record.ttl.toLong & 0xffffffffL)

        // The data goes through its own producer, continuing this one's offsets after the length
        // field it is then written behind, so the message's compression table stays absolute.
        val body = Producer.collect[Data](64): out2 =>
          Writer(written + 2, table, compressing).rdata(record.rdata)(using out2)

        u16(body.length)
        data(body)

      update def rdata(rdata: Rdata)(using out: Producer.Bytes^): Unit =

        def strings(list: List[Data]): Unit = list match
          case Nil => ()

          case string :: tail =>
            u8(string.length)
            data(string)
            strings(tail)

        def options(list: List[(Int, Data)]): Unit = list match
          case Nil => ()

          case (code, payload) :: tail =>
            u16(code)
            u16(payload.length)
            data(payload)
            options(tail)

        rdata match
          case Rdata.A(address)    => data(address.in[Data])
          case Rdata.Aaaa(address) => data(address.in[Data])
          case Rdata.Ptr(target)   => name(target, true)
          case Rdata.Cname(target) => name(target, true)
          case Rdata.Ns(target)    => name(target, true)
          case Rdata.Unknown(_, d) => data(d)
          case Rdata.Opt(list)     => options(list)
          case txt: Rdata.Txt      => if txt.strings == Nil then u8(0) else strings(txt.strings)

          case Rdata.Mx(preference, exchange) =>
            u16(preference)
            name(exchange, true)

          case Rdata.Srv(priority, weight, port, target) =>
            u16(priority)
            u16(weight)
            u16(port)
            name(target, false)

          case Rdata.Soa(mname, rname, serial, refresh, retry, expire, minimum) =>
            name(mname, true)
            name(rname, true)
            u32(serial)
            u32(refresh.toLong & 0xffffffffL)
            u32(retry.toLong & 0xffffffffL)
            u32(expire.toLong & 0xffffffffL)
            u32(minimum.toLong & 0xffffffffL)

      // A section's count is written modulo 2^16: a longer section is not representable, and a
      // message carrying one already exceeds what any DNS transport can carry.
      update def message(message: Message)(using out: Producer.Bytes^): Unit =

        val flags = message.flags

        val bits =
          (if flags.response then 0x8000 else 0) |
            ((flags.opcode.number & 0xf) << 11) |
            (if flags.authoritative then 0x0400 else 0) |
            (if flags.truncated then 0x0200 else 0) |
            (if flags.recursionDesired then 0x0100 else 0) |
            (if flags.recursionAvailable then 0x0080 else 0) |
            (if flags.authentic then 0x0020 else 0) |
            (if flags.checkingDisabled then 0x0010 else 0) |
            (flags.rcode.number & 0xf)

        def questions(list: List[Question]): Unit = list match
          case Nil          => ()
          case head :: tail => question(head).also(questions(tail))

        def records(list: List[Record]): Unit = list match
          case Nil          => ()
          case head :: tail => record(head).also(records(tail))

        u16(message.id)
        u16(bits)
        u16(message.questions.size & 0xffff)
        u16(message.answers.size & 0xffff)
        u16(message.authority.size & 0xffff)
        u16(message.additional.size & 0xffff)
        questions(message.questions)
        records(message.answers)
        records(message.authority)
        records(message.additional)

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
      case Unresolved(name: Text)             extends Reason(11)
      case Timeout                            extends Reason(12)
      case Mismatch(id: Int)                  extends Reason(13)

  case class Error(reason: Dns.Error.Reason)(using Diagnostics)
  extends fulminate.Error(53, reason.number)(m"the DNS message is not valid because $reason")
