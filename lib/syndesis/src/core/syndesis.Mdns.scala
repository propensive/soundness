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

import java.net as jn
import java.util.concurrent as juc

import scala.caps
import scala.collection.mutable as scm

import anticipation.*, abstractables.millisecondsAbstractable
import capricious.*
import coaxial.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import murmuration.deduplicate
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*

import Mdns.Error.Reason.*

// Multicast DNS (RFC 6762): the transport a responder speaks through, the cache of what it has
// heard, and the responder itself. The transport is a seam of its own, below `Discovery`'s:
// the protocol logic runs identically over the sockets and over an in-memory `Bus`, which is
// how it is tested without touching the host's network.
object Mdns:
  // The mDNS groups, and the one port both the source and destination of every message (RFC
  // 6762 §5.1).
  val group4: Multicast = Multicast(ip"224.0.0.251", udp"mdns")
  val group6: Multicast = Multicast(ip"ff02::fb", udp"mdns")
  val port: Udp.Port = udp"mdns"

  // The cache keeps time in `System.nanoTime` nanoseconds; the responder's waits are given to
  // `snooze` in milliseconds.
  private val second: Long = 1_000_000_000L

  // ── Transport ───────────────────────────────────────────────────────────────────────────────
  object Transport:
    // One source of packets, received one at a time by a blocking call: a joined socket. A
    // transport over both groups has two, and the responder reads each in a loop of its own.
    trait Inlet:
      def receive(): Packet raises Socket.Error

    // Over the sockets: each group joined on every suitable interface that has an address of
    // its family — the IPv6 group is left out on an IPv4-only host, and it is an error only if
    // neither group can be joined. This host's name is its first label under `.local`, and its
    // addresses are those of the joined interfaces.
    def sockets(backend: Socket.Backend, options: List[Socket.Option])
    :   Transport raises Mdns.Error =

      val interfaces = Multicast.interfaces(Unset)
      if interfaces == Nil then abort(Mdns.Error(NoInterface))

      def join(group: Multicast, suitable: Boolean): Optional[backend.MulticastSocket] =
        if !suitable then Unset else
          try backend.joinMulticast(group, interfaces, options)
          catch case error: java.io.IOException => Unset

      val binding4 = join(group4, interfaces.exists(_.ipv4 != Nil))
      val binding6 = join(group6, interfaces.exists(_.ipv6 != Nil))

      val bindings: List[backend.MulticastSocket] =
        List.concat(binding4.let(List(_)).or(Nil), binding6.let(List(_)).or(Nil))

      if bindings == Nil then abort(Mdns.Error(Join(group4.show)))

      val addresses2: List[Ipv4 | Ipv6] =
        List.from(interfaces.stdlib.flatMap(_.addresses.stdlib.map(_.address)))

      val label: Text =
        safely(jn.InetAddress.getLocalHost.nn.getHostName.nn.tt).or(t"soundness").cut(t".").prim
        . or(t"soundness")

      val host2: Dns.Name = safely(Dns.Name(label, t"local")).or(dns"soundness.local")

      new Transport:
        def host: Dns.Name = host2
        def addresses: List[Ipv4 | Ipv6] = addresses2

        def send(data: Data): Unit raises Socket.Error =
          bindings.each: binding => backend.sendGroup(binding, data)

        // A unicast reply leaves through the socket of the destination's family.
        def reply(to: Ipv4 | Ipv6, port: Udp.Port, data: Data): Unit raises Socket.Error =
          val binding = to.absolve match
            case _: (Ipv4 @unchecked) => binding4.or(binding6)
            case _: Ipv6              => binding6.or(binding4)

          binding.let: binding => backend.sendTo(binding, to, port, data)

        def inlets: List[Inlet] = bindings.map: binding =>
          new Inlet:
            def receive(): Packet raises Socket.Error = backend.receiveMulticast(binding)

        def close(): Unit = bindings.each: binding => backend.leaveMulticast(binding)

    // An in-memory network: every transport joined to a bus receives what any of them sends,
    // itself included (as multicast loopback delivers), and a reply reaches the member whose
    // address it names. A member is its own single inlet, which a test may receive from directly.
    class Bus():
      private val members: juc.CopyOnWriteArrayList[Member] = juc.CopyOnWriteArrayList()

      def join(host: Dns.Name, addresses: List[Ipv4 | Ipv6]): Transport & Inlet =
        val member = Member(host, addresses)
        members.add(member)
        member

      private object Termination

      private class Member(val host: Dns.Name, val addresses: List[Ipv4 | Ipv6])
      extends Transport, Inlet:
        private val queue: juc.LinkedBlockingQueue[Packet | Termination.type] =
          juc.LinkedBlockingQueue()

        private def source: Ipv4 | Ipv6 = addresses.prim.or(Ipv4.Localhost)

        def send(data: Data): Unit raises Socket.Error =
          val packet = Packet(data, source, port)
          members.forEach: member => member.nn.queue.put(packet)

        def reply(to: Ipv4 | Ipv6, port: Udp.Port, data: Data): Unit raises Socket.Error =
          val packet = Packet(data, source, Mdns.port)
          members.forEach: member => if member.nn.addresses.has(to) then member.nn.queue.put(packet)

        def receive(): Packet raises Socket.Error = queue.take().nn match
          case Termination    => abort(Socket.Error(Socket.Error.Reason.Accept))
          case packet: Packet => packet

        def inlets: List[Inlet] = List(this)

        def close(): Unit =
          members.remove(this)
          queue.put(Termination)

  // What the responder needs of the network: this host's name and addresses (what it publishes
  // for itself), a send to the group, a unicast reply, the inlets to receive from, and closing,
  // which makes a pending receive fail.
  trait Transport:
    def host: Dns.Name
    def addresses: List[Ipv4 | Ipv6]
    def send(data: Data): Unit raises Socket.Error
    def reply(to: Ipv4 | Ipv6, port: Udp.Port, data: Data): Unit raises Socket.Error
    def inlets: List[Transport.Inlet]
    def close(): Unit

  // ── Cache ───────────────────────────────────────────────────────────────────────────────────
  object Cache:
    // What a cache holds for one record: when it last arrived and when it lapses, in the
    // monotonic nanoseconds of `System.nanoTime`.
    case class Entry(record: Dns.Record, received: Long, expiry: Long)

    // What absorbing records or sweeping the cache changed, for the responder to turn into
    // browse events.
    enum Transition:
      case Added(record: Dns.Record)
      case Refreshed(record: Dns.Record)
      case Removed(record: Dns.Record)

      def record: Dns.Record

  // The records heard on the link, by identity (name, type and data), honouring their TTLs
  // (RFC 6762 §10): a goodbye (TTL 0) lapses a second later, and a record with the cache-flush
  // bit retires the other records of its name and type that are more than a second old.
  class Cache():
    import Cache.*, Cache.Transition.*

    private val mutex: Mutex = Mutex()
    private val entries: scm.HashMap[(Dns.Name, Dns.Type, Dns.Rdata), Entry] = scm.HashMap()

    def absorb(records: List[Dns.Record], now: Long): List[Transition] = mutex:
      var transitions: List[Transition] = Nil

      records.each: record =>
        val key = record.key

        if record.ttl == 0 then
          entries.get(key).foreach: entry => entries(key) = entry.copy(expiry = now + second)
        else
          if record.flush then
            entries.foreach: (key2, entry) =>
              val sameSet = key2 != key && key2(0) == key(0) && key2(1) == key(1)

              if sameSet && entry.received < now - second
              then entries(key2) = entry.copy(expiry = (now + second).min(entry.expiry))

          val known = entries.contains(key)
          entries(key) = Entry(record, now, now + record.ttl*second)
          transitions = (if known then Refreshed(record) else Added(record)) :: transitions

      transitions.reverse

    // Drops every lapsed record, reporting each.
    def sweep(now: Long): List[Transition] = mutex:
      val lapsed = List.from(entries.values.filter(_.expiry <= now).map(_.record))
      lapsed.each: record => entries.remove(record.key)
      lapsed.map(Removed(_))

    // The unexpired records of a name and type.
    def lookup(name: Dns.Name, rtype: Dns.Type, now: Long): List[Dns.Record] = mutex:
      List.from:
        entries.values.filter: entry =>
          entry.record.name == name && entry.record.rtype == rtype && entry.expiry > now

        . map(_.record)

    // The records worth telling a responder we already know (RFC 6762 §7.1): those with more
    // than half their TTL remaining, with the TTL that remains.
    def knownAnswers(name: Dns.Name, rtype: Dns.Type, now: Long): List[Dns.Record] = mutex:
      List.from:
        entries.values.filter: entry =>
          entry.record.name == name && entry.record.rtype == rtype &&
            (entry.expiry - now)*2 > (entry.expiry - entry.received)

        . map: entry => entry.record.copy(ttl = ((entry.expiry - now)/second).toInt)

    def earliestExpiry(name: Dns.Name, rtype: Dns.Type): Optional[Long] = mutex:
      val expiries = entries.values.filter: entry =>
        entry.record.name == name && entry.record.rtype == rtype

      if expiries.isEmpty then Unset else expiries.map(_.expiry).min

    def size: Int = mutex(entries.size)

  // ── Tiebreaking ─────────────────────────────────────────────────────────────────────────────
  // The order RFC 6762 §8.2.1 puts records in to break a tie between two hosts probing for one
  // name at once: class, then type, then the uncompressed rdata compared as unsigned bytes, a
  // prefix ranking below what extends it.
  object Tiebreak:
    def compare(left: Dns.Record, right: Dns.Record): Comparison =
      Comparison(left.netClass.number - right.netClass.number)
      . also(Comparison(left.rtype.number - right.rtype.number))
      . also(compareBytes(left.rdataBytes, right.rdataBytes, 0))

    private def compareBytes(left: Data, right: Data, index: Int): Comparison =
      if index == left.length || index == right.length then Comparison(left.length - right.length)
      else
        val difference = (left.readable(index) & 0xff) - (right.readable(index) & 0xff)
        if difference == 0 then compareBytes(left, right, index + 1) else Comparison(difference)

    // Each host's proposed records in order, compared pairwise; the first difference decides,
    // and a set that runs out first loses. `Same` is two hosts proposing identical records,
    // which is no conflict at all.
    def compare(ours: List[Dns.Record], theirs: List[Dns.Record]): Comparison =
      def compareSorted(ours: List[Dns.Record], theirs: List[Dns.Record]): Comparison =
        (ours, theirs) match
          case (Nil, Nil)                       => Comparison.Same
          case (Nil, _)                         => Comparison.Less
          case (_, Nil)                         => Comparison.More

          case (our :: ours2, their :: theirs2) =>
            compare(our, their).also(compareSorted(ours2, theirs2))

      compareSorted(sorted(ours), sorted(theirs))

    // The handful of records a probe proposes, in `compare` order; a small insertion sort
    // rather than `order`, which would need a sorting algorithm chosen in scope.
    private def sorted(records: List[Dns.Record]): List[Dns.Record] =
      def insert(record: Dns.Record, sorted: List[Dns.Record]): List[Dns.Record] = sorted match
        case head :: tail if compare(head, record).less => head :: insert(record, tail)
        case _                                          => record :: sorted

      records match
        case Nil          => Nil
        case head :: tail => insert(head, sorted(tail))

  // ── The responder ───────────────────────────────────────────────────────────────────────────
  // How probing for a name has gone so far: nothing heard against it; another host answered
  // for it (a conflict, §8.1), so it must be renamed; or another host probed for it at the
  // same time and won the tiebreak (§8.2.1), so we yield and probe for the same name again a
  // second later.
  private enum Verdict:
    case Clear, Conflict, Yield

  // A name being probed for (RFC 6762 §8.1), with the records proposed for it.
  private class Probing(val name: Dns.Name, val records: List[Dns.Record]):
    @caps.unsafe.untrackedCaptures @volatile var verdict: Verdict = Verdict.Clear

  // What this responder has claimed on the network: the instance (renamed if a conflict forces
  // it, before or after establishment), the records that say so, what they were built from (to
  // rebuild them under a new name), and whether its loan has ended, which stops a re-probe in
  // flight. It is the handle a loan holds, and reads its `instance` live.
  private final class Claim
    ( instance0:       Discovery.Instance,
      val description: Discovery.Description,
      val txt:         Dns.Rdata.Txt,
      records0:        List[Dns.Record] )
  extends Discovery.Advertising:

    @caps.unsafe.untrackedCaptures @volatile var instance: Discovery.Instance = instance0
    @caps.unsafe.untrackedCaptures @volatile var records: List[Dns.Record] = records0
    @caps.unsafe.untrackedCaptures @volatile var withdrawn: Boolean = false

  // A running browse, with its re-query loop and task smuggled past tracking as `Handles` are.
  private case class Browse
    ( service:    Discovery.Service,
      browsing:   Discovery.Browsing,
      requerying: AnyRef,
      requery:    AnyRef ):

    def channel: Relay[Discovery.Event] = browsing.relay

  // The live transport's loops and tasks: a receive loop and its task per inlet, and the cache
  // sweeper's.
  private case class Handles(receivers: List[(AnyRef, AnyRef)], sweeping: AnyRef, sweeper: AnyRef)

  private case class Pending(instance: Discovery.Instance, promise: Promise[Discovery.Resolution])

  // The mDNS responder behind `Discovery`: one per program, serving every advertisement, browse
  // and resolution through one transport, which it opens at the first loan and closes when the
  // last ends. Its receive loops and cache sweeper run as tasks under the `Monitor` it was
  // given, so cancelling that monitor ends them with everything else.
  //
  // Of RFC 6762 it does: probing with renaming on conflict and tiebreaking between simultaneous
  // probes (§8.2.1), announcing, defending an established name and re-probing it when a later
  // response conflicts with it (§9), answering (with known-answer suppression, a random delay
  // for shared records with answers falling due together aggregated into one message (§6.4),
  // and legacy unicast replies), goodbyes, TTL-honouring browsing with exponential re-query,
  // and resolving. A message's own echo is recognised by its ID.
  class Responder(open: () -> (Transport raises Mdns.Error)) extends Discovery.Backend:
    // A random count below `bound`, for the protocol's jitter.
    private def jitter(bound: Long): Long = (Random.global.long() & 0x7fffffffL) % bound

    private val mutex: Mutex = Mutex()
    private val cache: Cache = Cache()

    // Shared between the responder's tasks under `mutex`, as parasite's own state is: untracked,
    // since `Mutable`'s single-writer discipline is not the shape of a responder.
    @caps.unsafe.untrackedCaptures private var live: Optional[Transport] = Unset
    // The live transport's loops and tasks, smuggled past capture tracking as `AnyRef`s (as
    // scintillate's server does with its loops), to be stopped and awaited at release.
    @caps.unsafe.untrackedCaptures private var handles: Optional[Handles] = Unset
    @caps.unsafe.untrackedCaptures private var loans: Int = 0
    @caps.unsafe.untrackedCaptures private var owned: List[Claim] = Nil
    @caps.unsafe.untrackedCaptures private var probing: List[Probing] = Nil
    @caps.unsafe.untrackedCaptures private var browses: List[Browse] = Nil
    @caps.unsafe.untrackedCaptures private var pending: List[Pending] = Nil
    // Answers to shared records awaiting the one delayed response that carries them all.
    @caps.unsafe.untrackedCaptures private var deferred: List[Dns.Record] = Nil

    // Our messages carry this ID (receivers ignore it, §18.1), which is how our own echoes are
    // told from another responder on this host.
    private val tag: Int = (jitter(0x10000L) | 1L).toInt

    private def now: Long = System.nanoTime

    // ── Lifecycle ─────────────────────────────────────────────────────────────────────────────
    private def acquire()(using Monitor^, SharedProbate, Tactic[Discovery.Error]): Transport = mutex:
      loans += 1

      live.or:
        val transport =
          recover:
            case error: Mdns.Error =>
              given diagnostics: Diagnostics = error.diagnostics
              abort(Discovery.Error(Discovery.Error.Reason.Unavailable))

          . protect(open())

        // A receive loop per inlet. A surprising message must not end a loop; what fails is
        // that one dispatch. The loops are created and awaited under the same monitor (no
        // aliased writer), and handed on erased: a recursion rather than a `map`, whose
        // capture-polymorphic lambda cannot return a fresh loop.
        def start(inlets: List[Transport.Inlet], started: List[(AnyRef, AnyRef)])
        :   List[(AnyRef, AnyRef)] =

          inlets match
            case Nil => started

            case inlet :: rest =>
              val receiving = loop:
                safely(inlet.receive()).let: packet =>
                  try dispatch(packet) catch case _: Exception => ()

              val receiver = async(receiving.run())
              val handle = (receiving.asInstanceOf[AnyRef], receiver.asInstanceOf[AnyRef])
              start(rest, handle :: started)

        val receivers = start(transport.inlets, Nil)

        val sweeping = loop:
          safely(snooze(250L))
          sweep()

        val sweeper = async(sweeping.run())
        live = transport
        handles = Handles(receivers, sweeping.asInstanceOf[AnyRef], sweeper.asInstanceOf[AnyRef])
        transport

    // The last loan's release stops the tasks and waits for them — outside the mutex, which the
    // tasks themselves take.
    private def release()(using Monitor^): Unit =
      val ending: Optional[(Transport, Handles)] = mutex:
        loans -= 1

        if loans > 0 then Unset else
          val ending = live.let: transport => handles.let: handles2 => (transport, handles2)
          live = Unset
          handles = Unset
          ending

      ending.let: (transport, handles2) =>
        handles2.receivers.each: (receiving, _) => receiving.asInstanceOf[Loop].stop()
        handles2.sweeping.asInstanceOf[Loop].stop()
        transport.close()
        handles2.receivers.each: (_, receiver) => receiver.asInstanceOf[Task[Unit]].attend()
        handles2.sweeper.asInstanceOf[Task[Unit]].attend()

    // The transport, for a send within a loan, which is when one is always open.
    private def transport: Transport = mutex(live.or(unsafely(open())))

    private def send(message: Dns.Message): Unit = safely(transport.send(message.in[Data]))

    private def query(questions: List[Dns.Question], known: List[Dns.Record] = Nil): Unit =
      send(Dns.Message(tag, Dns.Flags(), questions, known))

    private def respond(records: List[Dns.Record]): Dns.Message =
      Dns.Message(tag, Dns.Flags(response = true, authoritative = true), Nil, records)

    // ── Receiving ─────────────────────────────────────────────────────────────────────────────
    private def dispatch(packet: Packet)(using Monitor^, SharedProbate): Unit =
      safely(packet.data.as[Dns.Message]).let: message =>
        if message.flags.response then absorb(message)
        else if message.id != tag then
          tiebreak(message)
          answer(message, packet)

    private def unique(record: Dns.Record): Boolean = record.rtype != Dns.Type.Ptr

    private def matches(question: Dns.Question, record: Dns.Record): Boolean =
      question.name == record.name &&
        (question.rtype == Dns.Type.Any || question.rtype == record.rtype)

    // A probe from another host (a query proposing records in its authority section) for a name
    // we are probing for too (§8.2.1): the host whose proposed records for that name compare
    // lower yields. If ours compare higher it is the other host's turn to yield, and nothing is
    // done here.
    private def tiebreak(message: Dns.Message): Unit =
      if message.authority != Nil then mutex(probing).each: probe =>
        val theirs = message.authority.filter(_.name == probe.name)
        val ours = probe.records.filter(_.name == probe.name)

        if theirs != Nil && probe.verdict == Verdict.Clear && Tiebreak.compare(ours, theirs).less
        then probe.verdict = Verdict.Yield

    // A query from another responder: answer with the records we own that it asks for, minus
    // those it already knows with at least half their TTL left (§7.1). A probe (a query with
    // authority records) for a name we hold is answered at once, in defence (§8.2); other
    // answers wait 20–120 ms (§6), so responders on the link do not all speak together. A
    // legacy querier (not on port 5353) is answered by unicast, under its own ID (§6.7).
    private def answer(message: Dns.Message, packet: Packet)(using Monitor^, SharedProbate): Unit =
      val candidates = mutex(owned.flatMap(_.records))

      val answers = candidates.filter: record =>
        message.questions.exists(matches(_, record)) &&
          !message.answers.exists: known => known.key == record.key && known.ttl*2 >= record.ttl

      if answers != Nil then
        val legacy = packet.port != port
        val defence = message.authority != Nil

        if legacy then
          val response = respond(answers).copy(id = message.id)
          safely(transport.reply(packet.sender, packet.port, response.in[Data]))
        else if defence || answers.all(unique) then
          send(respond(answers))
        else
          defer(answers)

    // Delayed answers falling due together go out as one message (§6.4): the first to be
    // deferred schedules the response, and those deferred before it is sent join it.
    private def defer(answers: List[Dns.Record])(using Monitor^, SharedProbate): Unit =
      val first = mutex:
        val first = deferred == Nil
        deferred = List.concat(deferred, answers)
        first

      if first then
        caps.unsafe.unsafeAssumeSeparate:
          async:
            safely(snooze(20 + jitter(100L)))

            val answers2 = mutex:
              val answers2 = deferred
              deferred = Nil
              answers2

            send(respond(answers2.deduplicate(_.key)))

        ()

    // A response from the link: into the cache, whose changes are the browsers' events and may
    // complete a resolution; for a name we are probing, a conflict if its records differ from
    // ours; and for a name we hold, a contest.
    private def absorb(message: Dns.Message)(using Monitor^, SharedProbate): Unit =
      val records = List.concat(message.answers, message.additional)
      notify(cache.absorb(records, now))

      if message.id != tag then
        mutex(probing).each: probe =>
          records.each: record =>
            if record.name == probe.name && unique(record) &&
              !probe.records.exists(_.key == record.key)
            then probe.verdict = Verdict.Conflict

        contest(records)

      settle()

    // Another host's record with the name and type of one of our established unique records,
    // but other data, is a conflict (§9): the claim stops being defended and is probed for
    // afresh, under a new name if the probe is answered. A goodbye (TTL 0) is a stale record
    // retiring, not a rival. No goodbye is sent for the name given up: the shared PTR pointing
    // to it is the rival's too, and its unique records were never ours to retire.
    private def contest(records: List[Dns.Record])(using Monitor^, SharedProbate): Unit =
      val contested = mutex:
        val found = owned.filter: claim =>
          records.exists: record =>
            def sameSet(ours: Dns.Record): Boolean =
              ours.name == record.name && ours.rtype == record.rtype

            record.ttl > 0 && unique(record) && claim.records.exists(sameSet) &&
              !claim.records.exists(_.key == record.key)

        owned = owned.filter: claim => !found.exists(_ eq claim)
        found

      contested.each: claim =>
        async(safely(reclaim(claim)))
        ()

    private def sweep(): Unit = notify(cache.sweep(now))

    // PTR records of a browsed service type appearing or lapsing are its instances found and
    // lost.
    private def notify(transitions: List[Cache.Transition]): Unit =
      val browses2 = mutex(browses)

      transitions.each: transition =>
        val record = transition.record

        record.rdata match
          case Dns.Rdata.Ptr(target) =>
            browses2.each: browse =>
              if browse.service.dnsName == record.name then
                Discovery.Instance.parse(target).let: instance =>
                  transition match
                    case Cache.Transition.Added(_) =>
                      browse.channel.put(Discovery.Event.Found(instance))

                    case Cache.Transition.Removed(_) =>
                      browse.channel.put(Discovery.Event.Lost(instance))

                    case _ => ()

          case _ => ()

    // Completes every pending resolution the cache can now answer.
    private def settle(): Unit =
      val pending2 = mutex(pending)

      pending2.each: pend =>
        resolved(pend.instance).let: resolution =>
          mutex { pending = pending.filter(_ != pend) }
          pend.promise.offer(resolution)

    // An instance's resolution from the cache alone, if its SRV, TXT and the target host's
    // address records are all present.
    private def resolved(instance: Discovery.Instance): Optional[Discovery.Resolution] =
      val time = now
      val name = instance.dnsName

      cache.lookup(name, Dns.Type.Srv, time).prim.let: srvRecord =>
        srvRecord.rdata.absolve match
          case Dns.Rdata.Srv(_, _, portNumber, target) =>
            cache.lookup(name, Dns.Type.Txt, time).prim.let: txtRecord =>
              val txt = txtRecord.rdata.absolve match
                case txt: Dns.Rdata.Txt => Discovery.Txt.parse(txt.texts)

              val ipv4 = cache.lookup(target, Dns.Type.A, time)
              val ipv6 = cache.lookup(target, Dns.Type.Aaaa, time)

              val addresses: List[Ipv4 | Ipv6] =
                List.concat(ipv4, ipv6)
                . map(_.rdata)
                . map:
                    case Dns.Rdata.A(address)    => address
                    case Dns.Rdata.Aaaa(address) => address
                    case _                       => Ipv4.Localhost

              if addresses == Nil then Unset else
                safely(instance.service.port(portNumber)).let: port =>
                  Discovery.Resolution(instance, target, port, txt, addresses)

    // ── Advertising ───────────────────────────────────────────────────────────────────────────
    // The records that advertise an instance (RFC 6763 §4–6): the PTR from its service type and
    // the enumeration meta-type, its SRV and TXT, and its host's addresses — the last three
    // unique to it, hence cache-flushing. PTRs live 75 minutes, the rest two minutes (§10).
    private def records
      ( instance: Discovery.Instance,
        description: Discovery.Description,
        txt: Dns.Rdata.Txt,
        transport: Transport )
      ( using Tactic[Discovery.Error] )
    :   List[Dns.Record] =

      val service = instance.service
      val name = instance.dnsName

      val host: Dns.Name =
        description.host.let: hostname =>
          val labels = List.concat(Dns.Name(hostname).labels, Discovery.local.labels)
          Discovery.named(hostname.show, labels)

        . or(transport.host)

      val addresses = transport.addresses.map: address =>
        val rdata = address.absolve match
          case ipv4: (Ipv4 @unchecked) => Dns.Rdata.A(ipv4)
          case ipv6: Ipv6              => Dns.Rdata.Aaaa(ipv6)

        Dns.Record(host, 120, rdata, flush = true)

      val srv =
        Dns.Rdata.Srv(description.priority, description.weight, description.port.number, host)

      val subtypes = service.subtypes.map: subtype =>
        Dns.Record(service.subtypeName(subtype), 4500, Dns.Rdata.Ptr(name))

      List.concat
        ( List
            ( Dns.Record(Discovery.Service.enumeration, 4500, Dns.Rdata.Ptr(service.dnsName)),
              Dns.Record(service.dnsName, 4500, Dns.Rdata.Ptr(name)),
              Dns.Record(name, 120, srv, flush = true),
              Dns.Record(name, 4500, txt, flush = true) ),
          List.concat(subtypes, addresses) )

    def advertise(service: Discovery.Service, description: Discovery.Description)
      ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
    :   Discovery.Advertising =

      val transport = acquire()

      try
        val txt =
          recover:
            case error: Dns.Error =>
              given diagnostics: Diagnostics = error.diagnostics
              abort(Discovery.Error(Discovery.Error.Reason.InvalidTxt(description.txt.show)))

          . protect(Dns.Rdata.Txt(description.txt.strings*))

        val instance = Discovery.Instance(description.instance, service)
        val records2 = records(instance, description, txt, transport)
        val claim = Claim(instance, description, txt, records2)
        establish(claim, transport, 0)
        claim

      catch case error: Throwable =>
        release()
        throw error

    // A goodbye: every record with a TTL of zero (§10.1). A claim mid-way through re-probing
    // has nothing established to say goodbye to, and its re-probe stops at the flag.
    def withdraw(advertising: Discovery.Advertising)
      ( using Monitor^, (Discovery.Activity is Loggable)^ )
    :   Unit =

      val claimed = mutex:
        val found = owned.filter(_ eq advertising)
        owned = owned.filter(!_.eq(advertising))
        found

      advertising match
        case claim: Claim => claim.withdrawn = true
        case _            => ()

      claimed.each: claim =>
        send(respond(claim.records.map(_.copy(ttl = 0, flush = false))))
        Log.info(Discovery.Activity.Withdrawn(claim.instance))

      release()

    // Probes for the claim's name three times, 250 ms apart (§8.1), after a random wait of up
    // to 250 ms. An objection — ours, if we hold the name already, or another host's answer —
    // renames it and tries again, up to a limit; losing a tiebreak against a simultaneous probe
    // (§8.2.1) waits a second and probes for the same name again; success is announced twice,
    // a second apart (§8.3).
    private def establish(claim: Claim, transport: Transport, attempts: Int)
      ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
    :   Unit =

      if attempts >= 100
      then abort(Discovery.Error(Discovery.Error.Reason.Conflict(claim.instance.show)))

      val name = claim.instance.dnsName

      val taken = mutex:
        owned.exists: claim2 => !(claim2 eq claim) && claim2.instance.dnsName == name

      if taken then rename(claim, transport, attempts) else
        val probe = Probing(name, claim.records.filter(unique))
        mutex { probing = probe :: probing }
        Log.fine(Discovery.Activity.Probing(name))
        safely(snooze(jitter(250L)))

        def probes(remaining: Int): Verdict =
          if remaining == 0 then probe.verdict else
            val questions = List(Dns.Question(name, Dns.Type.Any))
            send(Dns.Message(tag, Dns.Flags(), questions, Nil, probe.records))
            safely(snooze(250L))
            if probe.verdict == Verdict.Clear then probes(remaining - 1) else probe.verdict

        val verdict = probes(3)
        mutex { probing = probing.filter(_ != probe) }

        verdict match
          case Verdict.Conflict =>
            Log.info(Discovery.Activity.Conflicted(name))
            rename(claim, transport, attempts)

          case Verdict.Yield =>
            Log.info(Discovery.Activity.Yielded(name))
            safely(snooze(1000L))
            establish(claim, transport, attempts + 1)

          case Verdict.Clear =>
            if !claim.withdrawn then
              mutex { owned = claim :: owned }
              send(respond(claim.records))
              Log.info(Discovery.Activity.Claimed(claim.instance))

              // The second announcement, unless the advertisement was withdrawn or contested
              // in the meantime.
              caps.unsafe.unsafeAssumeSeparate:
                async:
                  safely(snooze(1000L))
                  if mutex(owned.exists(_ eq claim)) then send(respond(claim.records))

              ()

    // The next name (RFC 6762 §9: `Gondor` → `Gondor (2)`), with its records rebuilt.
    private def rename(claim: Claim, transport: Transport, attempts: Int)
      ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
    :   Unit =

      claim.instance = claim.instance.renamed
      claim.records = records(claim.instance, claim.description, claim.txt, transport)
      establish(claim, transport, attempts + 1)

    // After a conflict with an established claim: back through probing, unless the loan ended.
    // This runs in the background, where no loan's `Loggable` is at hand.
    private def reclaim(claim: Claim)
      ( using monitor: Monitor^, probate: SharedProbate, tactic: Tactic[Discovery.Error] )
    :   Unit =

      if !claim.withdrawn
      then establish(claim, transport, 0)(using monitor, probate, tactic, Discovery.Activity.silent)

    // ── Browsing ──────────────────────────────────────────────────────────────────────────────
    // Instances already known are reported at once; then the service type is queried, and
    // queried again at intervals doubling from a second to an hour (§5.2), each time telling
    // responders what we already know.
    def browse(service: Discovery.Service)
      ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
    :   Discovery.Browsing =

      acquire()
      Log.info(Discovery.Activity.Browsing(service))
      val relay: Relay[Discovery.Event] = Relay()
      val name = service.dnsName

      def instances(): Set[Discovery.Instance] =
        Set.from:
          cache.lookup(name, Dns.Type.Ptr, now).stdlib.flatMap: record =>
            record.rdata.absolve match
              case Dns.Rdata.Ptr(target) =>
                Discovery.Instance.parse(target).let(List(_)).or(Nil).stdlib

              case _ => scala.collection.immutable.Nil

      val browsing = Discovery.Browsing(relay, () => instances())

      def ask(): Unit =
        query(List(Dns.Question(name, Dns.Type.Ptr)), cache.knownAnswers(name, Dns.Type.Ptr, now))

      val interval: Atomic.Long = Atomic(1000L)

      val requerying = loop:
        ask()
        safely(snooze(interval()))
        interval() = (interval()*2).min(3_600_000L)

      val requery = async(requerying.run())

      val browse =
        Browse(service, browsing, requerying.asInstanceOf[AnyRef], requery.asInstanceOf[AnyRef])

      mutex { browses = browse :: browses }
      instances().each: instance => relay.put(Discovery.Event.Found(instance))
      browsing

    def dismiss(browsing: Discovery.Browsing)(using Monitor^): Unit =
      val found = mutex(browses.filter(_.browsing eq browsing))
      mutex { browses = browses.filter(!_.browsing.eq(browsing)) }

      found.each: browse =>
        browse.requerying.asInstanceOf[Loop].stop()
        browse.channel.stop()
        browse.requery.asInstanceOf[Task[Unit]].cancel()

      release()

    // ── Resolving ─────────────────────────────────────────────────────────────────────────────
    def resolve[duration: Abstractable across Durations to Long]
      ( instance: Discovery.Instance, timeout: duration )
      ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
    :   Discovery.Resolution =

      acquire()

      try resolved(instance).or:
        val promise: Promise[Discovery.Resolution] = Promise()
        val pend = Pending(instance, promise)
        mutex { pending = pend :: pending }
        val name = instance.dnsName

        try
          query(List(Dns.Question(name, Dns.Type.Srv), Dns.Question(name, Dns.Type.Txt)))

          // The target's addresses may not have come with the SRV record.
          cache.lookup(name, Dns.Type.Srv, now).prim.let: record =>
            record.rdata.absolve match
              case Dns.Rdata.Srv(_, _, _, target) =>
                query(List(Dns.Question(target, Dns.Type.A), Dns.Question(target, Dns.Type.Aaaa)))

          recover:
            case error: Async.Error =>
              given diagnostics: Diagnostics = error.diagnostics
              abort(Discovery.Error(Discovery.Error.Reason.Timeout(instance.show)))

          . protect(promise.await(timeout))

        finally mutex { pending = pending.filter(_ != pend) }

      . also(Log.info(Discovery.Activity.Resolved(instance)))

      finally release()

  // ── Errors ───────────────────────────────────────────────────────────────────────
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Join(group)  => m"the multicast group $group could not be joined"
        case NoInterface  => m"no interface is up and capable of multicast"

    enum Reason(val number: Int) extends Clarification:
      case Join(group: Text) extends Reason(1)
      case NoInterface       extends Reason(2)

  case class Error(reason: Mdns.Error.Reason)(using Diagnostics)
  extends fulminate.Error(55, reason.number)(m"the mDNS responder could not start because $reason")
