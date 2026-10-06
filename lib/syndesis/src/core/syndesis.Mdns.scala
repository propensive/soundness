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
    // Over the sockets: the IPv4 group, joined on every suitable interface. (The IPv6 group
    // awaits a receive that multiplexes two sockets.) This host's name is its first label under
    // `.local`, and its addresses are those of the joined interfaces.
    def sockets(backend: Socket.Backend, options: List[Socket.Option])
    :   Transport raises Mdns.Error =

      val interfaces = Multicast.interfaces(Unset)
      if interfaces == Nil then abort(Mdns.Error(NoInterface))

      val binding =
        try backend.joinMulticast(group4, interfaces, options)
        catch case error: java.io.IOException => abort(Mdns.Error(Join(group4.show)))

      val addresses2: List[Ipv4 | Ipv6] =
        List.from(interfaces.stdlib.flatMap(_.addresses.stdlib.map(_.address)))

      val label: Text =
        safely(jn.InetAddress.getLocalHost.nn.getHostName.nn.tt).or(t"soundness").cut(t".").prim
        . or(t"soundness")

      val host2: Dns.Name = safely(Dns.Name(label, t"local")).or(dns"soundness.local")

      new Transport:
        def host: Dns.Name = host2
        def addresses: List[Ipv4 | Ipv6] = addresses2
        def send(data: Data): Unit raises Socket.Error = backend.sendGroup(binding, data)

        def reply(to: Ipv4 | Ipv6, port: Udp.Port, data: Data): Unit raises Socket.Error =
          backend.sendTo(binding, to, port, data)

        def receive(): Packet raises Socket.Error = backend.receiveMulticast(binding)
        def close(): Unit = backend.leaveMulticast(binding)

    // An in-memory network: every transport joined to a bus receives what any of them sends,
    // itself included (as multicast loopback delivers), and a reply reaches the member whose
    // address it names.
    class Bus():
      private val members: juc.CopyOnWriteArrayList[Member] = juc.CopyOnWriteArrayList()

      def join(host: Dns.Name, addresses: List[Ipv4 | Ipv6]): Transport =
        val member = Member(host, addresses)
        members.add(member)
        member

      private object Termination

      private class Member(val host: Dns.Name, val addresses: List[Ipv4 | Ipv6])
      extends Transport:
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

        def close(): Unit =
          members.remove(this)
          queue.put(Termination)

  // What the responder needs of the network: this host's name and addresses (what it publishes
  // for itself), a send to the group, a unicast reply, a blocking receive, and closing, which
  // makes a pending receive fail.
  trait Transport:
    def host: Dns.Name
    def addresses: List[Ipv4 | Ipv6]
    def send(data: Data): Unit raises Socket.Error
    def reply(to: Ipv4 | Ipv6, port: Udp.Port, data: Data): Unit raises Socket.Error
    def receive(): Packet raises Socket.Error
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

  // ── The responder ───────────────────────────────────────────────────────────────────────────
  // What this responder has claimed on the network, with the records that say so.
  private case class Owned(instance: Discovery.Instance, records: List[Dns.Record])

  // A name being probed for (RFC 6762 §8.1), and whether another host has objected.
  private class Probing(val name: Dns.Name, val records: List[Dns.Record]):
    @caps.unsafe.untrackedCaptures @volatile var conflicted: Boolean = false

  // A running browse, with its re-query loop and task smuggled past tracking as `Handles` are.
  private case class Browse
    ( service:    Discovery.Service,
      browsing:   Discovery.Browsing,
      requerying: AnyRef,
      requery:    AnyRef ):

    def channel: Relay[Discovery.Event] = browsing.relay

  private case class Handles(receiving: AnyRef, sweeping: AnyRef, receiver: AnyRef, sweeper: AnyRef)
  private case class Pending(instance: Discovery.Instance, promise: Promise[Discovery.Resolution])

  // The mDNS responder behind `Discovery`: one per program, serving every advertisement, browse
  // and resolution through one transport, which it opens at the first loan and closes when the
  // last ends. Its receive loop and cache sweeper run as tasks under the `Monitor` it was given,
  // so cancelling that monitor ends them with everything else.
  //
  // Of RFC 6762 this first cut does: probing with renaming on conflict, announcing, answering
  // (with known-answer suppression, a random delay for shared records, and legacy unicast
  // replies), goodbyes, TTL-honouring browsing with exponential re-query, and resolving. It
  // does not yet break a tie between simultaneous probes (§8.2.1) or re-probe an established
  // name on a later conflict (§9); a message's own echo is recognised by its ID.
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
    @caps.unsafe.untrackedCaptures private var owned: List[Owned] = Nil
    @caps.unsafe.untrackedCaptures private var probing: List[Probing] = Nil
    @caps.unsafe.untrackedCaptures private var browses: List[Browse] = Nil
    @caps.unsafe.untrackedCaptures private var pending: List[Pending] = Nil

    // Our messages carry this ID (receivers ignore it, §18.1), which is how our own echoes are
    // told from another responder on this host.
    private val tag: Int = (jitter(0x10000L) | 1L).toInt

    private def now: Long = System.nanoTime

    // ── Lifecycle ─────────────────────────────────────────────────────────────────────────────
    private def acquire()(using Monitor^, Probate^, Tactic[Discovery.Error]): Transport = mutex:
      loans += 1

      live.or:
        val transport =
          recover:
            case error: Mdns.Error =>
              given diagnostics: Diagnostics = error.diagnostics
              abort(Discovery.Error(Discovery.Error.Reason.Unavailable))

          . protect(open())

        // A surprising message must not end the loop; what fails is that one dispatch.
        val receiving = loop:
          safely(transport.receive()).let: packet =>
            try dispatch(packet) catch case _: Exception => ()

        val sweeping = loop:
          safely(snooze(250L))
          sweep()

        // The loops are created and awaited under the same monitor; no aliased writer.
        val receiver = caps.unsafe.unsafeAssumeSeparate(async(receiving.run()))
        val sweeper = caps.unsafe.unsafeAssumeSeparate(async(sweeping.run()))
        live = transport

        handles =
          Handles
            ( receiving.asInstanceOf[AnyRef],
              sweeping.asInstanceOf[AnyRef],
              receiver.asInstanceOf[AnyRef],
              sweeper.asInstanceOf[AnyRef] )

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
        handles2.receiving.asInstanceOf[Loop].stop()
        handles2.sweeping.asInstanceOf[Loop].stop()
        transport.close()
        handles2.receiver.asInstanceOf[Task[Unit]].attend()
        handles2.sweeper.asInstanceOf[Task[Unit]].attend()

    // The transport, for a send within a loan, which is when one is always open.
    private def transport: Transport = mutex(live.or(unsafely(open())))

    private def send(message: Dns.Message): Unit = safely(transport.send(message.in[Data]))

    private def query(questions: List[Dns.Question], known: List[Dns.Record] = Nil): Unit =
      send(Dns.Message(tag, Dns.Flags(), questions, known))

    private def respond(records: List[Dns.Record]): Dns.Message =
      Dns.Message(tag, Dns.Flags(response = true, authoritative = true), Nil, records)

    // ── Receiving ─────────────────────────────────────────────────────────────────────────────
    private def dispatch(packet: Packet)(using Monitor^, Probate^): Unit =
      safely(packet.data.as[Dns.Message]).let: message =>
        if message.flags.response then absorb(message)
        else if message.id != tag then answer(message, packet)

    private def unique(record: Dns.Record): Boolean = record.rtype != Dns.Type.Ptr

    private def matches(question: Dns.Question, record: Dns.Record): Boolean =
      question.name == record.name &&
        (question.rtype == Dns.Type.Any || question.rtype == record.rtype)

    // A query from another responder: answer with the records we own that it asks for, minus
    // those it already knows with at least half their TTL left (§7.1). A probe (a query with
    // authority records) for a name we hold is answered at once, in defence (§8.2); other
    // answers wait 20–120 ms (§6), so responders on the link do not all speak together. A
    // legacy querier (not on port 5353) is answered by unicast, under its own ID (§6.7).
    private def answer(message: Dns.Message, packet: Packet)(using Monitor^, Probate^): Unit =
      val candidates = mutex(owned.flatMap(_.records))

      val answers = candidates.filter: record =>
        message.questions.exists(matches(_, record)) &&
          !message.answers.exists: known => known.key == record.key && known.ttl*2 >= record.ttl

      if answers != Nil then
        val legacy = packet.port != port
        val defence = message.authority != Nil

        def reply(): Unit =
          val response = if legacy then respond(answers).copy(id = message.id) else respond(answers)

          if legacy then safely(transport.reply(packet.sender, packet.port, response.in[Data]))
          else send(response)

        if defence || legacy || answers.all(unique) then reply() else
          val wait = 20 + jitter(100L)
          caps.unsafe.unsafeAssumeSeparate(async(safely(snooze(wait)).also(reply())))
          ()

    // A response from the link: into the cache, whose changes are the browsers' events and may
    // complete a resolution; and, for a name we are probing, a conflict if its records differ
    // from ours.
    private def absorb(message: Dns.Message): Unit =
      val records = List.concat(message.answers, message.additional)
      notify(cache.absorb(records, now))

      if message.id != tag then mutex(probing).each: probe =>
        records.each: record =>
          if record.name == probe.name && unique(record) &&
            !probe.records.exists(_.key == record.key)
          then probe.conflicted = true

      settle()

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
      ( using Monitor^, Probate^, Tactic[Discovery.Error] )
    :   Discovery.Instance =

      val transport = acquire()

      val claimed =
        try
          val txt =
            recover:
              case error: Dns.Error =>
                given diagnostics: Diagnostics = error.diagnostics
                abort(Discovery.Error(Discovery.Error.Reason.InvalidTxt(description.txt.show)))

            . protect(Dns.Rdata.Txt(description.txt.strings*))

          claim(Discovery.Instance(description.instance, service), description, txt, transport, 0)

        catch case error: Throwable =>
          release()
          throw error

      claimed.instance

    // A goodbye: every record with a TTL of zero (§10.1).
    def withdraw(instance: Discovery.Instance)(using Monitor^): Unit =
      val claimed = mutex(owned.filter(_.instance == instance))
      mutex { owned = owned.filter(_.instance != instance) }

      claimed.each: claimed2 => send(respond(claimed2.records.map(_.copy(ttl = 0, flush = false))))

      release()

    // Probes for the instance's name three times, 250 ms apart (§8.1), after a random wait of
    // up to 250 ms; an objection — ours, if we hold the name already, or another host's — renames
    // it and tries again, up to a limit; success is announced twice, a second apart (§8.3).
    private def claim
      ( instance: Discovery.Instance,
        description: Discovery.Description,
        txt: Dns.Rdata.Txt,
        transport: Transport,
        attempts: Int )
      ( using Monitor^, Probate^, Tactic[Discovery.Error] )
    :   Owned =

      if attempts >= 100 then abort(Discovery.Error(Discovery.Error.Reason.Conflict(instance.show)))
      val name = instance.dnsName
      val taken = mutex(owned.exists(_.instance.dnsName == name))

      if taken then claim(instance.renamed, description, txt, transport, attempts + 1) else
        val records2 = records(instance, description, txt, transport)
        val probe = Probing(name, records2.filter(unique))
        mutex { probing = probe :: probing }
        safely(snooze(jitter(250L)))

        def probes(remaining: Int): Boolean =
          if remaining == 0 then true else
            val questions = List(Dns.Question(name, Dns.Type.Any))
            send(Dns.Message(tag, Dns.Flags(), questions, Nil, probe.records))
            safely(snooze(250L))
            !probe.conflicted && probes(remaining - 1)

        val won = probes(3)
        mutex { probing = probing.filter(_ != probe) }

        if !won then
          claim(instance.renamed, description, txt, transport, attempts + 1)
        else
          val claimed = Owned(instance, records2)
          mutex { owned = claimed :: owned }
          send(respond(records2))
          // The second announcement, unless the advertisement was withdrawn in the meantime.
          caps.unsafe.unsafeAssumeSeparate:
            async:
              safely(snooze(1000L))
              if mutex(owned.exists(_ eq claimed)) then send(respond(records2))

          claimed

    // ── Browsing ──────────────────────────────────────────────────────────────────────────────
    // Instances already known are reported at once; then the service type is queried, and
    // queried again at intervals doubling from a second to an hour (§5.2), each time telling
    // responders what we already know.
    def browse(service: Discovery.Service)(using Monitor^, Probate^, Tactic[Discovery.Error])
    :   Discovery.Browsing =

      acquire()
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

      var interval: Long = 1000L

      val requerying = loop:
        ask()
        safely(snooze(interval))
        interval = (interval*2).min(3_600_000L)

      val requery = caps.unsafe.unsafeAssumeSeparate(async(requerying.run()))

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
      ( using Monitor^, Probate^, Tactic[Discovery.Error] )
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
