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

import anticipation.*
import coaxial.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
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

    private val second: Long = 1_000_000_000L

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

  // ── Events and errors ───────────────────────────────────────────────────────────────────────
  object Event:
    given communicable: Event is Communicable =
      case Joined(group)       => m"joined the multicast group $group"
      case Left(group)         => m"left the multicast group $group"
      case Probing(name)       => m"probing for the name $name"
      case Renamed(from, to)   => m"renamed $from to $to after a conflict"
      case Announced(name)     => m"announced $name"
      case Withdrawn(name)     => m"withdrew $name"
      case Queried(name)       => m"queried for $name"
      case Answered(name, n)   => m"answered a query for $name with $n records"

  enum Event:
    case Joined(group: Text)             extends Event, Log.Network
    case Left(group: Text)               extends Event, Log.Network
    case Probing(name: Text)             extends Event, Log.Network
    case Renamed(from: Text, to: Text)   extends Event, Log.Network
    case Announced(name: Text)           extends Event, Log.Network
    case Withdrawn(name: Text)           extends Event, Log.Network
    case Queried(name: Text)             extends Event, Log.Network
    case Answered(name: Text, count: Int) extends Event, Log.Network

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
