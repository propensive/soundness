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

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely

object Tests extends Suite(m"Syndesis tests"):
  def run(): Unit =
    val fury = Discovery.Service(t"fury", Tcp)

    suite(m"Service types"):
      test(m"A service type has its DNS-SD name"):
        fury.dnsName
      . assert(_ == dns"_fury._tcp.local")

      test(m"A service type shows as its registered form"):
        fury.show
      . assert(_ == t"_fury._tcp")

      test(m"A UDP service type uses the _udp label"):
        Discovery.Service(t"sip", Udp).dnsName
      . assert(_ == dns"_sip._udp.local")

      test(m"A subtype is browsed under _sub"):
        fury.subtypeName(t"printer")
      . assert(_ == dns"_printer._sub._fury._tcp.local")

      test(m"A service type parses from its DNS-SD name"):
        Discovery.Service.parse(dns"_fury._tcp.local")
      . assert(_ == fury)

      test(m"A name outside .local is not a service type"):
        Discovery.Service.parse(dns"_fury._tcp.example.com")
      . assert(_ == Unset)

      test(m"A service name over fifteen characters is rejected"):
        capture[Discovery.Error](Discovery.Service(t"sixteencharacter", Tcp)).reason
      . assert(_ == Discovery.Error.Reason.InvalidService(t"sixteencharacter"))

      test(m"A service name with its underscore is rejected"):
        capture[Discovery.Error](Discovery.Service(t"_fury", Tcp)).reason
      . assert(_ == Discovery.Error.Reason.InvalidService(t"_fury"))

    suite(m"Instances"):
      val gondor = Discovery.Instance(t"Gondor", fury)

      test(m"An instance's name prefixes its label as one label"):
        Discovery.Instance(t"Jon's Printer. Upstairs", fury).dnsName.labels
      . assert(_ == List(t"Jon's Printer. Upstairs", t"_fury", t"_tcp", t"local"))

      test(m"An instance shows with its label's dots escaped"):
        Discovery.Instance(t"Jon.Printer", fury).show
      . assert(_ == t"Jon\\.Printer._fury._tcp.local")

      test(m"An instance parses from a PTR target"):
        Discovery.Instance.parse(dns"Gondor._fury._tcp.local")
      . assert(_ == gondor)

      test(m"Parsed instances keep the label as written"):
        Discovery.Instance.parse(dns"Gondor._fury._tcp.local").let(_.label)
      . assert(_ == t"Gondor")

      test(m"A service type's own name is not an instance"):
        Discovery.Instance.parse(dns"_fury._tcp.local")
      . assert(_ == Unset)

      test(m"Renaming appends a counter"):
        gondor.renamed.label
      . assert(_ == t"Gondor (2)")

      test(m"Renaming increments an existing counter"):
        gondor.renamed.renamed.label
      . assert(_ == t"Gondor (3)")

      test(m"An empty label is rejected"):
        capture[Discovery.Error](Discovery.Instance(t"", fury)).reason
      . assert(_ == Discovery.Error.Reason.InvalidName(t""))

      test(m"A label over 63 bytes is rejected"):
        capture[Discovery.Error](Discovery.Instance(t"a"*64, fury)).reason
      . assert(_ == Discovery.Error.Reason.InvalidName(t"a"*64))

    suite(m"TXT records"):
      test(m"Entries render as key=value strings"):
        Discovery.Txt(t"fp" -> t"abc", t"flag" -> Unset).strings
      . assert(_ == List(t"fp=abc", t"flag"))

      test(m"An empty record is one empty string"):
        Discovery.Txt.empty.strings
      . assert(_ == List(t""))

      test(m"Strings parse back to entries"):
        Discovery.Txt.parse(List(t"fp=abc", t"flag", t"eq=a=b")).entries
      . assert(_ == List((t"fp", t"abc"), (t"flag", Unset), (t"eq", t"a=b")))

      test(m"The empty placeholder string parses to no entries"):
        Discovery.Txt.parse(List(t"")).entries
      . assert(_ == Nil)

      test(m"The first occurrence of a key wins"):
        Discovery.Txt(t"k" -> t"1", t"k" -> t"2")(t"k")
      . assert(_ == t"1")

      test(m"Keys compare without regard to case"):
        Discovery.Txt(t"Fp" -> t"abc")(t"fp")
      . assert(_ == t"abc")

      test(m"A key present without a value is present but Unset"):
        val txt = Discovery.Txt(t"flag" -> Unset)
        (txt.has(t"flag"), txt(t"flag"), txt.has(t"other"))
      . assert(_ == (true, Unset, false))

      test(m"A key containing = is rejected"):
        capture[Discovery.Error](Discovery.Txt(t"a=b" -> t"c")).reason
      . assert(_ == Discovery.Error.Reason.InvalidTxt(t"a=b"))

      test(m"An entry over 255 bytes is rejected"):
        capture[Discovery.Error](Discovery.Txt(t"k" -> t"v"*254)).reason
      . assert(_ == Discovery.Error.Reason.InvalidTxt(t"k"))

    suite(m"The mDNS cache"):
      val second = 1_000_000_000L
      val name = dns"gondor.local"
      def a(ttl: Int, flush: Boolean = false) = Dns.Record(name, ttl, Dns.Rdata.A(ip"10.0.0.1"), flush = flush)
      def other(ttl: Int, flush: Boolean = false) = Dns.Record(name, ttl, Dns.Rdata.A(ip"10.0.0.2"), flush = flush)

      test(m"A new record is added"):
        Mdns.Cache().absorb(List(a(120)), 0L)
      . assert(_ == List(Mdns.Cache.Transition.Added(a(120))))

      test(m"A record heard again is refreshed"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120)), 0L)
        cache.absorb(List(a(120)), second)
      . assert(_ == List(Mdns.Cache.Transition.Refreshed(a(120))))

      test(m"A record lapses when its TTL has elapsed"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120)), 0L)
        cache.sweep(121*second)
      . assert(_ == List(Mdns.Cache.Transition.Removed(a(120))))

      test(m"A record does not lapse before its TTL"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120)), 0L)
        cache.sweep(119*second)
      . assert(_ == Nil)

      test(m"A goodbye lapses the record a second later"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120)), 0L)
        cache.absorb(List(a(0)), 10*second)
        (cache.sweep(10*second + second/2), cache.sweep(12*second))
      . assert(_ == (Nil, List(Mdns.Cache.Transition.Removed(a(120)))))

      test(m"A cache-flush record retires older records of its name and type"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120)), 0L)
        cache.absorb(List(other(120, flush = true)), 10*second)
        cache.sweep(12*second).map(_.record.rdata)
      . assert(_ == List(Dns.Rdata.A(ip"10.0.0.1")))

      test(m"A cache-flush record spares records heard within the last second"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120)), 0L)
        cache.absorb(List(other(120, flush = true)), second/2)
        cache.sweep(12*second)
      . assert(_ == Nil)

      test(m"Known answers are those with over half their TTL left, with the TTL remaining"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120)), 0L)
        (cache.knownAnswers(name, Dns.Type.A, 50*second), cache.knownAnswers(name, Dns.Type.A, 70*second))
      . assert(_ == (List(a(70)), Nil))

      test(m"Lookup yields the unexpired records of a name and type"):
        val cache = Mdns.Cache()
        cache.absorb(List(a(120), other(10)), 0L)
        cache.lookup(name, Dns.Type.A, 20*second)
      . assert(_ == List(a(120)))

    suite(m"The in-memory bus"):
      test(m"Every member receives what one sends, the sender included"):
        val bus = Mdns.Transport.Bus()
        val first = bus.join(dns"first.local", List(ip"10.0.0.1"))
        val second = bus.join(dns"second.local", List(ip"10.0.0.2"))
        first.send(Data(1, 2, 3))
        (first.receive().sender, second.receive().sender)
      . assert(_ == (ip"10.0.0.1", ip"10.0.0.1"))

      test(m"A reply reaches the member with the address"):
        val bus = Mdns.Transport.Bus()
        val first = bus.join(dns"first.local", List(ip"10.0.0.1"))
        val second = bus.join(dns"second.local", List(ip"10.0.0.2"))
        first.reply(ip"10.0.0.2", udp"mdns", Data(9))
        second.receive().data.length
      . assert(_ == 1)

      test(m"A closed member's receive fails"):
        val bus = Mdns.Transport.Bus()
        val member = bus.join(dns"first.local", List(ip"10.0.0.1"))
        member.close()
        capture[Socket.Error](member.receive()).reason
      . assert(_ == Socket.Error.Reason.Accept)

    suite(m"Discovery over the bus"):
      import threading.platformThreading
      import probates.awaitProbate
      import abstractables.millisecondsAbstractable

      val gondor = Discovery.Instance(t"Gondor", fury)
      val description = Discovery.Description(t"Gondor", tcp"8443", Discovery.Txt(t"fp" -> t"abc"))

      test(m"An advertised instance is found by a browser"):
        supervise:
            val bus = Mdns.Transport.Bus()
            val a = Mdns.Responder(() => bus.join(dns"a.local", List(ip"10.0.0.1")))
            val b = Mdns.Responder(() => bus.join(dns"b.local", List(ip"10.0.0.2")))

            fury.advertise(description)(using a):
              fury.browse(using b):
                summon[Discovery.Browser].events.stdlib.head
      . assert(_ == Discovery.Event.Found(gondor))

      test(m"A found instance resolves to its port, TXT and addresses"):
        supervise:
            val bus = Mdns.Transport.Bus()
            val a = Mdns.Responder(() => bus.join(dns"a.local", List(ip"10.0.0.1")))
            val b = Mdns.Responder(() => bus.join(dns"b.local", List(ip"10.0.0.2")))

            fury.advertise(description)(using a):
              val resolution = gondor.resolve(5000L)(using b)
              (resolution.port.number, resolution.txt(t"fp"), resolution.addresses, resolution.host)
      . assert(_ == (8443, t"abc", List(ip"10.0.0.1"), dns"a.local"))

      test(m"A withdrawn instance is lost by a browser"):
        supervise:
            val bus = Mdns.Transport.Bus()
            val a = Mdns.Responder(() => bus.join(dns"a.local", List(ip"10.0.0.1")))
            val b = Mdns.Responder(() => bus.join(dns"b.local", List(ip"10.0.0.2")))

            fury.browse(using b):
              val events = summon[Discovery.Browser].events.stdlib
              fury.advertise(description)(using a)(events.head)
              events(1)
      . assert(_ == Discovery.Event.Lost(gondor))

      test(m"A name another host holds is renamed after probing"):
        supervise:
            val bus = Mdns.Transport.Bus()
            val a = Mdns.Responder(() => bus.join(dns"a.local", List(ip"10.0.0.1")))
            val b = Mdns.Responder(() => bus.join(dns"b.local", List(ip"10.0.0.2")))

            fury.advertise(description)(using a):
              fury.advertise(description.copy(port = tcp"8444"))(using b):
                summon[Discovery.Advertisement].instance.label
      . assert(_ == t"Gondor (2)")

      test(m"A name this responder holds is renamed without probing"):
        supervise:
            val bus = Mdns.Transport.Bus()
            val a = Mdns.Responder(() => bus.join(dns"a.local", List(ip"10.0.0.1")))

            fury.advertise(description)(using a):
              fury.advertise(description.copy(port = tcp"8444"))(using a):
                summon[Discovery.Advertisement].instance.label
      . assert(_ == t"Gondor (2)")

      test(m"An instance that never existed does not resolve"):
        supervise:
            val bus = Mdns.Transport.Bus()
            val b = Mdns.Responder(() => bus.join(dns"b.local", List(ip"10.0.0.2")))
            capture[Discovery.Error](Discovery.Instance(t"Nowhere", fury).resolve(300L)(using b)).reason
      . assert(_ == Discovery.Error.Reason.Timeout(t"Nowhere._fury._tcp.local"))

    suite(m"Resolutions"):
      test(m"A resolution's endpoints pair each address with the port"):
        val gondor = Discovery.Instance(t"Gondor", fury)
        val resolution =
          Discovery.Resolution(gondor, dns"gondor.local", tcp"8443", Discovery.Txt.empty,
              List(ip"192.168.1.2", ip"fe80::1"))

        resolution.endpoints.map(_.remote)
      . assert(_ == List(t"192.168.1.2", t"fe80::1"))
