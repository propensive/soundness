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

    suite(m"Resolutions"):
      test(m"A resolution's endpoints pair each address with the port"):
        val gondor = Discovery.Instance(t"Gondor", fury)
        val resolution =
          Discovery.Resolution(gondor, dns"gondor.local", tcp"8443", Discovery.Txt.empty,
              List(ip"192.168.1.2", ip"fe80::1"))

        resolution.endpoints.map(_.remote)
      . assert(_ == List(t"192.168.1.2", t"fe80::1"))
