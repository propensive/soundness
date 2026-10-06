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

import soundness.*
import fulminate.errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import urticose.teletypeables.urlTeletype
import denominative.dysasymptotics.linearSize
import alphabets.hexUpperCase

object Tests extends Suite(m"Urticose tests"):
  given palette: UrlPalette = new Palette:
    type Form = Srgb
    def background: Color in Srgb = WebColors.Black
    def foreground: Color in Srgb = WebColors.White
    def link: Color in Srgb       = WebColors.DeepSkyBlue

  def run(): Unit =
    suite(m"URL styling tests"):
      // `escapade` renders any `Showable` type unstyled if no `Teletypeable` is found, so a
      // lost `urlTeletype` given would degrade silently rather than failing to compile.
      test(m"A URL renders with styling, not via the plain Showable fallback"):
        val url = url"https://example.com/path"
        val styled = e"$url".render(termcapDefinitions.xterm256Termcap)
        styled == e"${url.show}".render(termcapDefinitions.xterm256Termcap)
      . assert(_ == false)

      test(m"A styled URL still contains the URL's own text"):
        val url = url"https://example.com/path"
        e"$url".plain
      . assert(_ == t"https://example.com/path")

    suite(m"Internet tests"):
      def remoteCall()(using Internet): Unit = ()

      test(m"Check remote call is callable with `Internet`"):
        internet(true):
          remoteCall()
      . assert()

      // TODO: fix
      // test(m"Check remote call is not callable without `Internet`"):
      //   val result = demilitarize:
      //     remoteCall()
      //   .map(_.id)
      //   println(result)
      //   result
      // .assert(_ == List(CompileError.Id.MissingImplicitArgument))


    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings. A failure names the ones which
    // fell back.
    suite(m"Native-rendering coverage"):
      test(m"urticose's network types all inspect natively"):
        Inspectable.fallbacks
         ( ip"192.168.0.1".inspect,
           ip"2001:db8::1".inspect,
           ip"255.123.143.0".subnet(12).inspect,
           MacAddress(1, 2, 3, 4, 5, 6).inspect,
           tcp"smtp".inspect,
           t"www.example.com".as[Hostname].inspect,
           t"simple@example.com".as[EmailAddress].inspect,
           url"https://example.com/path".inspect,
           url"https://example.com/path".scheme.inspect,
           t"user@example.com:8080".as[Authority].inspect,
           Endpoint(t"example.com", 8080).inspect )
      . assert(_ == Nil)

      test(m"An authority inspects as its URL form, introduced by `//`"):
        t"user@example.com:8080".as[Authority].inspect
      . assert(_ == t"//user@example.com:8080")

      test(m"An endpoint inspects both its remote and its port"):
        Endpoint(t"example.com", 8080).inspect
      . assert(_ == t"""Endpoint(t"example.com":8080)""")

    suite(m"IPv4 tests"):
      test(m"Parse in IPv4 address"):
        t"1.2.3.4".as[Ipv4]
      . assert(_ == Ipv4(1, 2, 3, 4))

      test(m"Show an Ipv4 address"):
        Ipv4(127, 244, 197, 0).show
      . assert(_ == t"127.244.197.0")

      test(m"Show a zero Ipv4 address"):
        Ipv4(0, 0, 0, 0).show
      . assert(_ == t"0.0.0.0")

      test(m"Show a 'maximum' Ipv4 address"):
        Ipv4(255, 255, 255, 255).show
      . assert(_ == t"255.255.255.255")

      test(m"Get an IP address as an integer"):
        Ipv4(192, 168, 0, 1).int
      . assert(_ == bin"11000000 10101000 00000000 00000001")

      test(m"Inspect an Ipv4 address"):
        Ipv4(127, 244, 197, 0).inspect
      . assert(_ == t"127.244.197.0")

    suite(m"IPv6 tests"):
      test(m"Parse an IPv6 address"):
        t"2001:db8:0000:1:1:1:1:1".as[Ipv6]
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))

      test(m"Render an IPv6 address"):
        t"2001:db8:0000:1:1:1:1:1".as[Ipv6].show
      . assert(_ == t"2001:db8:0:1:1:1:1:1")

      test(m"Inspect an IPv6 address"):
        t"2001:db8:0000:1:1:1:1:1".as[Ipv6].inspect
      . assert(_ == t"2001:db8:0:1:1:1:1:1")

      test(m"Parse zero IPv6 address"):
        t"::".as[Ipv6]
      . assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 0))

      test(m"Parse zero-leading IPv6 address"):
        t"::2".as[Ipv6]
      . assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 2))

      test(m"Parse zeroes-trailing IPv6 address"):
        t"8::".as[Ipv6]
      . assert(_ == Ipv6(8, 0, 0, 0, 0, 0, 0, 0))

      test(m"Show zero IPv6 address"):
        Ipv6(0, 0, 0, 0, 0, 0, 0, 0).show
      . assert(_ == t"::")

      test(m"Show zero-leading IPv6 address"):
        Ipv6(0, 0, 0, 0, 0, 0, 0, 1).show
      . assert(_ == t"::1")

      test(m"Show zeroes-trailing IPv6 address"):
        Ipv6(8, 0, 0, 0, 0, 0, 0, 0).show
      . assert(_ == t"8::")

      test(m"Parse IPv4 address at compiletime"):
        ip"122.0.0.1"
      . assert(_ == Ipv4(122, 0, 0, 1))

      test(m"Parse an IPv6 address at compiletime"):
        ip"2001:db8::1:1:1:1"
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0, 0x1, 0x1, 0x1, 0x1))

      test(m"Create and show a subnet"):
        (ip"255.123.143.0".subnet(12)).show
      . assert(_ == t"255.112.0.0/12")

      test(m"Parse an IPv6 containing capital letters"):
        t"2001:DB8::1:1:1:1:1".as[Ipv6]
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))

      test(m"Invalid IP address is compile error"):
        demilitarize(ip"192.168.0.0.0.1").map(_.message)
      . assert(_ == List(t"[↯SN-077.3] the IP address is not valid because the address contains 6 period-separated groups instead of 4"))

      test(m"IP address byte out of range"):
        capture(t"100.300.200.0".as[Ipv4])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv4ByteOutOfRange(300)))

      test(m"IPv4 address wrong number of bytes"):
        capture(t"10.3.20.0.8".as[Ipv4])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv4WrongNumberOfGroups(5)))

      test(m"IPv6 address non-hex value"):
        capture(t"::8:abcg:abc:1234".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6GroupNotHex(t"abcg")))

      test(m"IPv6 address too many groups"):
        capture(t"1:2:3:4::5:6:7:8".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6TooManyNonzeroGroups(8)))

      test(m"IPv6 address wrong number of groups"):
        capture(t"1:2:3:4:5:6:7:8:9".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6WrongNumberOfGroups(9)))

      test(m"IPv6 duplicate double-colon"):
        capture(t"1::3:7::9".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6MultipleDoubleColons))

      test(m"IPv6 address wrong-length group"):
        capture(t"::8:abcde:abc:1234".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6GroupWrongLength(t"abcde")))

    suite(m"Subnet tests"):
      test(m"Create an IPv4 subnet at compiletime"):
        subnet"192.168.0.0/24"
      . assert(_ == ip"192.168.0.0".subnet(24))

      test(m"IPv4 subnet at compiletime masks host bits"):
        subnet"255.123.143.0/12".show
      . assert(_ == t"255.112.0.0/12")

      test(m"Create an IPv6 subnet at compiletime"):
        subnet"2001:db8::/32"
      . assert(_ == ip"2001:db8::".subnet(32))

      test(m"Show an IPv6 subnet"):
        subnet"2001:db8::/32".show
      . assert(_ == t"2001:db8::/32")

      test(m"Parse an IPv4 subnet at runtime"):
        t"10.0.0.0/8".as[Ipv4Subnet]
      . assert(_ == Ipv4(10, 0, 0, 0).subnet(8))

      test(m"Parse an IPv6 subnet at runtime"):
        t"2001:db8::/32".as[Ipv6Subnet]
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0, 0, 0, 0, 0).subnet(32))

      test(m"IPv4 subnet prefix out of range"):
        capture(t"10.0.0.0/40".as[Ipv4Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv4SubnetPrefixOutOfRange(40)))

      test(m"IPv6 subnet prefix out of range"):
        capture(t"2001:db8::/130".as[Ipv6Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6SubnetPrefixOutOfRange(130)))

      test(m"Subnet prefix not numeric"):
        capture(t"10.0.0.0/x".as[Ipv4Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.SubnetPrefixNotNumeric(t"x")))

      test(m"Subnet without a prefix is wrong format"):
        capture(t"10.0.0.0".as[Ipv4Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.SubnetWrongFormat(1)))

      test(m"Invalid subnet prefix is compile error"):
        demilitarize(subnet"10.0.0.0/40").map(_.message)
      . assert(_ == List(t"[↯SN-077.9] the IP address is not valid because the prefix length 40 is not in the range 0-32"))

    suite(m"Email address tests"):
      import EmailAddress.Error.Reason.*

      test(m"simple@example.com"):
        t"simple@example.com".as[EmailAddress]
      . assert()

      test(m"very.common@example.com"):
        t"very.common@example.com".as[EmailAddress]
      . assert()

      test(m"x@example.com"):
        t"x@example.com".as[EmailAddress]
      . assert()

      test(m"long.email-address-with-hyphens@and.subdomains.example.com"):
        t"long.email-address-with-hyphens@and.subdomains.example.com".as[EmailAddress]
      . assert()

      test(m"user.name+tag+sorting@example.com"):
        t"user.name+tag+sorting@example.com".as[EmailAddress]
      . assert()

      test(m"name/surname@example.com"):
        t"name/surname@example.com".as[EmailAddress]
      . assert()

      test(m"admin@example"):
        t"admin@example".as[EmailAddress]
      . assert()

      test(m"example@s.example"):
        t"example@s.example".as[EmailAddress]
      . assert()

      test(m"\" \"@example.org"):
        t"\" \"@example.org".as[EmailAddress]
      . assert()

      test(m"\"john..doe\"@example.org"):
        t"\"john..doe\"@example.org".as[EmailAddress]
      . assert()

      test(m"mailhost!username@example.org"):
        t"mailhost!username@example.org".as[EmailAddress]
      . assert()

      test(m"\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"):
        t"\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com".as[EmailAddress]
      . assert()

      test(m"user%example.com@example.org"):
        t"user%example.com@example.org".as[EmailAddress]
      . assert()

      test(m"user-@example.org"):
        t"user-@example.org".as[EmailAddress]
      . assert()

      test(m"postmaster@[123.123.123.123]"):
        t"postmaster@[123.123.123.123]".as[EmailAddress]
      . assert()

      test(m"postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"):
        t"postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]".as[EmailAddress]
      . assert()

      test(m"Empty email address"):
        capture(t"".as[EmailAddress])
      . assert(_ == EmailAddress.Error(Empty))

      test(m"abc.example.com"):
        capture(t"abc.example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(MissingAtSymbol))

      test(m"a@b@c@example.com"):
        capture(t"a@b@c@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidDomain(Hostname.Error(t"b@c@example.com", Hostname.Error.Reason.InvalidChar('@')))))

      test(m"a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com"):
        capture(t"a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar('\\')))

      test(m"just\"not\"right@example.com"):
        capture(t"just\"not\"right@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar('\"')))

      test(m"this is\\\"not\\allowed@example.com"):
        capture(t"this is\\\"not\\allowed@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar(' ')))

      test(m"this\\ still\\\"not\\\\allowed@example.com"):
        capture(t"this\\ still\\\"not\\\\allowed@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar('\\')))

      test(m"64-digit local part with tag is too long"):
        capture(t"1234567890123456789012345678901234567890123456789012345678901234+x@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(LongLocalPart))

      test(m"user@[not-an-ip]"):
        capture(t"user@[not-an-ip]".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidDomain(IpAddress.Error(IpAddress.Error.Reason.Ipv4WrongNumberOfGroups(1)))))

      test(m"i.like.underscores@but_they_are_not_allowed_in_this_part"):
        capture(t"i.like.underscores@but_they_are_not_allowed_in_this_part".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidDomain(Hostname.Error(t"but_they_are_not_allowed_in_this_part", Hostname.Error.Reason.InvalidChar('_')))))

      test(m"I❤️CHOCOLATE🍫@example.com"):
        capture(t"I❤️CHOCOLATE🍫@example.com".as[EmailAddress])
      .matches:
        case EmailAddress.Error(InvalidChar(_)) =>

      test(m"Create an email address at compiletime"):
        email"test@example.com"
      . assert(_ == t"test@example.com".as[EmailAddress])

      test(m"Create an IPv4 email address at compiletime"):
        email"test@[192.168.0.1]"
      . assert(_ == t"test@[192.168.0.1]".as[EmailAddress])

      test(m"Create an IPv6 email address at compiletime"):
        email"test@[IPv6:1234::6789]"
      . assert(_ == t"test@[IPv6:1234::6789]".as[EmailAddress])

      test(m"Create a quoted email address at compiletime"):
        email""""test user"@example.com"""
      . assert(_ == t""""test user"@example.com""".as[EmailAddress])

      test(m"forbidden.@example.com"):
        capture(t"forbidden.@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(TerminalPeriod))

      test(m".forbidden@example.com"):
        capture(t".forbidden@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InitialPeriod))

      test(m"not..allowed@example.com"):
        capture(t"not..allowed@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(SuccessivePeriods))

      test(m""""unescaped quote " is forbidden"@example.com"""):
        capture(t""""unescaped quote " is forbidden"@example.com""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(UnescapedQuote))

      test(m""""unclosed.quote@example.com"""):
        capture(t""""unclosed.quote@example.com""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(UnclosedQuote))

      test(m"""missing.domain@"""):
        capture(t"""missing.domain@""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(MissingDomain))

      test(m"""unclosed IP address domain"""):
        capture(t"""user@[192.168.0.1""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(UnclosedIpAddress))

    suite(m"URL tests"):
      test(m"inspect a URL"):
        url"https://example.com/foo/bar".inspect
      . assert(_ == t"https://example.com/foo/bar")

      test(m"parse Authority with username and password"):
        t"username:password@example.com".as[Authority]
      . assert(_ == Authority(example.com, t"username:password"))

      test(m"parse Authority with username but not password"):
        t"username@example.com".as[Authority]
      . assert(_ == Authority(example.com, t"username"))

      test(m"parse Authority with username, password and port"):
        t"username:password@example.com:8080".as[Authority]
      . assert(_ == Authority(example.com, t"username:password", 8080))

      test(m"parse Authority with username and port"):
        t"username@example.com:8080".as[Authority]
      . assert(_ == Authority(example.com, t"username", 8080))

      test(m"parse Authority with username, numerical password and port"):
        t"username:1234@example.com:8080".as[Authority]
      . assert(_ == Authority(example.com, t"username:1234", 8080))

      test(m"Authority with invalid port fails"):
        scala.caps.unsafe.unsafeAssumeSeparate:
          capture(t"username@example.com:no".as[Authority])
      .matches:
        case Url.Error(_, position, Url.Error.Reason.Expected(Url.Error.Expectation.Number)) if position == 21.z =>

      test(m"Parse full URL"):
        t"http://user:pw@example.com:8080/path/to/location?query=1#ref".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, t"user:pw", 8080)),
          t"/path/to/location", t"query=1", t"ref"))

      test(m"Parse simple URL"):
        t"https://example.com/foo".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Https, Authority(example.com)), t"/foo"))

      test(m"Parse url with fragment"):
        t"https://example.com/#id".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Https, Authority(example.com)), t"/", Unset, t"id"))

      test(m"Show simple URL"):
        t"http://example.com/foo".as[HttpUrl].show
      . assert(_ == t"http://example.com/foo")

      test(m"show url with fragment"):
        t"https://example.com/#id".as[HttpUrl].show
      . assert(_ == t"https://example.com/#id")

      test(m"Parse full URL at compiletime"):
        url"http://user:pw@example.com:8080/path/to/location?query=1#ref"
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, t"user:pw", 8080)),
          t"/path/to/location", t"query=1", t"ref"))

      test(m"Parse FTP URL at compiletime"):
        url"ftp://user:pw@example.com:8080/path/to/location"
      . assert(_ == Url(Origin(Scheme(t"ftp"), Authority(example.com, t"user:pw", 8080)),
          t"/path/to/location"))

      test(m"Parse URL at compiletime with substitution"):
        val port = 1234
        url"http://user:pw@example.com:$port/path/to/location"
      . assert(_ == Url(Origin(Scheme(t"http"), Authority(example.com, t"user:pw", 1234)),
          t"/path/to/location"))

      test(m"Parse URL at compiletime with escaped substitution"):
        val message: Text = t"Hello world!"
        url"http://user:pw@example.com/$message"
      . assert(_ == Url(Origin(Scheme(t"http"), Authority(example.com, t"user:pw")), t"/Hello+world%21"))

      test(m"Parse URL with no path"):
        t"http://example.com".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t""))

      test(m"Parse URL with port and no path"):
        t"http://example.com:8080".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, Unset, 8080)), t""))

      test(m"Parse URL with query and no path"):
        t"http://example.com?q=1".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t"", t"q=1"))

      test(m"Parse URL with fragment and no path"):
        t"http://example.com#frag".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t"", Unset, t"frag"))

      test(m"Parse URL with port, query and no path"):
        t"http://example.com:8080?q=1".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, Unset, 8080)), t"", t"q=1"))

      test(m"Parse URL with port, fragment and no path"):
        t"http://example.com:8080#frag".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, Unset, 8080)), t"", Unset, t"frag"))

      test(m"Parse URL with empty query delimiter"):
        t"http://example.com/?".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t"/", t""))

      test(m"Parse URL with empty fragment delimiter"):
        t"http://example.com/#".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t"/", Unset, t""))

      test(m"Parse URL with empty query and empty fragment"):
        t"http://example.com/?#".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t"/", t"", t""))

      test(m"Parse URL with multiple query params"):
        t"http://example.com/a/b?x=1&y=2".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t"/a/b", t"x=1&y=2"))

      test(m"Parse URL with question mark inside query"):
        t"http://example.com/a?x=?".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), t"/a", t"x=?"))

      test(m"Parse RFC 3986 ftp URL"):
        t"ftp://ftp.is.co.za/rfc/rfc1808.txt".as[Url[Label]].show
      . assert(_ == t"ftp://ftp.is.co.za/rfc/rfc1808.txt")

      test(m"Parse RFC 3986 http URL"):
        t"http://www.ietf.org/rfc/rfc2396.txt".as[Url[Label]].show
      . assert(_ == t"http://www.ietf.org/rfc/rfc2396.txt")

      test(m"Parse RFC 3986 mailto URL"):
        t"mailto:John.Doe@example.com".as[Url[Label]].show
      . assert(_ == t"mailto:John.Doe@example.com")

      test(m"Parse RFC 3986 news URL"):
        t"news:comp.infosystems.www.servers.unix".as[Url[Label]].show
      . assert(_ == t"news:comp.infosystems.www.servers.unix")

      test(m"Parse RFC 3986 tel URL"):
        t"tel:+1-816-555-1212".as[Url[Label]].show
      . assert(_ == t"tel:+1-816-555-1212")

      test(m"Parse RFC 3986 urn URL"):
        t"urn:oasis:names:specification:docbook:dtd:xml:4.1.2".as[Url[Label]].show
      . assert(_ == t"urn:oasis:names:specification:docbook:dtd:xml:4.1.2")

      test(m"Parse opaque mailto with query"):
        t"mailto:user@example.com?subject=hi".as[Url[Label]].show
      . assert(_ == t"mailto:user@example.com?subject=hi")

      test(m"Parse opaque data URL with fragment"):
        t"data:text/html,test#test".as[Url[Label]].show
      . assert(_ == t"data:text/html,test#test")

      test(m"Round-trip URL with no path"):
        t"http://example.com".as[HttpUrl].show
      . assert(_ == t"http://example.com")

      test(m"Round-trip URL with port and no path"):
        t"http://example.com:8080".as[HttpUrl].show
      . assert(_ == t"http://example.com:8080")

      test(m"Round-trip URL with query and no path"):
        t"http://example.com?q=1".as[HttpUrl].show
      . assert(_ == t"http://example.com?q=1")

      test(m"Round-trip URL with fragment and no path"):
        t"http://example.com#frag".as[HttpUrl].show
      . assert(_ == t"http://example.com#frag")

      test(m"Parse URL with IPv6 host and port"):
        t"http://[::1]:8080/path".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1), Unset, 8080)),
          t"/path"))

      test(m"Parse URL with IPv6 host and no port"):
        t"http://[::1]/".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), t"/"))

      test(m"Parse URL with IPv6 host, no port, no path"):
        t"http://[::1]".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), t""))

      test(m"Parse URL with IPv6 host, userinfo and port"):
        t"http://user:pw@[2001:db8::1]:443/path".as[HttpUrl]
      . assert(_ == Url(
          Origin(Scheme.Http, Authority(Ipv6(0x2001, 0xdb8, 0, 0, 0, 0, 0, 1), t"user:pw", 443)),
          t"/path"))

      test(m"Parse RFC 3986 ldap URL with IPv6 host"):
        t"ldap://[2001:db8::7]/c=GB?objectClass?one".as[Url[Label]]
      . assert(_ == Url(
          Origin(Scheme(t"ldap"), Authority(Ipv6(0x2001, 0xdb8, 0, 0, 0, 0, 0, 7))),
          t"/c=GB",
          t"objectClass?one"))

      test(m"Round-trip URL with IPv6 host"):
        t"http://[::1]:8080/path".as[HttpUrl].show
      . assert(_ == t"http://[::1]:8080/path")

      test(m"Round-trip RFC 3986 ldap URL"):
        t"ldap://[2001:db8::7]/c=GB?objectClass?one".as[Url[Label]].show
      . assert(_ == t"ldap://[2001:db8::7]/c=GB?objectClass?one")

      test(m"Parse URL with IPv6 host and fragment"):
        t"http://[::1]#frag".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), t"",
          Unset, t"frag"))

      test(m"Parse URL with IPv6 host and query"):
        t"http://[::1]?q=1".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), t"", t"q=1"))

      test(m"Parse URL with IPv4 host"):
        t"http://192.168.0.1/path".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv4(192, 168, 0, 1))), t"/path"))

      test(m"Parse URL with IPv4 host and port"):
        t"http://192.168.0.1:8080/path".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv4(192, 168, 0, 1), Unset, 8080)),
          t"/path"))

      test(m"Round-trip URL with IPv4 host"):
        t"http://192.168.0.1:8080/path".as[HttpUrl].show
      . assert(_ == t"http://192.168.0.1:8080/path")



      // TODO: fix
      // test(m"Relative path is unescaped"):
      //   val message: Text = t"Hello world!"
      //   url"http://user:pw@example.com/$message/foo".path
      // .assert(_ == (? / n"Hello world!" / n"foo").descent)

      // test(m"Relative path with raw substitution is unescaped"):
      //   val message: Raw = Raw(t"Hello+world%21")
      //   url"http://user:pw@example.com/$message/foo".path
      // .assert(_ == (? / n"Hello world!" / n"foo").descent)

    suite(m"Hostname tests"):
      test(m"Parse a simple hostname"):
        t"www.example.com".as[Hostname]
      . assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"example"), DnsLabel(t"com")))

      test(m"Inspect a hostname"):
        t"www.example.com".as[Hostname].inspect
      . assert(_ == t"www.example.com")

      test(m"A hostname cannot end in a period"):
        capture[Hostname.Error](t"www.example.".as[Hostname])
      . assert(_ == Hostname.Error(t"www.example.", Hostname.Error.Reason.EmptyDnsLabel(2)))

      test(m"A hostname cannot start with a period"):
        capture[Hostname.Error](t".example.com".as[Hostname])
      . assert(_ == Hostname.Error(t".example.com", Hostname.Error.Reason.EmptyDnsLabel(0)))

      test(m"A hostname cannot have adjacent periods"):
        capture[Hostname.Error](t"www..com".as[Hostname])
      . assert(_ == Hostname.Error(t"www..com", Hostname.Error.Reason.EmptyDnsLabel(1)))

      test(m"A hostname cannot contain symbols"):
        capture[Hostname.Error](t"www.maybe?.com".as[Hostname])
      . assert(_ == Hostname.Error(t"www.maybe?.com", Hostname.Error.Reason.InvalidChar('?')))

      test(m"A DNS Label cannot begin with a dash"):
        capture[Hostname.Error](t"www.-maybe.com".as[Hostname])
      . assert(_ == Hostname.Error(t"www.-maybe.com", Hostname.Error.Reason.InitialDash(t"-maybe")))

      test(m"A hostname can contain two consecutive dashes"):
        t"www.exam--ple.com".as[Hostname]
      . assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"exam--ple"), DnsLabel(t"com")))

      test(m"A DNS label cannot be longer than 63 characters"):
        capture[Hostname.Error](t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij.com".as[Hostname])
      . assert(_ == Hostname.Error(
        t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij.com",
        Hostname.Error.Reason.LongDnsLabel(t"abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij")
      ))

      test(m"A DNS label may be 63 characters long"):
        t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi.com".as[Hostname]
      . assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi"), DnsLabel(t"com")))

      test(m"A DNS label may be 253 characters long"):
        t"www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.com".as[Hostname]
      . assert()

      test(m"A DNS label may not be longer than 253 characters"):
        capture(t"www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy.com".as[Hostname])
      . assert(_.reason == Hostname.Error.Reason.LongHostname)

      test(m"Parse hostname at compiletime"):
        host"www.altavista.com"
      . assert()

      test(m"Parse bad hostname at compiletime"):
        demilitarize(host"www..com").map(_.message)
      . assert(_ == List(t"[↯SN-892.4] the hostname is not valid because a DNS label cannot be empty"))

    suite(m"MAC Address tests"):
      import MacAddress.Error.Reason.*

      test(m"Test simple MAC address"):
        t"01-23-45-ab-cd-ef".as[MacAddress]
      . assert(_ == MacAddress(1251004370415L))

      test(m"Check MAC address with 5 groups"):
        capture[MacAddress.Error](t"01-23-ab-cd-ef".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupCount(5)))

      test(m"Check MAC address with 7 groups"):
        capture[MacAddress.Error](t"01-23-45-67-ab-cd-ef".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupCount(7)))

      test(m"Check MAC address with short group"):
        capture[MacAddress.Error](t"01-23-45-6-ab-cd".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupLength(3, 1)))

      test(m"Check MAC address with long group"):
        capture[MacAddress.Error](t"01-23-45-67-ab-cde".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupLength(5, 3)))

      test(m"Check MAC address with empty group"):
        capture[MacAddress.Error](t"01-23-45--ab-cd".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupLength(3, 0)))

      test(m"Check MAC address with non-hex character"):
        capture[MacAddress.Error](t"01-23-45-6g-ab-cd".as[MacAddress])
      . assert(_ == MacAddress.Error(NotHex(3, t"6g")))

      test(m"Show a MAC address"):
        t"01-23-45-ab-cd-ef".as[MacAddress].show
      . assert(_ == t"01-23-45-ab-cd-ef")

      test(m"Create a MAC address statically (and show it)"):
        mac"01-23-45-ab-cd-ef".show
      . assert(_ == t"01-23-45-ab-cd-ef")

      test(m"Check that a bad MAC address fails at compiletime"):
        demilitarize:
          mac"01-23-45-ab-cd-e"
        .map(_.message)
      . assert(_ == List(t"[↯SN-532.2] the MAC address is not valid because group 5 should be two hex digits, but its length is 1"))

      test(m"Create a MAC address from bytes"):
        MacAddress(1, 2, 3, 4, 5, 6).show
      . assert(_ == t"01-02-03-04-05-06")

      test(m"Inspect a MAC address"):
        MacAddress(1, 2, 3, 4, 5, 6).inspect
      . assert(_ == t"01-02-03-04-05-06")

    suite(m"Named port services"):
      test(m"Check SMTP over TCP port"):
        tcp"smtp"
      . assert(_ == Port[Tcp](25))

      test(m"Inspect a port"):
        tcp"smtp".inspect
      . assert(_ == t"⌗25")

      test(m"Check Docker over TCP port"):
        tcp"docker"
      . assert(_ == Port[Udp](2375))

      test(m"Check Docker over UDP port is not valid"):
        demilitarize(udp"docker").map(_.message)
      . assert(_ == List(t"[↯SN-915] docker is not a valid UDP port"))

      test(m"Check Nonexistent TCP port does not compile"):
        demilitarize(tcp"abcdef").map(_.message)
      . assert(_ == List(t"[↯SN-915] abcdef is not a valid TCP port"))

    suite(m"Unused port allocation"):
      test(m"An unused TCP port is in the valid range"):
        Port[Tcp]().number
      . assert(1 <= _ <= 65535)

      test(m"An unused UDP port is in the valid range"):
        Port[Udp]().number
      . assert(1 <= _ <= 65535)

      test(m"An unused TCP port can be bound"):
        val port = Port[Tcp]().number
        val socket = java.net.ServerSocket(port)
        try socket.getLocalPort finally socket.close()
      . assert(_ > 0)

    suite(m"Network interface tests"):
      test(m"Enumerate the machine's network interfaces"):
        NetworkInterface.all()
      . assert(!_.nil)

      test(m"There is a loopback interface"):
        NetworkInterface.all().exists(_.loopback)
      . assert(_ == true)

      test(m"An interface can be looked up by its name"):
        val interface = NetworkInterface.all().stdlib.head
        NetworkInterface.byName(interface.name).let(_.name)
      . assert(_ == NetworkInterface.all().stdlib.head.name)

      test(m"An interface can be looked up by its index"):
        val interface = NetworkInterface.all().stdlib.head
        NetworkInterface.byIndex(interface.index).let(_.index)
      . assert(_ == NetworkInterface.all().stdlib.head.index)

      test(m"Looking up a nonexistent interface name yields Unset"):
        NetworkInterface.byName(t"definitely-not-an-interface")
      . assert(_ == Unset)

      test(m"An interface's addresses partition into IPv4 and IPv6"):
        val interface = NetworkInterface.all().stdlib.head
        interface.ipv4.size + interface.ipv6.size
      . assert(_ == NetworkInterface.all().stdlib.head.addresses.size)

      test(m"The loopback interface can be found by its address"):
        val loopback = NetworkInterface.all().filter(_.loopback).stdlib.head
        NetworkInterface.byAddress(loopback.addresses.stdlib.head.address).let(_.loopback)
      . assert(_ == true)

    suite(m"DNS name tests"):
      test(m"Parse a simple name"):
        Dns.Name.parse(t"www.example.com").labels
      . assert(_ == List(t"www", t"example", t"com"))

      test(m"A trailing dot denotes the root and adds no label"):
        Dns.Name.parse(t"example.com.").labels
      . assert(_ == List(t"example", t"com"))

      test(m"A lone dot is the root"):
        Dns.Name.parse(t".")
      . assert(_ == Dns.Name.Root)

      test(m"Names compare without regard to ASCII case"):
        Dns.Name.parse(t"Example.COM") == Dns.Name.parse(t"example.com")
      . assert(_ == true)

      test(m"Case-insensitive names hash alike"):
        Dns.Name.parse(t"Example.COM").hashCode == Dns.Name.parse(t"example.com").hashCode
      . assert(_ == true)

      test(m"Labels keep the case they were written with"):
        Dns.Name.parse(t"Example.COM").labels
      . assert(_ == List(t"Example", t"COM"))

      test(m"An empty label is rejected"):
        capture[Dns.Error](Dns.Name.parse(t"a..b")).reason
      . assert(_ == Dns.Error.Reason.EmptyLabel(t"a..b"))

      test(m"A label of 64 characters is rejected"):
        val label = t"a"*64
        capture[Dns.Error](Dns.Name.parse(t"$label.com")).reason
      . assert(_ == Dns.Error.Reason.LongLabel(t"a"*64))

      test(m"A name of more than 255 octets is rejected"):
        val label = t"a"*63
        capture[Dns.Error](Dns.Name.parse(t"$label.$label.$label.$label.a")).reason
          match
            case Dns.Error.Reason.LongName(_) => true
            case _                            => false
      . assert(_ == true)

      test(m"An escaped dot is part of its label"):
        Dns.Name.parse(t"Jon\\.Printer._ipp._tcp.local").labels.stdlib.head
      . assert(_ == t"Jon.Printer")

      test(m"Showing a name re-escapes dots and backslashes"):
        Dns.Name(t"Jon.Printer\\", t"_ipp", t"_tcp", t"local").show
      . assert(_ == t"Jon\\.Printer\\\\._ipp._tcp.local")

      test(m"A decimal escape yields its character"):
        Dns.Name.parse(t"a\\032b.c").labels.stdlib.head
      . assert(_ == t"a b")

      test(m"A malformed escape is rejected"):
        capture[Dns.Error](Dns.Name.parse(t"a\\9b.c")).reason
      . assert(_ == Dns.Error.Reason.BadEscape(t"a\\9b.c"))

      test(m"Names concatenate with +"):
        (Dns.Name(t"_fury", t"_tcp") + Dns.Name.local).show
      . assert(_ == t"_fury._tcp.local")

      test(m"A label can be prefixed to a name"):
        Dns.Name(t"_fury", t"_tcp", t"local").prefix(t"Gondor").labels
      . assert(_ == List(t"Gondor", t"_fury", t"_tcp", t"local"))

      test(m"A name's parent drops its first label"):
        Dns.Name(t"Gondor", t"_fury", t"_tcp", t"local").parent.let(_.show)
      . assert(_ == t"_fury._tcp.local")

      test(m"The root has no parent"):
        Dns.Name.Root.parent
      . assert(_ == Unset)

      test(m"endsWith folds case"):
        Dns.Name.parse(t"Gondor._fury._tcp.LOCAL").endsWith(Dns.Name.local)
      . assert(_ == true)

      test(m"endsWith rejects a non-suffix"):
        Dns.Name.parse(t"gondor.local").endsWith(Dns.Name(t"example", t"local"))
      . assert(_ == false)

      test(m"The octet length counts each label's length byte and the terminator"):
        Dns.Name.parse(t"example.com").octets
      . assert(_ == 13)

      test(m"A hostname converts to a name"):
        host"example.com".dnsName
      . assert(_ == Dns.Name.parse(t"example.com"))

      test(m"An IPv4 address has a reverse name under in-addr.arpa"):
        ip"192.0.2.1".reverseName.show
      . assert(_ == t"1.2.0.192.in-addr.arpa")

      test(m"An IPv6 address has a reverse name under ip6.arpa"):
        ip"2001:db8::1".reverseName.show
      . assert(_ == t"1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa")

      test(m"A name decodes from text"):
        t"example.com".as[Dns.Name]
      . assert(_ == Dns.Name(t"example", t"com"))

      test(m"Record data carrying octets compares structurally"):
        Dns.Rdata.Txt(t"a", t"b") == Dns.Rdata.Txt(t"a", t"b")
      . assert(_ == true)

      test(m"An unknown record type shows generically"):
        Dns.Type(99).show
      . assert(_ == t"TYPE99")

      test(m"An OPT pseudo-record carries the UDP payload size in its class field"):
        Dns.Record.opt(udpPayload = 4096, dnssecOk = true)
      . assert: record =>
          record.udpPayload == 4096 && record.dnssecOk == true && record.rtype == Dns.Type.Opt

      test(m"localhost resolves through the platform resolver"):
        Dns.resolve(Dns.Name.parse(t"localhost"))
      . assert(!_.nil)

    suite(m"DNS wire format tests"):
      val query = Dns.Message.query(0x1234, List(Dns.Question(Dns.Name(t"example", t"com"), Dns.Type.A)))

      val queryBytes =
        hex"""1234 0100 0001 0000 0000 0000
              07 6578616d706c65 03 636f6d 00 0001 0001"""

      val responseBytes =
        hex"""1234 8180 0001 0001 0000 0000
              07 6578616d706c65 03 636f6d 00 0001 0001
              c00c 0001 0001 00000e10 0004 5db8d822"""

      val instance = Dns.Name(t"Gondor", t"_fury", t"_tcp", t"local")
      val service = Dns.Name(t"_fury", t"_tcp", t"local")
      val host = Dns.Name(t"gondor", t"local")

      val announcement =
        Dns.Message
          ( 0,
            Dns.Flags(response = true, authoritative = true),
            Nil,
            List
              ( Dns.Record(service, 4500, Dns.Rdata.Ptr(instance)),
                Dns.Record(instance, 120, Dns.Rdata.Srv(0, 0, 8080, host), flush = true),
                Dns.Record(instance, 4500, Dns.Rdata.Txt(t"txtvers=1", t""), flush = true),
                Dns.Record(host, 120, Dns.Rdata.A(ip"192.168.1.2"), flush = true) ) )

      // Owner names and the PTR target compress; the SRV target does not (RFC 2782).
      val announcementBytes =
        hex"""0000 8400 0000 0004 0000 0000
              05 5f66757279 04 5f746370 05 6c6f63616c 00 000c 0001 00001194 0009 06 476f6e646f72 c00c
              c028 0021 8001 00000078 0014 0000 0000 1f90 06 676f6e646f72 05 6c6f63616c 00
              c028 0010 8001 00001194 000b 09 747874766572733d31 00
              c043 0001 8001 00000078 0004 c0a80102"""

      def roundtrip(rdata: Dns.Rdata): Dns.Rdata =
        val record = Dns.Record(Dns.Name(t"example", t"com"), 60, rdata)
        val message = Dns.Message(1, Dns.Flags(), Nil, List(record))
        message.in[Data].as[Dns.Message].answers.stdlib.head.rdata

      test(m"A query encodes to its wire form"):
        query.in[Data].serialize[Hex]
      . assert(_ == queryBytes.serialize[Hex])

      test(m"A query decodes from its wire form"):
        queryBytes.as[Dns.Message]
      . assert(_ == query)

      test(m"A compressed response decodes"):
        responseBytes.as[Dns.Message].answers
      . assert(_ == List(Dns.Record(Dns.Name(t"example", t"com"), 3600, Dns.Rdata.A(ip"93.184.216.34"))))

      test(m"Re-encoding a compressed response reproduces its bytes"):
        responseBytes.as[Dns.Message].in[Data].serialize[Hex]
      . assert(_ == responseBytes.serialize[Hex])

      test(m"A DNS-SD announcement encodes with compression and flush bits"):
        announcement.in[Data].serialize[Hex]
      . assert(_ == announcementBytes.serialize[Hex])

      test(m"A DNS-SD announcement decodes to an equal message"):
        announcementBytes.as[Dns.Message]
      . assert(_ == announcement)

      test(m"The cache-flush bit is read from the class field"):
        announcementBytes.as[Dns.Message].answers.map(_.flush)
      . assert(_ == List(false, true, true, true))

      test(m"The class field without its flush bit is the record's class"):
        announcementBytes.as[Dns.Message].answers.map(_.netClass)
      . assert(_ == List.fill(4)(Dns.NetClass.Internet))

      test(m"A compressed SRV target is accepted on input"):
        val bytes =
          hex"""0000 8400 0000 0001 0000 0000
                06 676f6e646f72 05 6c6f63616c 00 0021 0001 00000078 0008 0000 0000 1f90 c00c"""
        bytes.as[Dns.Message].answers.stdlib.head.rdata
      . assert(_ == Dns.Rdata.Srv(0, 0, 8080, host))

      test(m"A pointer to itself is rejected"):
        capture[Dns.Error](hex"0000 0100 0001 0000 0000 0000 c00c 0001 0001".as[Dns.Message]).reason
      . assert(_ == Dns.Error.Reason.BadPointer(12))

      test(m"A forward pointer is rejected"):
        capture[Dns.Error](hex"0000 0100 0001 0000 0000 0000 c00e 0001 0001".as[Dns.Message]).reason
      . assert(_ == Dns.Error.Reason.BadPointer(12))

      test(m"A message cut short in a question is rejected"):
        capture[Dns.Error](hex"0000 0100 0001 0000 0000 0000".as[Dns.Message]).reason
      . assert(_ == Dns.Error.Reason.Truncated(12))

      test(m"Trailing bytes are rejected"):
        capture[Dns.Error](hex"1234 0100 0000 0000 0000 0000 00".as[Dns.Message]).reason
      . assert(_ == Dns.Error.Reason.Trailing(12))

      test(m"Record data of the wrong length is rejected"):
        val bytes = hex"0000 8400 0000 0001 0000 0000 00 0001 0001 00000078 0003 c0a801"
        capture[Dns.Error](bytes.as[Dns.Message]).reason
      . assert(_ == Dns.Error.Reason.BadRdata(Dns.Type.A, 23))

      test(m"Every record type round-trips"):
        val name = Dns.Name(t"ns", t"example", t"com")
        List
          ( Dns.Rdata.A(ip"10.0.0.1"),
            Dns.Rdata.Aaaa(ip"2001:db8::1"),
            Dns.Rdata.Ptr(name),
            Dns.Rdata.Cname(name),
            Dns.Rdata.Ns(name),
            Dns.Rdata.Mx(10, name),
            Dns.Rdata.Srv(1, 2, 443, name),
            Dns.Rdata.Txt(t"a=1", t"b"),
            Dns.Rdata.Soa(name, Dns.Name(t"hostmaster", t"example", t"com"), 2026100601L, 7200, 900, 1209600, 300),
            Dns.Rdata.Opt(List((10, hex"0102"))),
            Dns.Rdata.Unknown(Dns.Type(99), hex"deadbeef") )
        . map(rdata => roundtrip(rdata) == rdata)
      . assert(_.all(_ == true))

      test(m"An empty TXT record encodes as one empty string"):
        val record = Dns.Record(host, 60, Dns.Rdata.Txt(Nil))
        Dns.Message(1, Dns.Flags(), Nil, List(record)).in[Data].serialize[Hex]
      . assert(_.ends(hex"0001 00".serialize[Hex]))

      test(m"A 255-byte TXT string is accepted"):
        roundtrip(Dns.Rdata.Txt(t"a"*255))
      . assert(_ == Dns.Rdata.Txt(t"a"*255))

      test(m"A 256-byte TXT string is rejected at construction"):
        capture[Dns.Error](Dns.Rdata.Txt(t"a"*256)).reason
      . assert(_ == Dns.Error.Reason.LongString(256))

      test(m"An overlong label is rejected at construction"):
        capture[Dns.Error](Dns.Name(t"a"*64, t"local")).reason
      . assert(_ == Dns.Error.Reason.LongLabel(t"a"*64))

      test(m"Concatenation beyond 255 octets is rejected"):
        val long = Dns.Name(t"a"*63, t"a"*63, t"a"*63)
        capture[Dns.Error](long + long).reason match
          case Dns.Error.Reason.LongName(_) => true
          case _                            => false
      . assert(_ == true)

      test(m"The dns interpolator yields a name at compile time"):
        dns"_fury._tcp.local"
      . assert(_ == Dns.Name(t"_fury", t"_tcp", t"local"))

      test(m"The dns interpolator rejects an invalid name at compile time"):
        demilitarize(dns"a..b").map(_.message).nonEmpty
      . assert(_ == true)

      val unicastQuestion = Dns.Question(service, Dns.Type.Ptr, unicast = true)

      test(m"A unicast-response question sets the top bit of its class"):
        Dns.Message.query(0, List(unicastQuestion), false).in[Data].serialize[Hex]
      . assert(_.ends(hex"000c 8001".serialize[Hex]))

      test(m"A unicast-response question decodes with its class intact"):
        Dns.Message.query(0, List(unicastQuestion), false).in[Data].as[Dns.Message].questions
      . assert(_ == List(unicastQuestion))

      test(m"An OPT record keeps its payload size and flags through the wire"):
        val message = Dns.Message(1, Dns.Flags(), Nil, Nil, Nil, List(Dns.Record.opt(4096, true)))
        message.in[Data].as[Dns.Message].additional.stdlib.head
      . assert: record =>
          record.udpPayload == 4096 && record.dnssecOk == true && record.flush == false

      test(m"A TTL with its top bit set reads as zero"):
        hex"0000 8400 0000 0001 0000 0000 00 0001 0001 ffffffff 0004 c0a80102".as[Dns.Message]
        . answers.stdlib.head.ttl
      . assert(_ == 0)

      test(m"Record data in canonical form is uncompressed"):
        Dns.Record(instance, 120, Dns.Rdata.Srv(0, 0, 8080, host)).rdataBytes.serialize[Hex]
      . assert(_ == hex"0000 0000 1f90 06 676f6e646f72 05 6c6f63616c 00".serialize[Hex])

      val allFlags =
        Dns.Flags(true, Dns.Opcode.Notify, true, true, true, true, true, true, Dns.Rcode.Refused)

      test(m"Flags round-trip"):
        Dns.Message(0xffff, allFlags).in[Data].as[Dns.Message].flags
      . assert(_ == allFlags)

      test(m"A response echoes the query's ID and questions"):
        Dns.Message.response(query, Nil)
      . assert: response =>
          response.id == 0x1234 && response.questions == query.questions && response.flags.response

object example:
  val com = Hostname(DnsLabel(t"example"), DnsLabel(t"com"))
