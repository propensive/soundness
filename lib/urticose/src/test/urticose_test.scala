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
      . assert(_ == "https://example.com/path")

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
           "www.example.com".as[Hostname].inspect,
           "simple@example.com".as[EmailAddress].inspect,
           url"https://example.com/path".inspect,
           url"https://example.com/path".scheme.inspect,
           "user@example.com:8080".as[Authority].inspect,
           Endpoint("example.com", 8080).inspect )
      . assert(_ == Nil)

      test(m"An authority inspects as its URL form, introduced by `//`"):
        "user@example.com:8080".as[Authority].inspect
      . assert(_ == "//user@example.com:8080")

      test(m"An endpoint inspects both its remote and its port"):
        Endpoint("example.com", 8080).inspect
      . assert(_ == """Endpoint(t"example.com":8080)""")

    suite(m"IPv4 tests"):
      test(m"Parse in IPv4 address"):
        "1.2.3.4".as[Ipv4]
      . assert(_ == Ipv4(1, 2, 3, 4))

      test(m"Show an Ipv4 address"):
        Ipv4(127, 244, 197, 0).show
      . assert(_ == "127.244.197.0")

      test(m"Show a zero Ipv4 address"):
        Ipv4(0, 0, 0, 0).show
      . assert(_ == "0.0.0.0")

      test(m"Show a 'maximum' Ipv4 address"):
        Ipv4(255, 255, 255, 255).show
      . assert(_ == "255.255.255.255")

      test(m"Get an IP address as an integer"):
        Ipv4(192, 168, 0, 1).int
      . assert(_ == bin"11000000 10101000 00000000 00000001")

      test(m"Inspect an Ipv4 address"):
        Ipv4(127, 244, 197, 0).inspect
      . assert(_ == "127.244.197.0")

    suite(m"IPv6 tests"):
      test(m"Parse an IPv6 address"):
        "2001:db8:0000:1:1:1:1:1".as[Ipv6]
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))

      test(m"Render an IPv6 address"):
        "2001:db8:0000:1:1:1:1:1".as[Ipv6].show
      . assert(_ == "2001:db8:0:1:1:1:1:1")

      test(m"Inspect an IPv6 address"):
        "2001:db8:0000:1:1:1:1:1".as[Ipv6].inspect
      . assert(_ == "2001:db8:0:1:1:1:1:1")

      test(m"Parse zero IPv6 address"):
        "::".as[Ipv6]
      . assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 0))

      test(m"Parse zero-leading IPv6 address"):
        "::2".as[Ipv6]
      . assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 2))

      test(m"Parse zeroes-trailing IPv6 address"):
        "8::".as[Ipv6]
      . assert(_ == Ipv6(8, 0, 0, 0, 0, 0, 0, 0))

      test(m"Show zero IPv6 address"):
        Ipv6(0, 0, 0, 0, 0, 0, 0, 0).show
      . assert(_ == "::")

      test(m"Show zero-leading IPv6 address"):
        Ipv6(0, 0, 0, 0, 0, 0, 0, 1).show
      . assert(_ == "::1")

      test(m"Show zeroes-trailing IPv6 address"):
        Ipv6(8, 0, 0, 0, 0, 0, 0, 0).show
      . assert(_ == "8::")

      test(m"Parse IPv4 address at compiletime"):
        ip"122.0.0.1"
      . assert(_ == Ipv4(122, 0, 0, 1))

      test(m"Parse an IPv6 address at compiletime"):
        ip"2001:db8::1:1:1:1"
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0, 0x1, 0x1, 0x1, 0x1))

      test(m"Create and show a subnet"):
        (ip"255.123.143.0".subnet(12)).show
      . assert(_ == "255.112.0.0/12")

      test(m"Parse an IPv6 containing capital letters"):
        "2001:DB8::1:1:1:1:1".as[Ipv6]
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))

      test(m"Invalid IP address is compile error"):
        demilitarize(ip"192.168.0.0.0.1").map(_.message)
      . assert(_ == List(t"[↯SN-077.3] the IP address is not valid because the address contains 6 period-separated groups instead of 4"))

      test(m"IP address byte out of range"):
        capture("100.300.200.0".as[Ipv4])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv4ByteOutOfRange(300)))

      test(m"IPv4 address wrong number of bytes"):
        capture("10.3.20.0.8".as[Ipv4])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv4WrongNumberOfGroups(5)))

      test(m"IPv6 address non-hex value"):
        capture("::8:abcg:abc:1234".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6GroupNotHex("abcg")))

      test(m"IPv6 address too many groups"):
        capture("1:2:3:4::5:6:7:8".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6TooManyNonzeroGroups(8)))

      test(m"IPv6 address wrong number of groups"):
        capture("1:2:3:4:5:6:7:8:9".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6WrongNumberOfGroups(9)))

      test(m"IPv6 duplicate double-colon"):
        capture("1::3:7::9".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6MultipleDoubleColons))

      test(m"IPv6 address wrong-length group"):
        capture("::8:abcde:abc:1234".as[Ipv6])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6GroupWrongLength("abcde")))

    suite(m"Subnet tests"):
      test(m"Create an IPv4 subnet at compiletime"):
        subnet"192.168.0.0/24"
      . assert(_ == ip"192.168.0.0".subnet(24))

      test(m"IPv4 subnet at compiletime masks host bits"):
        subnet"255.123.143.0/12".show
      . assert(_ == "255.112.0.0/12")

      test(m"Create an IPv6 subnet at compiletime"):
        subnet"2001:db8::/32"
      . assert(_ == ip"2001:db8::".subnet(32))

      test(m"Show an IPv6 subnet"):
        subnet"2001:db8::/32".show
      . assert(_ == "2001:db8::/32")

      test(m"Parse an IPv4 subnet at runtime"):
        "10.0.0.0/8".as[Ipv4Subnet]
      . assert(_ == Ipv4(10, 0, 0, 0).subnet(8))

      test(m"Parse an IPv6 subnet at runtime"):
        "2001:db8::/32".as[Ipv6Subnet]
      . assert(_ == Ipv6(0x2001, 0xdb8, 0, 0, 0, 0, 0, 0).subnet(32))

      test(m"IPv4 subnet prefix out of range"):
        capture("10.0.0.0/40".as[Ipv4Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv4SubnetPrefixOutOfRange(40)))

      test(m"IPv6 subnet prefix out of range"):
        capture("2001:db8::/130".as[Ipv6Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.Ipv6SubnetPrefixOutOfRange(130)))

      test(m"Subnet prefix not numeric"):
        capture("10.0.0.0/x".as[Ipv4Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.SubnetPrefixNotNumeric("x")))

      test(m"Subnet without a prefix is wrong format"):
        capture("10.0.0.0".as[Ipv4Subnet])
      . assert(_ == IpAddress.Error(IpAddress.Error.Reason.SubnetWrongFormat(1)))

      test(m"Invalid subnet prefix is compile error"):
        demilitarize(subnet"10.0.0.0/40").map(_.message)
      . assert(_ == List(t"[↯SN-077.9] the IP address is not valid because the prefix length 40 is not in the range 0-32"))

    suite(m"Email address tests"):
      import EmailAddress.Error.Reason.*

      test(m"simple@example.com"):
        "simple@example.com".as[EmailAddress]
      . assert()

      test(m"very.common@example.com"):
        "very.common@example.com".as[EmailAddress]
      . assert()

      test(m"x@example.com"):
        "x@example.com".as[EmailAddress]
      . assert()

      test(m"long.email-address-with-hyphens@and.subdomains.example.com"):
        "long.email-address-with-hyphens@and.subdomains.example.com".as[EmailAddress]
      . assert()

      test(m"user.name+tag+sorting@example.com"):
        "user.name+tag+sorting@example.com".as[EmailAddress]
      . assert()

      test(m"name/surname@example.com"):
        "name/surname@example.com".as[EmailAddress]
      . assert()

      test(m"admin@example"):
        "admin@example".as[EmailAddress]
      . assert()

      test(m"example@s.example"):
        "example@s.example".as[EmailAddress]
      . assert()

      test(m"\" \"@example.org"):
        "\" \"@example.org".as[EmailAddress]
      . assert()

      test(m"\"john..doe\"@example.org"):
        "\"john..doe\"@example.org".as[EmailAddress]
      . assert()

      test(m"mailhost!username@example.org"):
        "mailhost!username@example.org".as[EmailAddress]
      . assert()

      test(m"\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"):
        "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com".as[EmailAddress]
      . assert()

      test(m"user%example.com@example.org"):
        "user%example.com@example.org".as[EmailAddress]
      . assert()

      test(m"user-@example.org"):
        "user-@example.org".as[EmailAddress]
      . assert()

      test(m"postmaster@[123.123.123.123]"):
        "postmaster@[123.123.123.123]".as[EmailAddress]
      . assert()

      test(m"postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"):
        "postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]".as[EmailAddress]
      . assert()

      test(m"Empty email address"):
        capture("".as[EmailAddress])
      . assert(_ == EmailAddress.Error(Empty))

      test(m"abc.example.com"):
        capture("abc.example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(MissingAtSymbol))

      test(m"a@b@c@example.com"):
        capture("a@b@c@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidDomain(Hostname.Error("b@c@example.com", Hostname.Error.Reason.InvalidChar('@')))))

      test(m"a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com"):
        capture("a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar('\\')))

      test(m"just\"not\"right@example.com"):
        capture("just\"not\"right@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar('\"')))

      test(m"this is\\\"not\\allowed@example.com"):
        capture("this is\\\"not\\allowed@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar(' ')))

      test(m"this\\ still\\\"not\\\\allowed@example.com"):
        capture("this\\ still\\\"not\\\\allowed@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidChar('\\')))

      test(m"64-digit local part with tag is too long"):
        capture("1234567890123456789012345678901234567890123456789012345678901234+x@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(LongLocalPart))

      test(m"user@[not-an-ip]"):
        capture("user@[not-an-ip]".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidDomain(IpAddress.Error(IpAddress.Error.Reason.Ipv4WrongNumberOfGroups(1)))))

      test(m"i.like.underscores@but_they_are_not_allowed_in_this_part"):
        capture("i.like.underscores@but_they_are_not_allowed_in_this_part".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InvalidDomain(Hostname.Error("but_they_are_not_allowed_in_this_part", Hostname.Error.Reason.InvalidChar('_')))))

      test(m"I❤️CHOCOLATE🍫@example.com"):
        capture("I❤️CHOCOLATE🍫@example.com".as[EmailAddress])
      .matches:
        case EmailAddress.Error(InvalidChar(_)) =>

      test(m"Create an email address at compiletime"):
        email"test@example.com"
      . assert(_ == "test@example.com".as[EmailAddress])

      test(m"Create an IPv4 email address at compiletime"):
        email"test@[192.168.0.1]"
      . assert(_ == "test@[192.168.0.1]".as[EmailAddress])

      test(m"Create an IPv6 email address at compiletime"):
        email"test@[IPv6:1234::6789]"
      . assert(_ == "test@[IPv6:1234::6789]".as[EmailAddress])

      test(m"Create a quoted email address at compiletime"):
        email""""test user"@example.com"""
      . assert(_ == """"test user"@example.com""".as[EmailAddress])

      test(m"forbidden.@example.com"):
        capture("forbidden.@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(TerminalPeriod))

      test(m".forbidden@example.com"):
        capture(".forbidden@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(InitialPeriod))

      test(m"not..allowed@example.com"):
        capture("not..allowed@example.com".as[EmailAddress])
      . assert(_ == EmailAddress.Error(SuccessivePeriods))

      test(m""""unescaped quote " is forbidden"@example.com"""):
        capture(""""unescaped quote " is forbidden"@example.com""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(UnescapedQuote))

      test(m""""unclosed.quote@example.com"""):
        capture(""""unclosed.quote@example.com""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(UnclosedQuote))

      test(m"""missing.domain@"""):
        capture("""missing.domain@""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(MissingDomain))

      test(m"""unclosed IP address domain"""):
        capture("""user@[192.168.0.1""".as[EmailAddress])
      . assert(_ == EmailAddress.Error(UnclosedIpAddress))

    suite(m"URL tests"):
      test(m"inspect a URL"):
        url"https://example.com/foo/bar".inspect
      . assert(_ == "https://example.com/foo/bar")

      test(m"parse Authority with username and password"):
        "username:password@example.com".as[Authority]
      . assert(_ == Authority(example.com, "username:password"))

      test(m"parse Authority with username but not password"):
        "username@example.com".as[Authority]
      . assert(_ == Authority(example.com, "username"))

      test(m"parse Authority with username, password and port"):
        "username:password@example.com:8080".as[Authority]
      . assert(_ == Authority(example.com, "username:password", 8080))

      test(m"parse Authority with username and port"):
        "username@example.com:8080".as[Authority]
      . assert(_ == Authority(example.com, "username", 8080))

      test(m"parse Authority with username, numerical password and port"):
        "username:1234@example.com:8080".as[Authority]
      . assert(_ == Authority(example.com, "username:1234", 8080))

      test(m"Authority with invalid port fails"):
        scala.caps.unsafe.unsafeAssumeSeparate:
          capture("username@example.com:no".as[Authority])
      .matches:
        case Url.Error(_, position, Url.Error.Reason.Expected(Url.Error.Expectation.Number)) if position == 21.z =>

      test(m"Parse full URL"):
        "http://user:pw@example.com:8080/path/to/location?query=1#ref".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, "user:pw", 8080)),
          "/path/to/location", "query=1", "ref"))

      test(m"Parse simple URL"):
        "https://example.com/foo".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Https, Authority(example.com)), "/foo"))

      test(m"Parse url with fragment"):
        "https://example.com/#id".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Https, Authority(example.com)), "/", Unset, "id"))

      test(m"Show simple URL"):
        "http://example.com/foo".as[HttpUrl].show
      . assert(_ == "http://example.com/foo")

      test(m"show url with fragment"):
        "https://example.com/#id".as[HttpUrl].show
      . assert(_ == "https://example.com/#id")

      test(m"Parse full URL at compiletime"):
        url"http://user:pw@example.com:8080/path/to/location?query=1#ref"
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, "user:pw", 8080)),
          "/path/to/location", "query=1", "ref"))

      test(m"Parse FTP URL at compiletime"):
        url"ftp://user:pw@example.com:8080/path/to/location"
      . assert(_ == Url(Origin(Scheme("ftp"), Authority(example.com, "user:pw", 8080)),
          "/path/to/location"))

      test(m"Parse URL at compiletime with substitution"):
        val port = 1234
        url"http://user:pw@example.com:$port/path/to/location"
      . assert(_ == Url(Origin(Scheme("http"), Authority(example.com, "user:pw", 1234)),
          "/path/to/location"))

      test(m"Parse URL at compiletime with escaped substitution"):
        val message: Text = "Hello world!"
        url"http://user:pw@example.com/$message"
      . assert(_ == Url(Origin(Scheme("http"), Authority(example.com, "user:pw")), "/Hello+world%21"))

      test(m"Parse URL with no path"):
        "http://example.com".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), ""))

      test(m"Parse URL with port and no path"):
        "http://example.com:8080".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, Unset, 8080)), ""))

      test(m"Parse URL with query and no path"):
        "http://example.com?q=1".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), "", "q=1"))

      test(m"Parse URL with fragment and no path"):
        "http://example.com#frag".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), "", Unset, "frag"))

      test(m"Parse URL with port, query and no path"):
        "http://example.com:8080?q=1".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, Unset, 8080)), "", "q=1"))

      test(m"Parse URL with port, fragment and no path"):
        "http://example.com:8080#frag".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com, Unset, 8080)), "", Unset, "frag"))

      test(m"Parse URL with empty query delimiter"):
        "http://example.com/?".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), "/", ""))

      test(m"Parse URL with empty fragment delimiter"):
        "http://example.com/#".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), "/", Unset, ""))

      test(m"Parse URL with empty query and empty fragment"):
        "http://example.com/?#".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), "/", "", ""))

      test(m"Parse URL with multiple query params"):
        "http://example.com/a/b?x=1&y=2".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), "/a/b", "x=1&y=2"))

      test(m"Parse URL with question mark inside query"):
        "http://example.com/a?x=?".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(example.com)), "/a", "x=?"))

      test(m"Parse RFC 3986 ftp URL"):
        "ftp://ftp.is.co.za/rfc/rfc1808.txt".as[Url[Label]].show
      . assert(_ == "ftp://ftp.is.co.za/rfc/rfc1808.txt")

      test(m"Parse RFC 3986 http URL"):
        "http://www.ietf.org/rfc/rfc2396.txt".as[Url[Label]].show
      . assert(_ == "http://www.ietf.org/rfc/rfc2396.txt")

      test(m"Parse RFC 3986 mailto URL"):
        "mailto:John.Doe@example.com".as[Url[Label]].show
      . assert(_ == "mailto:John.Doe@example.com")

      test(m"Parse RFC 3986 news URL"):
        "news:comp.infosystems.www.servers.unix".as[Url[Label]].show
      . assert(_ == "news:comp.infosystems.www.servers.unix")

      test(m"Parse RFC 3986 tel URL"):
        "tel:+1-816-555-1212".as[Url[Label]].show
      . assert(_ == "tel:+1-816-555-1212")

      test(m"Parse RFC 3986 urn URL"):
        "urn:oasis:names:specification:docbook:dtd:xml:4.1.2".as[Url[Label]].show
      . assert(_ == "urn:oasis:names:specification:docbook:dtd:xml:4.1.2")

      test(m"Parse opaque mailto with query"):
        "mailto:user@example.com?subject=hi".as[Url[Label]].show
      . assert(_ == "mailto:user@example.com?subject=hi")

      test(m"Parse opaque data URL with fragment"):
        "data:text/html,test#test".as[Url[Label]].show
      . assert(_ == "data:text/html,test#test")

      test(m"Round-trip URL with no path"):
        "http://example.com".as[HttpUrl].show
      . assert(_ == "http://example.com")

      test(m"Round-trip URL with port and no path"):
        "http://example.com:8080".as[HttpUrl].show
      . assert(_ == "http://example.com:8080")

      test(m"Round-trip URL with query and no path"):
        "http://example.com?q=1".as[HttpUrl].show
      . assert(_ == "http://example.com?q=1")

      test(m"Round-trip URL with fragment and no path"):
        "http://example.com#frag".as[HttpUrl].show
      . assert(_ == "http://example.com#frag")

      test(m"Parse URL with IPv6 host and port"):
        "http://[::1]:8080/path".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1), Unset, 8080)),
          "/path"))

      test(m"Parse URL with IPv6 host and no port"):
        "http://[::1]/".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), "/"))

      test(m"Parse URL with IPv6 host, no port, no path"):
        "http://[::1]".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), ""))

      test(m"Parse URL with IPv6 host, userinfo and port"):
        "http://user:pw@[2001:db8::1]:443/path".as[HttpUrl]
      . assert(_ == Url(
          Origin(Scheme.Http, Authority(Ipv6(0x2001, 0xdb8, 0, 0, 0, 0, 0, 1), "user:pw", 443)),
          "/path"))

      test(m"Parse RFC 3986 ldap URL with IPv6 host"):
        "ldap://[2001:db8::7]/c=GB?objectClass?one".as[Url[Label]]
      . assert(_ == Url(
          Origin(Scheme("ldap"), Authority(Ipv6(0x2001, 0xdb8, 0, 0, 0, 0, 0, 7))),
          "/c=GB",
          "objectClass?one"))

      test(m"Round-trip URL with IPv6 host"):
        "http://[::1]:8080/path".as[HttpUrl].show
      . assert(_ == "http://[::1]:8080/path")

      test(m"Round-trip RFC 3986 ldap URL"):
        "ldap://[2001:db8::7]/c=GB?objectClass?one".as[Url[Label]].show
      . assert(_ == "ldap://[2001:db8::7]/c=GB?objectClass?one")

      test(m"Parse URL with IPv6 host and fragment"):
        "http://[::1]#frag".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), "",
          Unset, "frag"))

      test(m"Parse URL with IPv6 host and query"):
        "http://[::1]?q=1".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv6(0, 0, 0, 0, 0, 0, 0, 1))), "", "q=1"))

      test(m"Parse URL with IPv4 host"):
        "http://192.168.0.1/path".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv4(192, 168, 0, 1))), "/path"))

      test(m"Parse URL with IPv4 host and port"):
        "http://192.168.0.1:8080/path".as[HttpUrl]
      . assert(_ == Url(Origin(Scheme.Http, Authority(Ipv4(192, 168, 0, 1), Unset, 8080)),
          "/path"))

      test(m"Round-trip URL with IPv4 host"):
        "http://192.168.0.1:8080/path".as[HttpUrl].show
      . assert(_ == "http://192.168.0.1:8080/path")



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
        "www.example.com".as[Hostname]
      . assert(_ == Hostname(DnsLabel("www"), DnsLabel("example"), DnsLabel("com")))

      test(m"Inspect a hostname"):
        "www.example.com".as[Hostname].inspect
      . assert(_ == "www.example.com")

      test(m"A hostname cannot end in a period"):
        capture[Hostname.Error]("www.example.".as[Hostname])
      . assert(_ == Hostname.Error("www.example.", Hostname.Error.Reason.EmptyDnsLabel(2)))

      test(m"A hostname cannot start with a period"):
        capture[Hostname.Error](".example.com".as[Hostname])
      . assert(_ == Hostname.Error(".example.com", Hostname.Error.Reason.EmptyDnsLabel(0)))

      test(m"A hostname cannot have adjacent periods"):
        capture[Hostname.Error]("www..com".as[Hostname])
      . assert(_ == Hostname.Error("www..com", Hostname.Error.Reason.EmptyDnsLabel(1)))

      test(m"A hostname cannot contain symbols"):
        capture[Hostname.Error]("www.maybe?.com".as[Hostname])
      . assert(_ == Hostname.Error("www.maybe?.com", Hostname.Error.Reason.InvalidChar('?')))

      test(m"A DNS Label cannot begin with a dash"):
        capture[Hostname.Error]("www.-maybe.com".as[Hostname])
      . assert(_ == Hostname.Error("www.-maybe.com", Hostname.Error.Reason.InitialDash("-maybe")))

      test(m"A hostname can contain two consecutive dashes"):
        "www.exam--ple.com".as[Hostname]
      . assert(_ == Hostname(DnsLabel("www"), DnsLabel("exam--ple"), DnsLabel("com")))

      test(m"A DNS label cannot be longer than 63 characters"):
        capture[Hostname.Error]("www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij.com".as[Hostname])
      . assert(_ == Hostname.Error(
        "www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij.com",
        Hostname.Error.Reason.LongDnsLabel("abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij")
      ))

      test(m"A DNS label may be 63 characters long"):
        "www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi.com".as[Hostname]
      . assert(_ == Hostname(DnsLabel("www"), DnsLabel("abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi"), DnsLabel("com")))

      test(m"A DNS label may be 253 characters long"):
        "www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.com".as[Hostname]
      . assert()

      test(m"A DNS label may not be longer than 253 characters"):
        capture("www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy.com".as[Hostname])
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
        "01-23-45-ab-cd-ef".as[MacAddress]
      . assert(_ == MacAddress(1251004370415L))

      test(m"Check MAC address with 5 groups"):
        capture[MacAddress.Error]("01-23-ab-cd-ef".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupCount(5)))

      test(m"Check MAC address with 7 groups"):
        capture[MacAddress.Error]("01-23-45-67-ab-cd-ef".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupCount(7)))

      test(m"Check MAC address with short group"):
        capture[MacAddress.Error]("01-23-45-6-ab-cd".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupLength(3, 1)))

      test(m"Check MAC address with long group"):
        capture[MacAddress.Error]("01-23-45-67-ab-cde".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupLength(5, 3)))

      test(m"Check MAC address with empty group"):
        capture[MacAddress.Error]("01-23-45--ab-cd".as[MacAddress])
      . assert(_ == MacAddress.Error(WrongGroupLength(3, 0)))

      test(m"Check MAC address with non-hex character"):
        capture[MacAddress.Error]("01-23-45-6g-ab-cd".as[MacAddress])
      . assert(_ == MacAddress.Error(NotHex(3, "6g")))

      test(m"Show a MAC address"):
        "01-23-45-ab-cd-ef".as[MacAddress].show
      . assert(_ == "01-23-45-ab-cd-ef")

      test(m"Create a MAC address statically (and show it)"):
        mac"01-23-45-ab-cd-ef".show
      . assert(_ == "01-23-45-ab-cd-ef")

      test(m"Check that a bad MAC address fails at compiletime"):
        demilitarize:
          mac"01-23-45-ab-cd-e"
        .map(_.message)
      . assert(_ == List(t"[↯SN-532.2] the MAC address is not valid because group 5 should be two hex digits, but its length is 1"))

      test(m"Create a MAC address from bytes"):
        MacAddress(1, 2, 3, 4, 5, 6).show
      . assert(_ == "01-02-03-04-05-06")

      test(m"Inspect a MAC address"):
        MacAddress(1, 2, 3, 4, 5, 6).inspect
      . assert(_ == "01-02-03-04-05-06")

    suite(m"Named port services"):
      test(m"Check SMTP over TCP port"):
        tcp"smtp"
      . assert(_ == Port[Tcp](25))

      test(m"Inspect a port"):
        tcp"smtp".inspect
      . assert(_ == "⌗25")

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
        NetworkInterface.byName("definitely-not-an-interface")
      . assert(_ == Unset)

      test(m"An interface's addresses partition into IPv4 and IPv6"):
        val interface = NetworkInterface.all().stdlib.head
        interface.ipv4.size + interface.ipv6.size
      . assert(_ == NetworkInterface.all().stdlib.head.addresses.size)

      test(m"The loopback interface can be found by its address"):
        val loopback = NetworkInterface.all().filter(_.loopback).stdlib.head
        NetworkInterface.byAddress(loopback.addresses.stdlib.head.address).let(_.loopback)
      . assert(_ == true)

object example:
  val com = Hostname(DnsLabel("example"), DnsLabel("com"))
