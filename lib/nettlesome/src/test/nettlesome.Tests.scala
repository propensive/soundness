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
┃    Soundness, version 0.33.0.                                                                    ┃
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
package nettlesome

import soundness.*
import fulminate.errorDiagnostics.stackTraces
import strategies.throwUnsafely

object Tests extends Suite(m"Nettlesome tests"):
  def run(): Unit =
    suite(m"Internet tests"):
      def remoteCall()(using Internet): Unit = ()

      test(m"Check remote call is callable with `Internet`"):
        internet(true):
          remoteCall()
      .assert()

      // TODO: fix
      // test(m"Check remote call is not callable without `Internet`"):
      //   val result = demilitarize:
      //     remoteCall()
      //   .map(_.id)
      //   println(result)
      //   result
      // .assert(_ == List(CompileErrorId.MissingImplicitArgument))


    suite(m"IPv4 tests"):
      test(m"Parse in IPv4 address"):
        Ipv4.parse(t"1.2.3.4")
      .assert(_ == Ipv4(1, 2, 3, 4))

      test(m"Show an Ipv4 address"):
        Ipv4(127, 244, 197, 0).show
      .assert(_ == t"127.244.197.0")

      test(m"Show a zero Ipv4 address"):
        Ipv4(0, 0, 0, 0).show
      .assert(_ == t"0.0.0.0")

      test(m"Show a 'maximum' Ipv4 address"):
        Ipv4(255, 255, 255, 255).show
      .assert(_ == t"255.255.255.255")

      test(m"Get an IP address as an integer"):
        Ipv4(192, 168, 0, 1).int
      .assert(_ == bin"11000000 10101000 00000000 00000001")

    suite(m"IPv6 tests"):
      test(m"Parse an IPv6 address"):
        Ipv6.parse(t"2001:db8:0000:1:1:1:1:1")
      .assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))

      test(m"Render an IPv6 address"):
        Ipv6.parse(t"2001:db8:0000:1:1:1:1:1").show
      .assert(_ == t"2001:db8:0:1:1:1:1:1")

      test(m"Parse zero IPv6 address"):
        Ipv6.parse(t"::")
      .assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 0))

      test(m"Parse zero-leading IPv6 address"):
        Ipv6.parse(t"::2")
      .assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 2))

      test(m"Parse zeroes-trailing IPv6 address"):
        Ipv6.parse(t"8::")
      .assert(_ == Ipv6(8, 0, 0, 0, 0, 0, 0, 0))

      test(m"Show zero IPv6 address"):
        Ipv6(0, 0, 0, 0, 0, 0, 0, 0).show
      .assert(_ == t"::")

      test(m"Show zero-leading IPv6 address"):
        Ipv6(0, 0, 0, 0, 0, 0, 0, 1).show
      .assert(_ == t"::1")

      test(m"Show zeroes-trailing IPv6 address"):
        Ipv6(8, 0, 0, 0, 0, 0, 0, 0).show
      .assert(_ == t"8::")

      test(m"Parse IPv4 address at compiletime"):
        ip"122.0.0.1"
      .assert(_ == Ipv4(122, 0, 0, 1))

      test(m"Parse an IPv6 address at compiletime"):
        ip"2001:db8::1:1:1:1"
      .assert(_ == Ipv6(0x2001, 0xdb8, 0, 0, 0x1, 0x1, 0x1, 0x1))

      test(m"Create and show a subnet"):
        (ip"255.123.143.0".subnet(12)).show
      .assert(_ == t"255.112.0.0/12")

      test(m"Parse an IPv6 containing capital letters"):
        Ipv6.parse(t"2001:DB8::1:1:1:1:1")
      .assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))

      test(m"Invalid IP address is compile error"):
        demilitarize(ip"192.168.0.0.0.1").map(_.message)
      .assert(_ == List(t"nettlesome: the IP address is not valid because the address contains 6 period-separated groups instead of 4"))

      test(m"IP address byte out of range"):
        capture(Ipv4.parse(t"100.300.200.0"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv4ByteOutOfRange(300)))

      test(m"IPv4 address wrong number of bytes"):
        capture(Ipv4.parse(t"10.3.20.0.8"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv4WrongNumberOfGroups(5)))

      test(m"IPv6 address non-hex value"):
        capture(Ipv6.parse(t"::8:abcg:abc:1234"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6GroupNotHex(t"abcg")))

      test(m"IPv6 address too many groups"):
        capture(Ipv6.parse(t"1:2:3:4::5:6:7:8"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6TooManyNonzeroGroups(8)))

      test(m"IPv6 address wrong number of groups"):
        capture(Ipv6.parse(t"1:2:3:4:5:6:7:8:9"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6WrongNumberOfGroups(9)))

      test(m"IPv6 address wrong number of groups"):
        capture(Ipv6.parse(t"1:2:3:4:5:6:7:8:9"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6WrongNumberOfGroups(9)))

      test(m"IPv6 duplicate double-colon"):
        capture(Ipv6.parse(t"1::3:7::9"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6MultipleDoubleColons))

      test(m"IPv6 address wrong-length group"):
        capture(Ipv6.parse(t"::8:abcde:abc:1234"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6GroupWrongLength(t"abcde")))

    suite(m"Email address tests"):
      import EmailAddressError.Reason.*

      test(m"simple@example.com"):
        EmailAddress.parse(t"simple@example.com")
      .assert()

      test(m"very.common@example.com"):
        EmailAddress.parse(t"very.common@example.com")
      .assert()

      test(m"x@example.com"):
        EmailAddress.parse(t"x@example.com")
      .assert()

      test(m"long.email-address-with-hyphens@and.subdomains.example.com"):
        EmailAddress.parse(t"long.email-address-with-hyphens@and.subdomains.example.com")
      .assert()

      test(m"user.name+tag+sorting@example.com"):
        EmailAddress.parse(t"user.name+tag+sorting@example.com")
      .assert()

      test(m"name/surname@example.com"):
        EmailAddress.parse(t"name/surname@example.com")
      .assert()

      test(m"admin@example"):
        EmailAddress.parse(t"admin@example")
      .assert()

      test(m"example@s.example"):
        EmailAddress.parse(t"example@s.example")
      .assert()

      test(m"\" \"@example.org"):
        EmailAddress.parse(t"\" \"@example.org")
      .assert()

      test(m"\"john..doe\"@example.org"):
        EmailAddress.parse(t"\"john..doe\"@example.org")
      .assert()

      test(m"mailhost!username@example.org"):
        EmailAddress.parse(t"mailhost!username@example.org")
      .assert()

      test(m"\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"):
        EmailAddress.parse(t"\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com")
      .assert()

      test(m"user%example.com@example.org"):
        EmailAddress.parse(t"user%example.com@example.org")
      .assert()

      test(m"user-@example.org"):
        EmailAddress.parse(t"user-@example.org")
      .assert()

      test(m"postmaster@[123.123.123.123]"):
        EmailAddress.parse(t"postmaster@[123.123.123.123]")
      .assert()

      test(m"postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"):
        EmailAddress.parse(t"postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]")
      .assert()

      test(m"Empty email address"):
        capture(EmailAddress.parse(t""))
      .assert(_ == EmailAddressError(Empty))

      test(m"abc.example.com"):
        capture(EmailAddress.parse(t"abc.example.com"))
      .assert(_ == EmailAddressError(MissingAtSymbol))

      test(m"a@b@c@example.com"):
        capture(EmailAddress.parse(t"a@b@c@example.com"))
      .assert(_ == EmailAddressError(InvalidDomain(HostnameError(t"b@c@example.com", HostnameError.Reason.InvalidChar('@')))))

      test(m"a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com"):
        capture(EmailAddress.parse(t"a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com"))
      .assert(_ == EmailAddressError(InvalidChar('\\')))

      test(m"just\"not\"right@example.com"):
        capture(EmailAddress.parse(t"just\"not\"right@example.com"))
      .assert(_ == EmailAddressError(InvalidChar('\"')))

      test(m"this is\\\"not\\allowed@example.com"):
        capture(EmailAddress.parse(t"this is\\\"not\\allowed@example.com"))
      .assert(_ == EmailAddressError(InvalidChar(' ')))

      test(m"this\\ still\\\"not\\\\allowed@example.com"):
        capture(EmailAddress.parse(t"this\\ still\\\"not\\\\allowed@example.com"))
      .assert(_ == EmailAddressError(InvalidChar('\\')))

      test(m"1234567890123456789012345678901234567890123456789012345678901234+x@example.com"):
        capture(EmailAddress.parse(t"1234567890123456789012345678901234567890123456789012345678901234+x@example.com"))
      .assert(_ == EmailAddressError(LongLocalPart))

      test(m"user@[not-an-ip]"):
        capture(EmailAddress.parse(t"user@[not-an-ip]"))
      .assert(_ == EmailAddressError(InvalidDomain(IpAddressError(IpAddressError.Reason.Ipv4WrongNumberOfGroups(1)))))

      test(m"i.like.underscores@but_they_are_not_allowed_in_this_part"):
        capture(EmailAddress.parse(t"i.like.underscores@but_they_are_not_allowed_in_this_part"))
      .assert(_ == EmailAddressError(InvalidDomain(HostnameError(t"but_they_are_not_allowed_in_this_part", HostnameError.Reason.InvalidChar('_')))))

      test(m"I❤️CHOCOLATE🍫@example.com"):
        capture(EmailAddress.parse(t"I❤️CHOCOLATE🍫@example.com"))
      .matches:
        case EmailAddressError(InvalidChar(_)) =>

      test(m"Create an email address at compiletime"):
        email"test@example.com"
      .assert(_ == EmailAddress.parse(t"test@example.com"))

      test(m"Create an IPv4 email address at compiletime"):
        email"test@[192.168.0.1]"
      .assert(_ == EmailAddress.parse(t"test@[192.168.0.1]"))

      test(m"Create an IPv6 email address at compiletime"):
        email"test@[IPv6:1234::6789]"
      .assert(_ == EmailAddress.parse(t"test@[IPv6:1234::6789]"))

      test(m"Create a quoted email address at compiletime"):
        email""""test user"@example.com"""
      .assert(_ == EmailAddress.parse(t""""test user"@example.com"""))

      test(m"forbidden.@example.com"):
        capture(EmailAddress.parse(t"forbidden.@example.com"))
      .assert(_ == EmailAddressError(TerminalPeriod))

      test(m".forbidden@example.com"):
        capture(EmailAddress.parse(t".forbidden@example.com"))
      .assert(_ == EmailAddressError(InitialPeriod))

      test(m"not..allowed@example.com"):
        capture(EmailAddress.parse(t"not..allowed@example.com"))
      .assert(_ == EmailAddressError(SuccessivePeriods))

      test(m""""unescaped quote " is forbidden"@example.com"""):
        capture(EmailAddress.parse(t""""unescaped quote " is forbidden"@example.com"""))
      .assert(_ == EmailAddressError(UnescapedQuote))

      test(m""""unclosed.quote@example.com"""):
        capture(EmailAddress.parse(t""""unclosed.quote@example.com"""))
      .assert(_ == EmailAddressError(UnclosedQuote))

      test(m"""missing.domain@"""):
        capture(EmailAddress.parse(t"""missing.domain@"""))
      .assert(_ == EmailAddressError(MissingDomain))

      test(m"""unclosed IP address domain"""):
        capture(EmailAddress.parse(t"""user@[192.168.0.1"""))
      .assert(_ == EmailAddressError(UnclosedIpAddress))

    suite(m"URL tests"):
      test(m"parse Authority with username and password"):
        Authority.parse(t"username:password@example.com")
      .assert(_ == Authority(example.com, t"username:password"))

      test(m"parse Authority with username but not password"):
        Authority.parse(t"username@example.com")
      .assert(_ == Authority(example.com, t"username"))

      test(m"parse Authority with username, password and port"):
        Authority.parse(t"username:password@example.com:8080")
      .assert(_ == Authority(example.com, t"username:password", 8080))

      test(m"parse Authority with username and port"):
        Authority.parse(t"username@example.com:8080")
      .assert(_ == Authority(example.com, t"username", 8080))

      test(m"parse Authority with username, numerical password and port"):
        Authority.parse(t"username:1234@example.com:8080")
      .assert(_ == Authority(example.com, t"username:1234", 8080))

      test(m"Authority with invalid port fails"):
        capture(Authority.parse(t"username@example.com:no"))
      .matches:
        case UrlError(_, position, UrlError.Reason.Expected(UrlError.Expectation.Number)) if position == 21.z =>

      test(m"Parse full URL"):
        t"http://user:pw@example.com:8080/path/to/location?query=1#ref".decode[HttpUrl]
      .assert(_ == Url(Origin(Scheme.Http, Authority(example.com, t"user:pw", 8080)),
          t"/path/to/location", t"query=1", t"ref"))

      test(m"Parse simple URL"):
        t"https://example.com/foo".decode[HttpUrl]
      .assert(_ == Url(Origin(Scheme.Https, Authority(example.com)), t"/foo"))

      test(m"Parse url with fragment"):
        t"https://example.com/#id".decode[HttpUrl]
      .assert(_ == Url(Origin(Scheme.Https, Authority(example.com)), t"/", Unset, t"id"))

      test(m"Show simple URL"):
        t"http://example.com/foo".decode[HttpUrl].show
      .assert(_ == t"http://example.com/foo")

      test(m"show url with fragment"):
        t"https://example.com/#id".decode[HttpUrl].show
      .assert(_ == t"https://example.com/#id")

      test(m"Parse full URL at compiletime"):
        url"http://user:pw@example.com:8080/path/to/location?query=1#ref"
      .assert(_ == Url(Origin(Scheme.Http, Authority(example.com, t"user:pw", 8080)),
          t"/path/to/location", t"query=1", t"ref"))

      test(m"Parse FTP URL at compiletime"):
        url"ftp://user:pw@example.com:8080/path/to/location"
      .assert(_ == Url(Origin(Scheme(t"ftp"), Authority(example.com, t"user:pw", 8080)),
          t"/path/to/location"))

      test(m"Parse URL at compiletime with substitution"):
        val port = 1234
        url"http://user:pw@example.com:$port/path/to/location"
      .assert(_ == Url(Origin(Scheme(t"http"), Authority(example.com, t"user:pw", 1234)),
          t"/path/to/location"))

      test(m"Parse URL at compiletime with escaped substitution"):
        val message: Text = t"Hello world!"
        url"http://user:pw@example.com/$message"
      .assert(_ == Url(Origin(Scheme(t"http"), Authority(example.com, t"user:pw")), t"/Hello+world%21"))



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
        Hostname.parse(t"www.example.com")
      .assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"example"), DnsLabel(t"com")))

      test(m"A hostname cannot end in a period"):
        capture[HostnameError](Hostname.parse(t"www.example."))
      .assert(_ == HostnameError(t"www.example.", HostnameError.Reason.EmptyDnsLabel(2)))

      test(m"A hostname cannot start with a period"):
        capture[HostnameError](Hostname.parse(t".example.com"))
      .assert(_ == HostnameError(t".example.com", HostnameError.Reason.EmptyDnsLabel(0)))

      test(m"A hostname cannot have adjacent periods"):
        capture[HostnameError](Hostname.parse(t"www..com"))
      .assert(_ == HostnameError(t"www..com", HostnameError.Reason.EmptyDnsLabel(1)))

      test(m"A hostname cannot contain symbols"):
        capture[HostnameError](Hostname.parse(t"www.maybe?.com"))
      .assert(_ == HostnameError(t"www.maybe?.com", HostnameError.Reason.InvalidChar('?')))

      test(m"A DNS Label cannot begin with a dash"):
        capture[HostnameError](Hostname.parse(t"www.-maybe.com"))
      .assert(_ == HostnameError(t"www.-maybe.com", HostnameError.Reason.InitialDash(t"-maybe")))

      test(m"A hostname can contain two consecutive dashes"):
        Hostname.parse(t"www.exam--ple.com")
      .assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"exam--ple"), DnsLabel(t"com")))

      test(m"A DNS label cannot be longer than 63 characters"):
        capture[HostnameError](Hostname.parse(t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij.com"))
      .assert(_ == HostnameError(
        t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij.com",
        HostnameError.Reason.LongDnsLabel(t"abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij")
      ))

      test(m"A DNS label may be 63 characters long"):
        Hostname.parse(t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi.com")
      .assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi"), DnsLabel(t"com")))

      test(m"A DNS label may be 253 characters long"):
        Hostname.parse(t"www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.com")
      .assert()

      test(m"A DNS label may not be longer than 253 characters"):
        capture[HostnameError](Hostname.parse(t"www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy.com"))
      .assert(_.reason == HostnameError.Reason.LongHostname)

      test(m"Parse hostname at compiletime"):
        host"www.altavista.com"
      .assert()

      test(m"Parse bad hostname at compiletime"):
        demilitarize(host"www..com").map(_.message)
      .assert(_ == List(t"nettlesome: the hostname is not valid because a DNS label cannot be empty"))

    suite(m"MAC Address tests"):
      import MacAddressError.Reason.*

      test(m"Test simple MAC address"):
        MacAddress.parse(t"01-23-45-ab-cd-ef")
      .assert(_ == MacAddress(1251004370415L))

      test(m"Check MAC address with too few groups"):
        capture[MacAddressError](MacAddress.parse(t"01-23-ab-cd-ef"))
      .assert(_ == MacAddressError(WrongGroupCount(5)))

      test(m"Check MAC address with too few groups"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-67-ab-cd-ef"))
      .assert(_ == MacAddressError(WrongGroupCount(7)))

      test(m"Check MAC address with short group"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-6-ab-cd"))
      .assert(_ == MacAddressError(WrongGroupLength(3, 1)))

      test(m"Check MAC address with long group"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-67-ab-cde"))
      .assert(_ == MacAddressError(WrongGroupLength(5, 3)))

      test(m"Check MAC address with empty group"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45--ab-cd"))
      .assert(_ == MacAddressError(WrongGroupLength(3, 0)))

      test(m"Check MAC address with non-hex character"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-6g-ab-cd"))
      .assert(_ == MacAddressError(NotHex(3, t"6g")))

      test(m"Show a MAC address"):
        MacAddress.parse(t"01-23-45-ab-cd-ef").show
      .assert(_ == t"01-23-45-ab-cd-ef")

      test(m"Create a MAC address statically (and show it)"):
        mac"01-23-45-ab-cd-ef".show
      .assert(_ == t"01-23-45-ab-cd-ef")

      test(m"Check that a bad MAC address fails at compiletime"):
        demilitarize:
          mac"01-23-45-ab-cd-e"
        .map(_.message)
      .assert(_ == List(t"nettlesome: the MAC address is not valid because group 5 should be two hex digits, but its length is 1"))

      test(m"Create a MAC address from bytes"):
        MacAddress(1, 2, 3, 4, 5, 6).show
      .assert(_ == t"01-02-03-04-05-06")

    suite(m"Named port services"):
      test(m"Check SMTP over TCP port"):
        tcp"smtp"
      . assert(_ == TcpPort(25))

      test(m"Check Docker over TCP port"):
        tcp"docker"
      . assert(_ == UdpPort(2375))

      test(m"Check Docker over UDP port is not valid"):
        demilitarize(udp"docker").map(_.message)
      . assert(_ == List(t"nettlesome: docker is not a valid UDP port"))

      test(m"Check Nonexistent TCP port does not compile"):
        demilitarize(tcp"abcdef").map(_.message)
      . assert(_ == List(t"nettlesome: abcdef is not a valid TCP port"))

object example:
  val com = Hostname(DnsLabel(t"example"), DnsLabel(t"com"))
