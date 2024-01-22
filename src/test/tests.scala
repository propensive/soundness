/*
    Nettlesome, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import probably.*
import rudiments.*
import gossamer.*
import anticipation.*
import serpentine.*, hierarchies.urls
import spectacular.*
import perforate.*
import larceny.*

import errorHandlers.throwUnsafely

object Tests extends Suite(t"Nettlesome tests"):
  def run(): Unit =
    suite(t"Internet tests"):
      def remoteCall()(using Internet): Unit = ()

      test(t"Check remote call is callable with `Internet`"):
        internet(true):
          remoteCall()
      .assert()
    
      test(t"Check remote call is not callable without `Internet`"):
        demilitarize:
          remoteCall()
        .map(_.errorId)
      .assert(_ == List(ErrorId.MissingImplicitArgumentID))


    suite(t"IPv4 tests"):
      test(t"Parse in IPv4 address"):
        Ipv4.parse(t"1.2.3.4")
      .assert(_ == Ipv4(1, 2, 3, 4))
      
      test(t"Show an Ipv4 address"):
        Ipv4(127, 244, 197, 0).show
      .assert(_ == t"127.244.197.0")
      
      test(t"Show a zero Ipv4 address"):
        Ipv4(0, 0, 0, 0).show
      .assert(_ == t"0.0.0.0")
      
      test(t"Show a 'maximum' Ipv4 address"):
        Ipv4(255, 255, 255, 255).show
      .assert(_ == t"255.255.255.255")

      test(t"Get an IP address as an integer"):
        Ipv4(192, 168, 0, 1).int
      .assert(_ == bin"11000000 10101000 00000000 00000001")
    
    suite(t"IPv6 tests"):
      test(t"Parse an IPv6 address"):
        Ipv6.parse(t"2001:db8:0000:1:1:1:1:1")
      .assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))
      
      test(t"Render an IPv6 address"):
        Ipv6.parse(t"2001:db8:0000:1:1:1:1:1").show
      .assert(_ == t"2001:db8:0:1:1:1:1:1")

      test(t"Parse zero IPv6 address"):       
        Ipv6.parse(t"::")
      .assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 0))
      
      test(t"Parse zero-leading IPv6 address"):       
        Ipv6.parse(t"::2")
      .assert(_ == Ipv6(0, 0, 0, 0, 0, 0, 0, 2))
      
      test(t"Parse zeroes-trailing IPv6 address"):       
        Ipv6.parse(t"8::")
      .assert(_ == Ipv6(8, 0, 0, 0, 0, 0, 0, 0))
      
      test(t"Show zero IPv6 address"):       
        Ipv6(0, 0, 0, 0, 0, 0, 0, 0).show
      .assert(_ == t"::")
      
      test(t"Show zero-leading IPv6 address"):       
        Ipv6(0, 0, 0, 0, 0, 0, 0, 1).show
      .assert(_ == t"::1")
      
      test(t"Show zeroes-trailing IPv6 address"):       
        Ipv6(8, 0, 0, 0, 0, 0, 0, 0).show
      .assert(_ == t"8::")

      test(t"Parse IPv4 address at compiletime"):
        ip"122.0.0.1"
      .assert(_ == Ipv4(122, 0, 0, 1))
      
      test(t"Parse an IPv6 address at compiletime"):
        ip"2001:db8::1:1:1:1"
      .assert(_ == Ipv6(0x2001, 0xdb8, 0, 0, 0x1, 0x1, 0x1, 0x1))
      
      test(t"Create and show a subnet"):
        (ip"255.123.143.0"/12).show
      .assert(_ == t"255.112.0.0/12")
      
      test(t"Parse an IPv6 containing capital letters"):
        Ipv6.parse(t"2001:DB8::1:1:1:1:1")
      .assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))
    
      test(t"Invalid IP address is compile error"):
        demilitarize(ip"192.168.0.0.0.1").map(_.message)
      .assert(_ == List(t"nettlesome: the IP address is not valid because the address contains 6 period-separated groups instead of 4"))
    
      test(t"IP address byte out of range"):
        capture(Ipv4.parse(t"100.300.200.0"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv4ByteOutOfRange(300)))
      
      test(t"IPv4 address wrong number of bytes"):
        capture(Ipv4.parse(t"10.3.20.0.8"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv4WrongNumberOfGroups(5)))
      
      test(t"IPv6 address non-hex value"):
        capture(Ipv6.parse(t"::8:abcg:abc:1234"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6GroupNotHex(t"abcg")))
      
      test(t"IPv6 address too many groups"):
        capture(Ipv6.parse(t"1:2:3:4::5:6:7:8"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6TooManyNonzeroGroups(8)))
      
      test(t"IPv6 address wrong number of groups"):
        capture(Ipv6.parse(t"1:2:3:4:5:6:7:8:9"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6WrongNumberOfGroups(9)))
      
      test(t"IPv6 address wrong number of groups"):
        capture(Ipv6.parse(t"1:2:3:4:5:6:7:8:9"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6WrongNumberOfGroups(9)))
      
      test(t"IPv6 duplicate double-colon"):
        capture(Ipv6.parse(t"1::3:7::9"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6MultipleDoubleColons))
      
      test(t"IPv6 address wrong-length group"):
        capture(Ipv6.parse(t"::8:abcde:abc:1234"))
      .assert(_ == IpAddressError(IpAddressError.Reason.Ipv6GroupWrongLength(t"abcde")))

    suite(t"Email address tests"):
      import EmailAddressError.Reason.*

      test(t"simple@example.com"):
        EmailAddress.parse(t"simple@example.com")
      .assert()

      test(t"very.common@example.com"):
        EmailAddress.parse(t"very.common@example.com")
      .assert()
      
      test(t"x@example.com"):
        EmailAddress.parse(t"x@example.com")
      .assert()
     
      test(t"long.email-address-with-hyphens@and.subdomains.example.com"):
        EmailAddress.parse(t"long.email-address-with-hyphens@and.subdomains.example.com")
      .assert()
     
      test(t"user.name+tag+sorting@example.com"):
        EmailAddress.parse(t"user.name+tag+sorting@example.com")
      .assert()
     
      test(t"name/surname@example.com"):
        EmailAddress.parse(t"name/surname@example.com")
      .assert()
     
      test(t"admin@example"):
        EmailAddress.parse(t"admin@example")
      .assert()
     
      test(t"example@s.example"):
        EmailAddress.parse(t"example@s.example")
      .assert()
     
      test(t"\" \"@example.org"):
        EmailAddress.parse(t"\" \"@example.org")
      .assert()
     
      test(t"\"john..doe\"@example.org"):
        EmailAddress.parse(t"\"john..doe\"@example.org")
      .assert()
     
      test(t"mailhost!username@example.org"):
        EmailAddress.parse(t"mailhost!username@example.org")
      .assert()
     
      test(t"\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"):
        EmailAddress.parse(t"\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com")
      .assert()
     
      test(t"user%example.com@example.org"):
        EmailAddress.parse(t"user%example.com@example.org")
      .assert()
     
      test(t"user-@example.org"):
        EmailAddress.parse(t"user-@example.org")
      .assert()
     
      test(t"postmaster@[123.123.123.123]"):
        EmailAddress.parse(t"postmaster@[123.123.123.123]")
      .assert()
     
      test(t"postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"):
        EmailAddress.parse(t"postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]")
      .assert()

      test(t"Empty email address"):
        capture(EmailAddress.parse(t""))
      .assert(_ == EmailAddressError(Empty))
      
      test(t"abc.example.com"):
        capture(EmailAddress.parse(t"abc.example.com"))
      .assert(_ == EmailAddressError(MissingAtSymbol))
     
      test(t"a@b@c@example.com"):
        capture(EmailAddress.parse(t"a@b@c@example.com"))
      .assert(_ == EmailAddressError(InvalidDomain(HostnameError(HostnameError.Reason.InvalidChar('@')))))
     
      test(t"a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com"):
        capture(EmailAddress.parse(t"a\\\"b(c)d,e:f;g<h>i[j\\k]l@example.com"))
      .assert(_ == EmailAddressError(InvalidChar('\\')))
     
      test(t"just\"not\"right@example.com"):
        capture(EmailAddress.parse(t"just\"not\"right@example.com"))
      .assert(_ == EmailAddressError(InvalidChar('\"')))
     
      test(t"this is\\\"not\\allowed@example.com"):
        capture(EmailAddress.parse(t"this is\\\"not\\allowed@example.com"))
      .assert(_ == EmailAddressError(InvalidChar(' ')))
     
      test(t"this\\ still\\\"not\\\\allowed@example.com"):
        capture(EmailAddress.parse(t"this\\ still\\\"not\\\\allowed@example.com"))
      .assert(_ == EmailAddressError(InvalidChar('\\')))
     
      test(t"1234567890123456789012345678901234567890123456789012345678901234+x@example.com"):
        capture(EmailAddress.parse(t"1234567890123456789012345678901234567890123456789012345678901234+x@example.com"))
      .assert(_ == EmailAddressError(LongLocalPart))
     
      test(t"user@[not-an-ip]"):
        capture(EmailAddress.parse(t"user@[not-an-ip]"))
      .assert(_ == EmailAddressError(InvalidDomain(IpAddressError(IpAddressError.Reason.Ipv4WrongNumberOfGroups(1)))))
     
      test(t"i.like.underscores@but_they_are_not_allowed_in_this_part"):
        capture(EmailAddress.parse(t"i.like.underscores@but_they_are_not_allowed_in_this_part"))
      .assert(_ == EmailAddressError(InvalidDomain(HostnameError(HostnameError.Reason.InvalidChar('_')))))
      
      test(t"I❤️CHOCOLATE🍫@example.com"):
        capture(EmailAddress.parse(t"I❤️CHOCOLATE🍫@example.com"))
      .matches:
        case EmailAddressError(InvalidChar(_)) =>

      test(t"Create an email address at compiletime"):
        email"test@example.com"
      .assert(_ == EmailAddress.parse(t"test@example.com"))

      test(t"Create an IPv4 email address at compiletime"):
        email"test@[192.168.0.1]"
      .assert(_ == EmailAddress.parse(t"test@[192.168.0.1]"))

      test(t"Create an IPv6 email address at compiletime"):
        email"test@[IPv6:1234::6789]"
      .assert(_ == EmailAddress.parse(t"test@[IPv6:1234::6789]"))
      
      test(t"Create a quoted email address at compiletime"):
        email""""test user"@example.com"""
      .assert(_ == EmailAddress.parse(t""""test user"@example.com"""))
    
      test(t"forbidden.@example.com"):
        capture(EmailAddress.parse(t"forbidden.@example.com"))
      .assert(_ == EmailAddressError(TerminalPeriod))
      
      test(t".forbidden@example.com"):
        capture(EmailAddress.parse(t".forbidden@example.com"))
      .assert(_ == EmailAddressError(InitialPeriod))
      
      test(t"not..allowed@example.com"):
        capture(EmailAddress.parse(t"not..allowed@example.com"))
      .assert(_ == EmailAddressError(SuccessivePeriods))
      
      test(t""""unescaped quote " is forbidden"@example.com"""):
        capture(EmailAddress.parse(t""""unescaped quote " is forbidden"@example.com"""))
      .assert(_ == EmailAddressError(UnescapedQuote))
      
      test(t""""unclosed.quote@example.com"""):
        capture(EmailAddress.parse(t""""unclosed.quote@example.com"""))
      .assert(_ == EmailAddressError(UnclosedQuote))
      
      test(t"""missing.domain@"""):
        capture(EmailAddress.parse(t"""missing.domain@"""))
      .assert(_ == EmailAddressError(MissingDomain))
      
      test(t"""unclosed IP address domain"""):
        capture(EmailAddress.parse(t"""user@[192.168.0.1"""))
      .assert(_ == EmailAddressError(UnclosedIpAddress))
      
    suite(t"URL tests"):
      test(t"parse Authority with username and password"):
        Authority.parse(t"username:password@example.com")
      .assert(_ == Authority(example.com, t"username:password"))
      
      test(t"parse Authority with username but not password"):
        Authority.parse(t"username@example.com")
      .assert(_ == Authority(example.com, t"username"))
      
      test(t"parse Authority with username, password and port"):
        Authority.parse(t"username:password@example.com:8080")
      .assert(_ == Authority(example.com, t"username:password", 8080))
      
      test(t"parse Authority with username and port"):
        Authority.parse(t"username@example.com:8080")
      .assert(_ == Authority(example.com, t"username", 8080))
      
      test(t"parse Authority with username, numerical password and port"):
        Authority.parse(t"username:1234@example.com:8080")
      .assert(_ == Authority(example.com, t"username:1234", 8080))

      test(t"Authority with invalid port fails"):
        capture(Authority.parse(t"username@example.com:no"))
      .matches:
        case UrlError(_, 21, UrlError.Expectation.Number) =>
      
      test(t"Parse full URL"):
        Url.parse(t"http://user:pw@example.com:8080/path/to/location?query=1#ref")
      .assert(_ == Url(Scheme.Http, Authority(example.com, t"user:pw", 8080),
          t"/path/to/location", t"query=1", t"ref"))
      
      test(t"Parse simple URL"):
        Url.parse(t"https://example.com/foo")
      .assert(_ == Url(Scheme.Https, Authority(example.com), t"/foo"))
      
      test(t"Show simple URL"):
        Url.parse(t"http://example.com/foo").show
      .assert(_ == t"http://example.com/foo")
      
      test(t"Parse full URL at compiletime"):
        url"http://user:pw@example.com:8080/path/to/location?query=1#ref"
      .assert(_ == Url(Scheme.Http, Authority(example.com, t"user:pw", 8080),
          t"/path/to/location", t"query=1", t"ref"))
      
      test(t"Parse FTP URL at compiletime"):
        url"ftp://user:pw@example.com:8080/path/to/location"
      .assert(_ == Url(Scheme(t"ftp"), Authority(example.com, t"user:pw", 8080),
          t"/path/to/location"))
      
      test(t"Parse URL at compiletime with substitution"):
        val port = 1234
        url"http://user:pw@example.com:$port/path/to/location"
      .assert(_ == Url(Scheme(t"http"), Authority(example.com, t"user:pw", 1234),
          t"/path/to/location"))
      
      test(t"Parse URL at compiletime with escaped substitution"):
        val message: Text = t"Hello world!"
        url"http://user:pw@example.com/$message"
      .assert(_ == Url(Scheme(t"http"), Authority(example.com, t"user:pw"), t"/Hello+world%21"))
      
      test(t"Parse URL at compiletime with unescaped substitution"):
        val message = Raw(t"Hello world!")
        url"http://user:pw@example.com/$message"
      .assert(_ == Url(Scheme(t"http"), Authority(example.com, t"user:pw"), t"/Hello world!"))
      
      test(t"Relative path is unescaped"):
        val message: Text = t"Hello world!"
        url"http://user:pw@example.com/$message/foo".path
      .assert(_ == (? / p"Hello world!" / p"foo").descent)
      
      test(t"Relative path with raw substitution is unescaped"):
        val message: Raw = Raw(t"Hello+world%21")
        url"http://user:pw@example.com/$message/foo".path
      .assert(_ == (? / p"Hello world!" / p"foo").descent)
   
    suite(t"Hostname tests"):
      test(t"Parse a simple hostname"):
        Hostname.parse(t"www.example.com")
      .assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"example"), DnsLabel(t"com")))

      test(t"A hostname cannot end in a period"):
        capture[HostnameError](Hostname.parse(t"www.example."))
      .assert(_ == HostnameError(HostnameError.Reason.EmptyDnsLabel(2)))
      
      test(t"A hostname cannot start with a period"):
        capture[HostnameError](Hostname.parse(t".example.com"))
      .assert(_ == HostnameError(HostnameError.Reason.EmptyDnsLabel(0)))
      
      test(t"A hostname cannot have adjacent periods"):
        capture[HostnameError](Hostname.parse(t"www..com"))
      .assert(_ == HostnameError(HostnameError.Reason.EmptyDnsLabel(1)))

      test(t"A hostname cannot contain symbols"):
        capture[HostnameError](Hostname.parse(t"www.maybe?.com"))
      .assert(_ == HostnameError(HostnameError.Reason.InvalidChar('?')))
      
      test(t"A DNS Label cannot begin with a dash"):
        capture[HostnameError](Hostname.parse(t"www.-maybe.com"))
      .assert(_ == HostnameError(HostnameError.Reason.InitialDash(t"-maybe")))
      
      test(t"A hostname can contain two consecutive dashes"):
        Hostname.parse(t"www.exam--ple.com")
      .assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"exam--ple"), DnsLabel(t"com")))
      
      test(t"A DNS label cannot be longer than 63 characters"):
        capture[HostnameError](Hostname.parse(t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij.com"))
      .assert(_ == HostnameError(HostnameError.Reason.LongDnsLabel(t"abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghij")))
      
      test(t"A DNS label may be 63 characters long"):
        Hostname.parse(t"www.abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi.com")
      .assert(_ == Hostname(DnsLabel(t"www"), DnsLabel(t"abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz-abcdefghi"), DnsLabel(t"com")))
      
      test(t"A DNS label may be 253 characters long"):
        Hostname.parse(t"www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.com")
      .assert()
      
      test(t"A DNS label may not be longer than 253 characters"):
        capture[HostnameError](Hostname.parse(t"www.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy.com"))
      .assert(_ == HostnameError(HostnameError.Reason.LongHostname))

      test(t"Parse hostname at compiletime"):
        host"www.altavista.com"
      .assert()
      
      test(t"Parse bad hostname at compiletime"):
        demilitarize(host"www..com").map(_.message)
      .assert(_ == List(t"nettlesome: the hostname is not valid because a DNS label cannot be empty"))
    
    suite(t"MAC Address tests"):
      import MacAddressError.Reason.*

      test(t"Test simple MAC address"):
        MacAddress.parse(t"01-23-45-ab-cd-ef")
      .assert(_ == MacAddress(1251004370415L))

      test(t"Check MAC address with too few groups"):
        capture[MacAddressError](MacAddress.parse(t"01-23-ab-cd-ef"))
      .assert(_ == MacAddressError(WrongGroupCount(5)))
      
      test(t"Check MAC address with too few groups"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-67-ab-cd-ef"))
      .assert(_ == MacAddressError(WrongGroupCount(7)))
      
      test(t"Check MAC address with short group"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-6-ab-cd"))
      .assert(_ == MacAddressError(WrongGroupLength(3, 1)))
      
      test(t"Check MAC address with long group"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-67-ab-cde"))
      .assert(_ == MacAddressError(WrongGroupLength(5, 3)))
      
      test(t"Check MAC address with empty group"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45--ab-cd"))
      .assert(_ == MacAddressError(WrongGroupLength(3, 0)))
      
      test(t"Check MAC address with non-hex character"):
        capture[MacAddressError](MacAddress.parse(t"01-23-45-6g-ab-cd"))
      .assert(_ == MacAddressError(NotHex(3, t"6g")))

      test(t"Show a MAC address"):
        MacAddress.parse(t"01-23-45-ab-cd-ef").show
      .assert(_ == t"01-23-45-ab-cd-ef")

      test(t"Create a MAC address statically (and show it)"):
        mac"01-23-45-ab-cd-ef".show
      .assert(_ == t"01-23-45-ab-cd-ef")

      test(t"Check that a bad MAC address fails at compiletime"):
        demilitarize(mac"01-23-45-ab-cd-e").map(_.message)
      .assert(_ == List(t"nettlesome: the MAC address is not valid because group 5 should be two hex digits, but its length is 1"))

      test(t"Create a MAC address from bytes"):
        MacAddress(1, 2, 3, 4, 5, 6).show
      .assert(_ == t"01-02-03-04-05-06")

object example:
  val com = Hostname(DnsLabel(t"example"), DnsLabel(t"com"))
