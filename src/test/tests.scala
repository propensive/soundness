/*
    Nettlesome, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import spectacular.*
import perforate.*
import larceny.*

import errorHandlers.throwUnsafely

object Tests extends Suite(t"Nettlesome tests"):
  def run(): Unit =
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
      .assert(_ == List(t"nettlesome: the IP address is not valid because the address contains 6 numbers instead of 4"))
    
      test(t"IP address byte out of range"):
        capture(Ipv4.parse(t"100.300.200.0"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv4ByteOutOfRange(300)))
      
      test(t"IPv4 address wrong number of bytes"):
        capture(Ipv4.parse(t"10.3.20.0.8"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv4WrongNumberOfBytes(5)))
      
      test(t"IPv6 address non-hex value"):
        capture(Ipv6.parse(t"::8:abcg:abc:1234"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv6GroupNotHex(t"abcg")))
      
      test(t"IPv6 address too many groups"):
        capture(Ipv6.parse(t"1:2:3:4::5:6:7:8"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv6TooManyNonzeroGroups(8)))
      
      test(t"IPv6 address wrong number of groups"):
        capture(Ipv6.parse(t"1:2:3:4:5:6:7:8:9"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv6WrongNumberOfGroups(9)))
      
      test(t"IPv6 address wrong number of groups"):
        capture(Ipv6.parse(t"1:2:3:4:5:6:7:8:9"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv6WrongNumberOfGroups(9)))
      
      test(t"IPv6 duplicate double-colon"):
        capture(Ipv6.parse(t"1::3:7::9"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv6MultipleDoubleColons))
      
      test(t"IPv6 address wrong-length group"):
        capture(Ipv6.parse(t"::8:abcde:abc:1234"))
      .assert(_ == IpAddressError(IpAddressError.Issue.Ipv6GroupWrongLength(t"abcde")))
      
