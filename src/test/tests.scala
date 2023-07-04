package nettlesome

import probably.*
import rudiments.*
import gossamer.*
import spectacular.*

import unsafeExceptions.canThrowAny

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
      
      test(t"Parse an IPv6 containing capital letters"):
        Ipv6.parse(t"2001:DB8::1:1:1:1:1")
      .assert(_ == Ipv6(0x2001, 0xdb8, 0, 0x1, 0x1, 0x1, 0x1, 0x1))