# Addresses in a JSON document

A hostname, an IP address and a MAC address are decoded from text, and that is all JSON needs
to know. A case class with fields of those types is read from a document with no codec written
for it, and each field is validated as it is read.

```scala fuel
compile
frame
  label Fields of text
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Stage(host: Text, address: Text, card: Text)

      val json = t"""{"host": "globe.example.com", "address": "10.0.16.4",
                      "card": "01-23-45-ab-cd-ef"}"""

      val stage = json.read[Json].as[Stage]
frame
  label Fields which are validated
  highlight Hostname..MacAddress
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Stage(host: Hostname, address: Ipv4, card: MacAddress)

      val json = t"""{"host": "globe.example.com", "address": "10.0.16.4",
                      "card": "01-23-45-ab-cd-ef"}"""

      val stage = json.read[Json].as[Stage]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(stage.inspect)
```

[Parsing at runtime](../../soundness/modules/network-addresses.md#parsing-at-runtime) says how
each type is decoded, and [JSON](../../soundness/modules/json.md) how a case class is.
