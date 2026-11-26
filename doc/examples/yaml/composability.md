# A configuration file with typed fields

A field of a case class may be any type which can be read from text. An IP address and a
version number come from other libraries, which know nothing of YAML, and a configuration file
is decoded to them directly.

```scala fuel
compile
frame
  label Fields of text
  code
      import strategies.throwUnsafely

      case class Playhouse(name: Text, address: Text, version: Text)

      val globe = t"name: Globe\naddress: 10.0.16.3\nversion: 1.5.99".read[Playhouse in Yaml]
frame
  label Fields of their own types
  highlight Ipv4  from the network library
  highlight Semver  from the versioning library
  code
      import strategies.throwUnsafely

      case class Playhouse(name: Text, address: Ipv4, version: Semver)

      val globe = t"name: Globe\naddress: 10.0.16.3\nversion: 1.5.99".read[Playhouse in Yaml]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(globe.address.show)
Out.println(globe.version.major.show)
```

[Decoding](../../soundness/modules/decoding.md) says how a type comes to be readable from text,
and [YAML](../../soundness/modules/yaml.md#parsing-and-decoding) how a document is decoded.
