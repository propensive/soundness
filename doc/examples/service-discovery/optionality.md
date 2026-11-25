# A key which a TXT record may not have

An instance describes itself in a TXT record of keys and values, and a record need not have
the key that is asked for. Looking one up gives an `Optional[Text]`, which is not `Text` until
the absence is dealt with.

```scala fuel
compile
frame
  label A version which may be absent
  errors show
  code
      import strategies.throwUnsafely

      val txt = Discovery.Txt(t"fingerprint" -> t"ab12", t"version" -> t"0.71")

      val version: Text = txt(t"version")
frame
  label With a value for its absence
  highlight or(t"unknown")
  code
      import strategies.throwUnsafely

      val txt = Discovery.Txt(t"fingerprint" -> t"ab12", t"version" -> t"0.71")

      val version: Text = txt(t"version").or(t"unknown")
      val license: Text = txt(t"license").or(t"unknown")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(version)
Out.println(license)
```

[Services and instances](../../soundness/modules/service-discovery.md#services-and-instances)
describes the record, and [optionality](../../philosophy/optionality.md) the type.
