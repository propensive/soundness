# What was found, and what was wanted

A `Json.Error` carries its reason as a value. For a value of the wrong type, the reason holds
the kind which was found and the kind which was wanted, and a handler reads them from a
pattern, with no message to parse.

```scala fuel
compile
frame
  label Any failure, handled alike
  code
      def age(json: Json): Text =
        recover:
          case error: Json.Error => t"unreadable"
        . protect(json.as[Int].show)
frame
  label The two kinds, taken from the reason
  highlight NotType(found..wanted)  both are values of Json.Primitive
  code
      import Json.Error.Reason.NotType

      def age(json: Json): Text =
        recover:
          case Json.Error(NotType(found, wanted)) => t"${found.show}, not ${wanted.show}"
          case error: Json.Error                  => t"unreadable"
        . protect(json.as[Int].show)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(age(j"30"))
Out.println(age(j""""thirty""""))
```

[Errors](../../soundness/modules/json.md#errors) lists the reasons, and
[expressive errors](../../philosophy/expressive-errors.md) says why they are values.
