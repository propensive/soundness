# Which Base64 is an import

There are many Base64s, which write the same bytes in different characters. `serialize[Base64]`
names the base, and the alphabet comes from the scope. With none there, it does not compile.

```scala fuel
compile
frame
  label No alphabet in scope
  errors show
  code
      val token: Text = hex"fbefff".serialize[Base64]
frame
  label The standard alphabet
  highlight base64Standard  with + and /
  code
      import alphabets.base64Standard

      val token: Text = hex"fbefff".serialize[Base64]
frame
  label The alphabet which is safe in a URL
  highlight base64Url  with - and _
  code
      import alphabets.base64Url

      val token: Text = hex"fbefff".serialize[Base64]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(token)
```

The standard alphabet writes the same bytes as `++//`.
[Choosing an alphabet](../../soundness/modules/base-encoding.md#choosing-an-alphabet) lists
them all, and [declarative context](../../philosophy/declarative-context.md) says why the
alphabet is not a flag.
