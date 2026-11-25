# A class name read from text may not be one

Text which arrives as the program runs is made an `Fqcn` by parsing it, and parsing can fail.
The signature of `Fqcn` says so with `raises Fqcn.Error`, and a method which calls it must say
the same, or handle the error.

```scala fuel
compile
frame
  label The failure is not accounted for
  errors show
  code
      def mainClass(name: Text): Fqcn = Fqcn(name)
frame
  label It is declared
  highlight raises..Error  the caller must deal with it
  code
      def mainClass(name: Text): Fqcn raises Fqcn.Error = Fqcn(name)
frame
  label Or handled, with the reason in hand
  highlight error.reason  one of four cases, each with its own wording
  code
      def mainClass(name: Text): Fqcn raises Fqcn.Error = Fqcn(name)

      def check(name: Text): Text =
        recover:
          case error: Fqcn.Error => error.reason.communicate.text
        . protect(mainClass(name).className)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(check(t"verona.Juliet"))
Out.println(check(t"verona.class.Juliet"))
Out.println(check(t"verona.2Gentlemen"))
```

[Class names](../../soundness/modules/stack-traces.md#class-names) describes the rules, and
[errors](../../soundness/modules/errors.md) what `raises` and `recover` mean.
