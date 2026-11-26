# A lookup which cannot miss

An `LruCache` may have evicted any key, so a lookup which only returned what is held would
fail for some of them. There is no such lookup. A read names the key and the computation which
makes its value, and the computation runs only when the key is absent.

```scala fuel
compile
frame
  label A cache of four lines
  code
      var recitals: Int = 0

      def recite(line: Int): Text =
        recitals += 1
        t"line $line"

      val prompter = LruCache[Int, Text](4)
frame
  label A read, with the way to fill it
  highlight (recite(1))  passed by name
  code
      var recitals: Int = 0

      def recite(line: Int): Text =
        recitals += 1
        t"line $line"

      val prompter = LruCache[Int, Text](4)
      val first: Text = prompter(1)(recite(1))
frame
  label The same read again
  highlight again  held already, so nothing is recited
  code
      var recitals: Int = 0

      def recite(line: Int): Text =
        recitals += 1
        t"line $line"

      val prompter = LruCache[Int, Text](4)
      val first: Text = prompter(1)(recite(1))
      val again: Text = prompter(1)(recite(1))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(again)
Out.println(recitals.show)
```

Both reads give a `Text`, and the line was recited once.
[An LRU cache](../../soundness/modules/caching.md#an-lru-cache) shows eviction, and
[total transitions](../../philosophy/total-transitions.md) the principle.
