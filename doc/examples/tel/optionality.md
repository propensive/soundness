# Two spellings of absence

A TEL document may leave a keyword out, or write it with nothing after it. A field of type
`Optional` reads both as `Unset`, so a keyword left in place and emptied means what a keyword
removed does.

```scala fuel
compile
frame
  label A keyword with a value
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, patron: Optional[Text])

      val full: Player = t"name Feste\npatron Olivia\n".read[Player in Tel]
frame
  label The keyword left out
  highlight t"name..\n"  no such keyword
    line 6
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, patron: Optional[Text])

      val full: Player = t"name Feste\npatron Olivia\n".read[Player in Tel]
      val omitted: Player = t"name Feste\n".read[Player in Tel]
frame
  label The keyword with nothing after it
  highlight patron\n  present, and empty
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, patron: Optional[Text])

      val full: Player = t"name Feste\npatron Olivia\n".read[Player in Tel]
      val omitted: Player = t"name Feste\n".read[Player in Tel]
      val emptied: Player = t"name Feste\npatron\n".read[Player in Tel]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(full.inspect)
Out.println(omitted.inspect)
Out.println(emptied.inspect)
```

[Optional fields](../../soundness/modules/tel.md#optional-fields) has the policy which makes
absence an error.
