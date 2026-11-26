# One document, or a stream of them

A YAML source may hold several documents, divided by `---`. There is no second method for
reading them: `read` takes the type which is wanted, and a list of documents is a type.

```scala fuel
compile
frame
  label One document
  highlight read[Yaml]  a single document
  code
      import strategies.throwUnsafely

      val act: Yaml = t"act: 1".read[Yaml]
frame
  label Every document of a stream
  highlight read[List[Yaml]]  each document between the markers
  code
      import strategies.throwUnsafely

      val acts: List[Yaml] = t"---\nact: 1\n---\nact: 2\n---\nact: 3".read[List[Yaml]]
      val numbers: List[Int] = acts.map(_(t"act").as[Int])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(numbers.inspect)
```

[Parsing and decoding](../../soundness/modules/yaml.md#parsing-and-decoding) says how a large
stream is read without being held whole.
