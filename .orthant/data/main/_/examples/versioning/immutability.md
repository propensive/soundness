# A manifest with one attribute more

A `Manifest` is a value. Adding an attribute with `+` gives another manifest, and the one it
was added to is unchanged, so a base manifest can be shared by every JAR which is built from
it.

```scala fuel
compile
frame
  label A manifest for every JAR
  code
      import manifestAttributes.*

      val base = Manifest(CreatedBy(t"Soundness"))
frame
  label Another, for the one which runs
  highlight +  a new manifest
  code
      import manifestAttributes.*

      val base = Manifest(CreatedBy(t"Soundness"))
      val runnable = base + MainClass(fqcn"globe.Prompter")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import dysasymptotics.linearSize

Out.println(base.entries.size.show)
Out.println(runnable.entries.size.show)
```

The base manifest still has one attribute, and the new one has two.
[Manifests](../../soundness/modules/versioning.md#manifests) has the attributes, and
[immutability](../../philosophy/immutability.md) the reasons.
