# A dependency's public home, or none

The repackager finds where a dependency can be downloaded by its content hash. A resolver is a
function from a hash to an optional URL, so two sources are combined with `or`: the hints
which were named first, then the global index, and a hash which neither knows is `Unset`.

```scala fuel
compile
frame
  label The hints which were named
  code
      def resolver(hints: Map[Text, HttpUrl]): Repackager.Resolver =
        hash => hints.at(hash)
frame
  label Then the index of Maven artifacts
  highlight or(DepsDev.mavenUrl(hash))  asked only where the hints have nothing
  code
      def resolver(hints: Map[Text, HttpUrl]): Repackager.Resolver =
        hash => hints.at(hash).or(DepsDev.mavenUrl(hash))
##
```

The example is compiled, and is not run, since the lookup needs the network. A dependency with
no home is inlined into the JAR, as [repackaging](../../soundness/modules/packaging.md#repackaging)
describes, and [optionality](../../philosophy/optionality.md) has the operations used here.
