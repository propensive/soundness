package soundness

export scala.collection.immutable.Vector as Trie
export Predef.runtimeChecked as absolve
export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}
export scala.collection.concurrent.TrieMap

export Predef
. { nn, identity, summon, charWrapper, $conforms, ArrowAssoc, intWrapper, longWrapper,
    shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper, classOf, locally }

export scala.util.control.NonFatal

export scala.util.boundary, boundary.break

export scala.jdk.CollectionConverters
. { IteratorHasAsScala, ListHasAsScala, MapHasAsScala, SeqHasAsJava, MapHasAsJava,
    EnumerationHasAsScala }

export caps.Cap as Capability

export scala.annotation
. { tailrec, implicitNotFound as missingContext, targetName, switch, StaticAnnotation }

export scala.annotation.unchecked.{uncheckedVariance, uncheckedCaptures, uncheckedStable}

export scala.LazyList as Stream
export scala.DummyImplicit as Void

