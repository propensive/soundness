package rudiments

import language.experimental.captureChecking

export scala.util.chaining.scalaUtilChainingOps

export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}
export scala.collection.concurrent.TrieMap

export Predef.{nn, genericArrayOps, identity, summon, charWrapper, $conforms, ArrowAssoc,
    intWrapper, longWrapper, shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper,
    classOf, locally}

export scala.util.control.NonFatal

export scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala, MapHasAsScala, SeqHasAsJava,
    MapHasAsJava, EnumerationHasAsScala}

export scala.annotation.{tailrec, implicitNotFound as missingContext, targetName, switch, StaticAnnotation,
    capability, allowConversions as convertible}
