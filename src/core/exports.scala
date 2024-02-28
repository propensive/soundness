/*
    Rudiments, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import language.experimental.captureChecking

export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}
export scala.collection.concurrent.TrieMap

export Predef.{nn, identity, summon, charWrapper, $conforms, ArrowAssoc, intWrapper, longWrapper, shortWrapper,
    byteWrapper, valueOf, doubleWrapper, floatWrapper, classOf, locally}

export scala.util.control.NonFatal

export scala.util.boundary, boundary.break

export scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala, MapHasAsScala, SeqHasAsJava,
    MapHasAsJava, EnumerationHasAsScala}

export scala.annotation.{tailrec, implicitNotFound as missingContext, targetName, switch, StaticAnnotation,
    capability}

export scala.annotation.unchecked.{uncheckedVariance, uncheckedCaptures, uncheckedStable}

@targetName("erasedValue")
erased def ###[ErasedType] : ErasedType = scala.compiletime.erasedValue
