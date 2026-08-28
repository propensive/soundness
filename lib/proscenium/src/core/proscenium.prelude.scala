                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package proscenium

// The blessed default namespace. With `scala` dropped from `-Yimports`
// (`-Yimports:java.lang,proscenium`), only what is re-exported here — plus `java.lang` and
// proscenium's own definitions — is visible without an import. In particular, the bare
// `scala.collection` aliases the `scala` package used to provide (`Seq`, `Vector`,
// `IndexedSeq`, `Range`, …) are deliberately NOT re-exported: the opaque prelude collections
// (`List`, `Set`, `Map`, `Sequence`) are the defaults, and stdlib collections at interop seams
// must announce themselves with an explicit `import scala.collection...`.
//
// `scala.caps`, `scala.math`, `scala.annotation`, `scala.util`, `scala.sys` and
// `scala.compiletime` are *packages*, which cannot be re-exported; files using them bare
// carry `import scala.{caps, ...}` instead.

// Fundamental types. `Unit` cannot be `export`ed (its companion object is forbidden in
// source), so it gets a manual type alias.
export scala.{Any, AnyKind, AnyRef, AnyVal, Matchable, Nothing, Null, Singleton}
type Unit = scala.Unit

// The infix intersection/union type aliases and the subtype/equality witnesses.
export scala.{`&`, `|`, `<:<`, `=:=`}

// Primitives and arrays.
export scala.{Boolean, Byte, Char, Double, Float, Int, Long, Short}
// `Array` is the blessed opaque alias (proscenium.Array, the separation-checked mutable
// array), a package member here, so ambient without an alias; its frozen form is
// `Array[element]^{}`. The raw stdlib forms stay reachable as `scala.Array`/`scala.IArray`
// for the JDK-boundary and erasure-level code that needs them.

// Core data types and their extractors.
export scala.{Either, Left, None, Option, Right, Some}
export scala.{EmptyTuple, NonEmptyTuple, Tuple, Tuple1, Tuple2, *:}

// Abstractions.
export scala.{CanEqual, Conversion, Dynamic, Function, PartialFunction, Product, Selectable,
    StringContext}

// Numeric abstractions (aliases in the `scala` package object).
export scala.{BigDecimal, BigInt, Fractional, Integral, Numeric, Ordered, Ordering}

// Annotations defined directly in the `scala` package.
export scala.{deprecated, unchecked, volatile, transient, native, main}

// `scala.unsafeExceptions` is an object, so `import unsafeExceptions.canThrowAny` keeps working.

export scala.unsafeExceptions

// Miscellaneous `scala`-package types with existing instances or uses.
//
// `Precise` is deliberately NOT among them (#1811). Exporting it yields a distinct alias
// symbol which the compiler's union-widening suppression does not recognise, so a context
// bound written `[t: Precise]` against the alias compiles and then does nothing: inference
// widens the union anyway, with no warning. Since every module compiles with
// `-Yimports:proscenium`, re-exporting it here would make that silent failure the default.
// Write `scala.Precise` explicitly, which works; naming the bare `Precise` now fails to
// compile rather than degrading quietly.
export scala.{CanThrow, DummyImplicit, MatchError, NamedTuple, Specializable, ValueOf}

// Exception aliases the `scala` package object provides for `java.util` types.
export scala.NoSuchElementException

// The two blessed stdlib collection *interfaces*: abstract, and the internal currency of the
// `Traversable`/`Reshapable` machinery. Concrete stdlib collections are not re-exported.
export scala.{Iterable, Iterator}

// `Chain` (and its lazy cons `#::`) is now the opaque alias defined in
// `proscenium.Chain.scala`, so it is NOT re-exported from `scala` here.
// TEMPORARY: `StringBuilder` is the mutable text-accumulation currency, pending its own review.
export scala.StringBuilder

