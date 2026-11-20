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
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package murmuration

import scala.quoted.*

// Rebuilds a map around a perfect hash of its keys, found and compiled while the program
// runs. `PerfectHash.search` looks for a program which separates the keys; the program is
// spliced into a `PerfectMap` subclass, as it would be by a macro, and that class is compiled
// by the staging compiler and loaded. A read of the result computes its key's slot with
// inline arithmetic and makes one comparison.
//
// Compiling a class takes the compiler a noticeable fraction of a second, and the first time
// several seconds, so this is for a map that is made once and read a great many times. It
// needs the compiler on the classpath, which is why it is a component of its own that nothing
// else depends upon.
object Optimizer:
  // What the keys are, which decides how a key becomes the word that is hashed: a text by
  // its hash code, which the language specifies and the JVM caches; a number or a character
  // by its value.
  private enum Kind:
    case Textual, Integral, Wide, Symbolic

  private def kind(key: Any): Kind | Null = key match
    case _: String              => Kind.Textual
    case _: java.lang.Integer   => Kind.Integral
    case _: java.lang.Long      => Kind.Wide
    case _: java.lang.Character => Kind.Symbolic
    case _                      => null

  private def word(key: Any): Long = key match
    case text: String                 => text.hashCode.toLong
    case number: java.lang.Integer    => number.intValue.toLong
    case number: java.lang.Long       => number.longValue
    case symbol: java.lang.Character  => symbol.charValue.toLong
    case _                            => 0L

  // One compiler for every map, made from the classloader that loaded this class, since that
  // is the one which can see `PerfectMap`.
  // The compiler insists on being used by one thread only, the first to use it, however the
  // others take their turns; so it has a thread of its own, and every map is compiled there.
  // A daemon, so that it never keeps a finished program alive.
  private lazy val worker: java.util.concurrent.ExecutorService =
    java.util.concurrent.Executors.newSingleThreadExecutor: runnable =>
      val thread = new Thread(runnable, "murmuration-optimizer")
      thread.setDaemon(true)
      thread

    . nn

  // `-experimental` because every Soundness class, `PerfectMap` among them, is compiled
  // under experimental language features and so may only be used by code that is too.
  private lazy val compiler: staging.Compiler =
    val options = scala.collection.immutable.List("-experimental")
    val settings = staging.Compiler.Settings.make(None, options)
    staging.Compiler.make(getClass.getClassLoader.nn)(using settings)

  // The word a key of `kind` is hashed by, or the answer -1 for a value of another kind.
  private def indexing
    ( kind: Kind, hash: PerfectHash, key: Expr[Any], table: Expr[scala.Array[Int]] )
    ( using Quotes )
  :   Expr[Int] =

    def slot(word: Expr[Long]): Expr[Int] = hash.quoted(List(word))(table)

    kind match
      case Kind.Textual =>
        ' {
            $key match
              case text: String => ${slot('{text.hashCode.toLong})}
              case _            => -1
          }

      case Kind.Integral =>
        ' {
            $key match
              case number: java.lang.Integer => ${slot('{number.intValue.toLong})}
              case _                         => -1
          }

      case Kind.Wide =>
        ' {
            $key match
              case number: java.lang.Long => ${slot('{number.longValue})}
              case _                      => -1
          }

      case Kind.Symbolic =>
        ' {
            $key match
              case symbol: java.lang.Character => ${slot('{symbol.charValue.toLong})}
              case _                           => -1
          }

  // The same map, read through a perfect hash of its keys; or the map as it was, if it has
  // too few entries to gain, its keys are not all texts, all `Int`s, all `Long`s or all
  // `Char`s, or no perfect hash of them was found.
  def optimize[key, value](map: Map[key, value]): Map[key, value] =
    val count = Map.size(map)
    val iterator = Set.iterator(Map.keys(map))
    val words = new scala.Array[Long](count)
    var kinds: Kind | Null = null
    var uniform = count > 4
    var position = 0

    // Bounded by the map's keys.
    while uniform && iterator.hasNext do
      val key: Any = iterator.next()
      val found = kind(key)
      if position == 0 then kinds = found
      if found == null || found != kinds then uniform = false else words(position) = word(key)
      position += 1

    val keyed = kinds
    val hash = if uniform && keyed != null then PerfectHash.search(words, 1) else null

    if hash == null || keyed == null then map else
      // The displacement table is handed to the generated class, not written into it: as a
      // literal, a table for a large map would overflow the JVM's limit on a method's size.
      type Maker = (scala.Array[Any], Int, scala.Array[Int]) => PerfectMap[Any, Any]

      val compilation: java.util.concurrent.Callable[Maker] = () =>
        given staging.Compiler = compiler

        staging.run:
          ' {
              (slots: scala.Array[Any], count: Int, table: scala.Array[Int]) =>
                new PerfectMap[Any, Any](slots, count):
                  def index(key: Any): Int = ${indexing(keyed, hash, 'key, 'table)}
            }

      val make: Maker = worker.submit(compilation).nn.get().nn
      val table: scala.Array[Int] = hash.table.asInstanceOf[scala.Array[Int]]

      Map.perfect(map, hash.capacity): (slots, count) =>
        make(slots, count, table).asInstanceOf[PerfectMap[key, value]]
