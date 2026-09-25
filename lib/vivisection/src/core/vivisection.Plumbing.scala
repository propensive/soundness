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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package vivisection

import scala.collection.concurrent as scc

import anticipation.*
import gossamer.*
import proscenium.*
import rudiments.*
import vacuous.*

// The classification of JVM methods the programmer never wrote: bridges, mixin and static
// forwarders, field accessors, lazy-val accessors, `$outer` accessors, and the frames of runtime
// machinery (boxing, lambda metafactories, reflection). A logical step passes through them
// without stopping, so stepping into `robot.greet(name)` arrives in `greet`'s body rather than the
// forwarder which delegates to it. Flags and names classify most; the rest (accessors and
// forwarders carry no flag) are recognised by their bytecode shape, which needs the class's
// constant pool to name the callee.
private[vivisection] object Plumbing:
  // Class-signature prefixes (JVM internal form) of the runtime machinery a step should never rest
  // in: Scala's boxing and lazy-val support, and the whole of the JDK — whose lambda
  // metafactories, method handles, reflection and class loading are what a step into user code
  // otherwise lands in, on a class's first use. This is `jdb`'s default exclusion list plus
  // Scala's runtime.
  val noise: List[Text] =
    List(t"Lscala/runtime/", t"Ljava/", t"Ljavax/", t"Ljdk/", t"Lsun/", t"Lcom/sun/")

  // The same, in the `ClassMatch` form a JDWP event request's `ClassExclude` modifier takes.
  val excluded: List[Text] =
    List(t"scala.runtime.*", t"java.*", t"javax.*", t"jdk.*", t"sun.*", t"com.sun.*")

  private val AccBridge: Int = 0x0040
  private val AccSynthetic: Int = 0x1000

  def noisy(signature: Text): Boolean = noise.exists(signature.starts(_))

  // Whether a method's flags and name alone mark it as plumbing: any bridge, and any synthetic
  // method except a lambda body, which is user code the compiler happened to lift into a method.
  def flagged(info: Jdwp.MethodInfo): Boolean =
    val bridge = (info.modifiers & AccBridge) != 0
    val synthetic = (info.modifiers & AccSynthetic) != 0
    bridge || (synthetic && !info.name.contains(t"$$anonfun$$"))

  // Whether `name` is the accessor of a lazy val: the compiler emits its initializer as a sibling
  // `name$lzyINIT<n>`, and the accessor's only interesting act is calling it.
  def lazyAccessor(name: Text, siblings: List[Jdwp.MethodInfo]): Boolean =
    siblings.exists(_.name.starts(t"$name$$lzyINIT"))

  object Pool:
    val empty: Pool = Pool(scc.TrieMap())

    // Parses `count - 1` entries in classfile order. Only the entries a method reference chains
    // through are kept — Utf8, the three reference kinds and NameAndType — and every other kind
    // is skipped by its fixed width. Longs and doubles occupy two indices.
    def parse(count: Int, reader: Jdwp.Reader): Pool =
      val entries: scc.TrieMap[Int, Entry] = scc.TrieMap()

      def recur(index: Int): Unit = if index < count && reader.remaining > 0 then
        val tag = reader.byte() & 0xff

        tag match
          case 1 =>
            val length = reader.short() & 0xffff
            entries(index) = Entry.Utf8(reader.modifiedUtf8(length))
            recur(index + 1)

          case 9 | 10 | 11 =>
            val cls = reader.short() & 0xffff
            entries(index) = Entry.Ref(cls, reader.short() & 0xffff)
            recur(index + 1)

          case 12 =>
            val name = reader.short() & 0xffff
            entries(index) = Entry.NameAndType(name, reader.short() & 0xffff)
            recur(index + 1)

          case 5 | 6 =>
            reader.skip(8)
            recur(index + 2)

          case 3 | 4 | 17 | 18 =>
            reader.skip(4)
            recur(index + 1)

          case 15 =>
            reader.skip(3)
            recur(index + 1)

          case 7 | 8 | 16 | 19 | 20 =>
            reader.skip(2)
            recur(index + 1)

          case _ =>
            () // an unknown tag: its width is unknown too, so the rest of the pool is lost

      recur(1)
      Pool(entries)

  enum Entry:
    case Utf8(text: Text)
    case Ref(cls: Int, nameAndType: Int)
    case NameAndType(name: Int, descriptor: Int)

  class Pool(entries: scc.TrieMap[Int, Entry]):
    private def utf8(index: Int): Optional[Text] = entries.get(index) match
      case scala.Some(Entry.Utf8(text)) => text
      case _                            => Unset

    // The name of the member a method-reference entry names.
    def memberName(index: Int): Optional[Text] = entries.get(index) match
      case scala.Some(Entry.Ref(_, nameAndType)) => entries.get(nameAndType) match
        case scala.Some(Entry.NameAndType(name, _)) => utf8(name)
        case _                                      => Unset

      case _ =>
        Unset

  // A summary of a method's bytecode in the vocabulary trivial methods are written in: local
  // loads, static and instance field reads, field writes, invocations (by callee name) and
  // returns. Any other opcode ends the scan and marks the method as substantive.
  private case class Shape
    ( loads:     Int            = 0,
      statics:   Int            = 0,
      getfields: Int            = 0,
      putfields: Int            = 0,
      invokes:   Int            = 0,
      callee:    Optional[Text] = Unset,
      returns:   Int            = 0,
      other:     Boolean        = false )

  private def shape(code: Jdwp.Reader, pool: Pool): Shape =
    def recur(shape: Shape): Shape = if code.remaining <= 0 || shape.other then shape else
      val opcode = code.byte() & 0xff

      if opcode >= 0x15 && opcode <= 0x19 then
        code.skip(1)
        recur(shape.copy(loads = shape.loads + 1))
      else if opcode >= 0x1a && opcode <= 0x2d then
        recur(shape.copy(loads = shape.loads + 1))
      else if opcode >= 0xac && opcode <= 0xb1 then
        recur(shape.copy(returns = shape.returns + 1))
      else
        opcode match
        case 0xb2 =>
          code.skip(2)
          recur(shape.copy(statics = shape.statics + 1))

        case 0xb4 =>
          code.skip(2)
          recur(shape.copy(getfields = shape.getfields + 1))

        case 0xb5 =>
          code.skip(2)
          recur(shape.copy(putfields = shape.putfields + 1))

        case 0xb6 | 0xb7 | 0xb8 | 0xb9 =>
          val callee = pool.memberName(code.short() & 0xffff)
          if opcode == 0xb9 then code.skip(2)
          recur(shape.copy(invokes = shape.invokes + 1, callee = callee))

        case _ =>
          shape.copy(other = true)

    recur(Shape())

  // Whether a method's bytecode is one of the three trivial shapes: a getter (load `this`, read
  // a field, return), a setter (load `this` and the value, write a field, return), or a
  // forwarder (load the arguments, or the module instance and the arguments; make one call to a
  // method of the same name, and return its result). A trait method's static accessor carries a
  // trailing `$`: a class's mixin forwarder `greet` calls `Greeter.greet$`, which in turn calls
  // the default method `greet` holding the body, so the names may differ by the `$` in either
  // direction.
  def trivial(name: Text, code: Jdwp.Reader, pool: Pool): Boolean =
    val summary = shape(code, pool)

    if summary.other || summary.returns != 1 then false
    else if summary.invokes == 0 then
      val getter = summary.loads == 1 && summary.getfields == 1 && summary.putfields == 0
      val setter = summary.loads == 2 && summary.putfields == 1 && summary.getfields == 0
      summary.statics == 0 && (getter || setter)
    else
      val delegates = summary.callee.lay(false): callee =>
        callee == name || callee == t"$name$$" || name == t"$callee$$"
      val pure = summary.getfields == 0 && summary.putfields == 0 && summary.statics <= 1
      pure && summary.invokes == 1 && delegates
