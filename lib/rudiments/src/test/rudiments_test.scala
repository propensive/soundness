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
package rudiments

import soundness.*
import denominative.dysasymptotics.linearSize

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Rudiments Tests"):
  def run(): Unit =

    test(m"Dual extraction"):
      object AsInt:
        def unapply(x: String): Option[Char] = Some('I')

      object AsLong:
        def unapply(x: String): Option[Char] = Some('J')

      "123" match
        case AsInt(x) & AsLong(y) => (x, y)
        case _                    => ('X', 'X')
    . assert(_ == ('I', 'J'))

    suite(m"has tests"):
      test(m"List membership via has"):
        List(1, 2, 3).has(2)
      . assert(_ == true)

      test(m"List non-membership via has"):
        List(1, 2, 3).has(4)
      . assert(_ == false)

      test(m"Sequence[Int] has is membership, not index validity"):
        Sequence(10, 20, 30).has(2)
      . assert(_ == false)

      test(m"Range has is membership, not index validity"):
        (100 to 110).has(5)
      . assert(_ == false)

      test(m"Range has positive membership"):
        (100 to 110).has(105)
      . assert(_ == true)

      test(m"Set has element"):
        Set(1, 2, 3).has(2)
      . assert(_ == true)

      test(m"Set has missing element"):
        Set(1, 2, 3).has(4)
      . assert(_ == false)

      test(m"frozen array element membership"):
        Array.of(1, 2, 3).has(2)
      . assert(_ == true)

      test(m"frozen array missing element"):
        Array.of(1, 2, 3).has(4)
      . assert(_ == false)

      test(m"Array element membership"):
        Inclusive.array[Int]
          . has(java.util.Arrays.copyOf(scala.Array(1, 2, 3), 3).nn.asInstanceOf[scala.Array[Int]], 2)
      . assert(_ == true)

      test(m"Map key membership"):
        Map(t"a" -> 1, t"b" -> 2).defines(t"a")
      . assert(_ == true)

      test(m"Map missing key"):
        Map(t"a" -> 1, t"b" -> 2).defines(t"c")
      . assert(_ == false)

    suite(m"Mapping tests"):
      // The `Map[…]`/`Set[…]` ascriptions are the established pattern for opaque-alias
      // literals (a bare literal in a receiver position mis-elaborates).
      test(m"List map preserves shape"):
        val xs: List[Int] = List(1, 2, 3)
        xs.map(_ + 1)
      . assert(_ == List(2, 3, 4))

      test(m"Set map preserves shape"):
        val xs: Set[Int] = Set(1, 2, 3)
        xs.map(_ + 1)
      . assert(_ == Set(2, 3, 4))

      test(m"Map map transforms values, preserving keys"):
        val m: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        m.map(_ + 10)
      . assert(_ == Map(t"a" -> 11, t"b" -> 12))

      test(m"Map map operand is the value, not the pair"):
        val m: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        m.map(_*2)
      . assert(_ == Map(t"a" -> 2, t"b" -> 4))

      test(m"remap transforms entries pairwise into a Map"):
        val m: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        m.remap { (key, value) => value -> key }
      . assert(_ == Map(1 -> t"a", 2 -> t"b"))

    suite(m"Query tests"):
      test(m"minimize finds the element with the least key"):
        val xs: List[Text] = List(t"epsilon", t"mu", t"beta")
        xs.minimize(_.length)
      . assert(_ == t"mu")

      test(m"maximize finds the element with the greatest key"):
        val xs: List[Text] = List(t"epsilon", t"mu", t"beta")
        xs.maximize(_.length)
      . assert(_ == t"epsilon")

      test(m"minimize of empty is Unset"):
        val xs: List[Int] = List()
        xs.minimize(identity(_))
      . assert(_ == Unset)

      test(m"last on an occupied chain demands the unbounded acknowledgement"):
        import denominative.dysasymptotics.unboundedSize
        val xs: Chain[Int] = Chain(1, 2, 3)
        xs.last
      . assert(_ == 3)

    suite(m"Size and count tests"):
      test(m"Set size is ungated"):
        val xs: Set[Int] = Set(1, 2, 3)
        xs.size
      . assert(_ == 3)

      test(m"List size demands the linear acknowledgement"):
        val xs: List[Int] = List(1, 2, 3)
        xs.size
      . assert(_ == 3)

      test(m"count by predicate"):
        val xs: List[Int] = List(1, 2, 3, 4)
        xs.count(_%2 == 0)
      . assert(_ == 2)

      test(m"count of a specific element is spelt with equality"):
        val xs: List[Int] = List(1, 2, 2, 3)
        xs.count(_ == 2)
      . assert(_ == 2)

    suite(m"last and lead tests"):
      test(m"last of a sequence"):
        val xs: Sequence[Int] = Sequence(1, 2, 3)
        xs.last
      . assert(_ == 3)

      test(m"lead is everything but the last"):
        val xs: Sequence[Int] = Sequence(1, 2, 3)
        xs.occupied.lay(Sequence[Int]())(_.lead)
      . assert(_ == Sequence(1, 2))

      test(m"last of a list demands the linear acknowledgement"):
        val xs: List[Int] = List(1, 2, 3)
        xs.last
      . assert(_ == 3)

      test(m"lead of a list"):
        val xs: List[Int] = List(1, 2, 3)
        xs.occupied.lay(List[Int]())(_.lead)
      . assert(_ == List(1, 2))

      test(m"lead then last reconstitutes the whole"):
        val xs: List[Int] = List(1, 2, 3)
        xs.occupied.lay(List[Int]()) { xs => xs.lead :+ xs.last }
      . assert(_ == List(1, 2, 3))

      test(m"last adapts: an unproven receiver yields an Optional"):
        val xs: Sequence[Int] = Sequence(1, 2, 3)
        val result: Optional[Int] = xs.last
        result
      . assert(_ == 3)

      test(m"last of an empty unproven receiver is Unset"):
        val xs: Sequence[Int] = Sequence()
        xs.last
      . assert(_ == Unset)

      test(m"last adapts: a Populated receiver yields the element itself"):
        val xs: Sequence[Int] = Sequence(1, 2, 3)
        // Typed as `Int`, not `Optional[Int]`: the ascription is the assertion.
        xs.occupied.lay(0): xs =>
          val total: Int = xs.last
          total
      . assert(_ == 3)

      test(m"last of an unproven list still demands the linear acknowledgement"):
        val xs: List[Int] = List(1, 2, 3)
        xs.last
      . assert(_ == 3)

      test(m"the ordinal-prefix operation is now `prefix`"):
        val xs: Sequence[Int] = Sequence(10, 20, 30)
        val interval: Interval = xs.prefix(_ => true)
        interval.limit.n0
      . assert(_ == 3)

    suite(m"glean and omit tests"):
      test(m"glean transforms the first match"):
        val xs: List[Int] = List(1, 2, 3, 4)
        xs.glean { case n if n%2 == 0 => n*10 }
      . assert(_ == 20)

      test(m"glean without a match is Unset"):
        val xs: List[Int] = List(1, 3)
        xs.glean { case n if n%2 == 0 => n*10 }
      . assert(_ == Unset)

      test(m"omit removes a map key"):
        val m: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        m.omit(t"b")
      . assert(_ == Map(t"a" -> 1))

      test(m"omit of an absent key changes nothing"):
        val m: Map[Text, Int] = Map(t"a" -> 1)
        m.omit(t"z")
      . assert(_ == Map(t"a" -> 1))

      test(m"maps concatenate right-biased"):
        val left: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        val right: Map[Text, Int] = Map(t"b" -> 20, t"c" -> 3)
        left + right
      . assert(_ == Map(t"a" -> 1, t"b" -> 20, t"c" -> 3))

    suite(m"Set algebra tests"):
      test(m"intersect keeps common elements"):
        val xs: Set[Int] = Set(1, 2, 3)
        xs.intersect(Set(2, 3, 4))
      . assert(_ == Set(2, 3))

      test(m"except removes the other set's elements"):
        val xs: Set[Int] = Set(1, 2, 3)
        xs.except(Set(2))
      . assert(_ == Set(1, 3))

      test(m"union is concatenation"):
        val xs: Set[Int] = Set(1, 2)
        val ys: Set[Int] = Set(2, 3)
        xs + ys
      . assert(_ == Set(1, 2, 3))

    suite(m"Keyed tests"):
      test(m"Map keys are a Set"):
        val m: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        m.keys
      . assert(_ == Set(t"a", t"b"))

      test(m"Map values are a List"):
        val m: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        m.values.to[Set]
      . assert(_ == Set(1, 2))

      test(m"Ledger keys are an insertion-ordered List"):
        val ledger: Ledger[Text, Int] = Ledger(t"c" -> 3, t"a" -> 1, t"b" -> 2)
        ledger.keys
      . assert(_ == List(t"c", t"a", t"b"))

      test(m"Ledger values preserve insertion order"):
        val ledger: Ledger[Text, Int] = Ledger(t"c" -> 3, t"a" -> 1, t"b" -> 2)
        ledger.values
      . assert(_ == List(3, 1, 2))

    suite(m"Branded literal tests"):
      // Qualified throughout: in this test module the unqualified `List` resolves to Scala's,
      // so an unqualified suite would test the wrong collection (discovered when the negative
      // cases passed vacuously).
      test(m"a non-empty List literal proves itself: head is bare"):
        val first: Int = proscenium.List(1, 2, 3).head
        first
      . assert(_ == 1)

      test(m"a singleton List literal is branded too"):
        val sole: Int = proscenium.List(42).head
        sole
      . assert(_ == 42)

      test(m"a non-empty Sequence literal proves itself"):
        val first: Int = proscenium.Sequence(5, 6).head
        first
      . assert(_ == 5)

      test(m"an empty List literal is not Populated"):
        demilitarize:
          val xs = proscenium.List[Int]()
          xs.head
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"a splatted sequence is not Populated"):
        demilitarize:
          val source: proscenium.List[Int] = proscenium.List(1, 2)
          val xs = proscenium.List(source.stdlib*)
          xs.head
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"a branded literal compares with its unbranded self"):
        proscenium.List(1, 2, 3) == (proscenium.List(1) + proscenium.List(2, 3)
            : proscenium.List[Int])
      . assert(_ == true)

    suite(m"Attested tests"):
      test(m"an attested ordinal reads bare, with no Optional"):
        import denominative.dysasymptotics.linearAccess
        val xs: proscenium.List[Int] = proscenium.List(10, 20, 30)

        unsafely:
          // In bounds by construction: the literal above has three elements.
          val third: Int = xs(xs.attested(Ter))
          third
      . assert(_ == 30)

      test(m"the block form scopes the attestation"):
        val xs: Sequence[Int] = Sequence(1, 2, 4)

        unsafely:
          // In bounds by construction: the literal above has three elements.
          xs.attested(Sec): ordinal =>
            val value: Int = xs(ordinal)
            value
      . assert(_ == 2)

      test(m"an attested map key reads bare"):
        val map: Map[Text, Int] = Map(t"one" -> 1, t"two" -> 2)

        unsafely:
          // Defined by construction: the literal above binds the key.
          val value: Int = map(map.attested(t"two"))
          value
      . assert(_ == 2)

      test(m"an attested interval drives iterate"):
        val xs: Sequence[Int] = Sequence(1, 2, 3, 4)
        var total = 0

        unsafely:
          // Valid by construction: [0, 2) lies within the four-element literal above.
          xs.iterate(Interval.zerary(0, 2).attested(xs)): ordinal =>
            total += xs(ordinal)

        total
      . assert(_ == 3)

    suite(m"Definable tests"):
      test(m"define replaces a sequence element positionally"):
        val xs: Sequence[Int] = Sequence(1, 2, 3)
        xs.define(Sec, 20)
      . assert(_ == Sequence(1, 20, 3))

      test(m"define outside the sequence returns it unchanged"):
        val xs: Sequence[Int] = Sequence(1, 2, 3)
        xs.define(Quat, 40)
      . assert(_ == Sequence(1, 2, 3))

      test(m"define replaces a map value by key"):
        val m: Map[Text, Int] = Map(t"a" -> 1, t"b" -> 2)
        m.define(t"b", 20)
      . assert(_ == Map(t"a" -> 1, t"b" -> 20))

      test(m"define adds an absent key"):
        val m: Map[Text, Int] = Map(t"a" -> 1)
        m.define(t"b", 2)
      . assert(_ == Map(t"a" -> 1, t"b" -> 2))

      test(m"define on a list demands the linear-access acknowledgement"):
        import denominative.dysasymptotics.linearAccess
        val xs: List[Int] = List(1, 2, 3)
        xs.define(Sec, 20)
      . assert(_ == List(1, 20, 3))

      test(m"define outside a list returns it unchanged"):
        import denominative.dysasymptotics.linearAccess
        val xs: List[Int] = List(1, 2, 3)
        xs.define(Quat, 40)
      . assert(_ == List(1, 2, 3))

      test(m"define replaces a frozen array element"):
        val xs: Array[Int]^{} = Array.tabulate(3)(_ + 1)
        xs.define(Prim, 10).to[List]
      . assert(_ == List(10, 2, 3))

    suite(m"Ordered reshaping tests"):
      test(m"Map sorts into a Ledger iterating in sorted order"):
        val m: Map[Text, Int] = Map(t"b" -> 2, t"c" -> 3, t"a" -> 1)
        m.sort { (key, value) => key }.to[List]
      . assert(_ == List(t"a" -> 1, t"b" -> 2, t"c" -> 3))

      test(m"Map sort result is a Ledger"):
        val m: Map[Text, Int] = Map(t"b" -> 2, t"a" -> 1)
        val sorted: Ledger[Text, Int] = m.sort { (key, value) => key }
        sorted.to[List]
      . assert(_ == List(t"a" -> 1, t"b" -> 2))

      test(m"Ledger filter preserves insertion order and shape"):
        val ledger: Ledger[Text, Int] = Ledger(t"c" -> 3, t"a" -> 1, t"b" -> 2)
        val filtered: Ledger[Text, Int] = ledger.filter { (key, value) => value != 1 }
        filtered.to[List]
      . assert(_ == List(t"c" -> 3, t"b" -> 2))

      test(m"Ledger sorts into a Ledger"):
        val ledger: Ledger[Text, Int] = Ledger(t"c" -> 3, t"a" -> 1, t"b" -> 2)
        val sorted: Ledger[Text, Int] = ledger.sort { (key, value) => value }
        sorted.to[List]
      . assert(_ == List(t"a" -> 1, t"b" -> 2, t"c" -> 3))

      test(m"Map non-pair stable reshape still yields a List"):
        val m: Map[Text, Int] = Map(t"b" -> 2, t"a" -> 1)
        val traced: List[Int] = m.trace(0) { (total, pair) => total + pair(1) }
        traced
      . assert(_ == List(0, 2, 3))

    suite(m"Confined index tests"):
      val text = t"hello"
      val array = Array.of(10, 20, 30)

      test(m"Plain `at` returns Optional"):
        text(Prim)
      . assert(_ == 'h')

      test(m"`within` + confined `at` returns a bare element"):
        Ter.within(text).let { i => val c: Char = text(i); c }
      . assert(_ == 'l')

      test(m"`within` returns Unset for an out-of-range Ordinal"):
        Ordinal.zerary(99).within(text)
      . assert(_ == Unset)

      test(m"`iterate` yields confined indices usable with bare `at`"):
        var total = 0
        array.iterate { i => total += array.at(i) }
        total
      . assert(_ == 60)

      test(m"`iterate` over a branded sub-interval visits only that range"):
        var total = 0
        array.iterate(array.prefix(_ => true).capped(2)) { i => total += array.at(i) }
        total
      . assert(_ == 30)

      test(m"`spot` finds the first matching confined index"):
        text.spot { i => text(i) == 'l' }.let { i => (i: Ordinal).n0 }
      . assert(_ == 2)

      test(m"`spot` returns Unset when nothing matches"):
        text.spot { i => text(i) == 'z' }
      . assert(_ == Unset)

      test(m"`tail` drops the first element of a collection"):
        val list: proscenium.List[Int] =
          (proscenium.List(1, 2, 3): proscenium.List[Int]).tail
        list.stdlib
      . assert(_ == scala.List(2, 3))

      test(m"`tail` of an empty collection is empty"):
        val list: proscenium.List[Int] = proscenium.List.empty[Int].tail
        list.stdlib
      . assert(_ == scala.Nil)

      test(m"`tail` drops the first character of a text"):
        text.tail
      . assert(_ == "ello".tt)

      test(m"`spot(after)` resumes the scan from the interval's limit"):
        // text = "hello": first 'l' after the prefix of non-'l's... then scan again past it
        val first = text.prefix { i => text(i) != 'l' }
        text.spot(first) { i => text(i) == 'l' }.let { i => (i: Ordinal).n0 }
      . assert(_ == 2)

      test(m"`spot(after)` returns Unset when nothing matches beyond the interval"):
        val all = text.prefix { _ => true }
        text.spot(all) { _ => true }
      . assert(_ == Unset)

      test(m"`prefix(after)` extends the interval through the next run"):
        val aitch = text.prefix { i => text(i) == 'h' }
        val interval: Interval = text.prefix(aitch) { i => text(i) == 'e' }
        ((interval: Interval).start.n0, interval.size)
      . assert(_ == (0, 2))

      test(m"`prefix(after)` with an empty run returns `after` unchanged"):
        val all = text.prefix { _ => true }
        val interval: Interval = text.prefix(all) { _ => true }
        ((interval: Interval).start.n0, interval.size)
      . assert(_ == (0, 5))

      test(m"chained `prefix(after)` scans equal one combined scan"):
        val digits = Array.of(1, 2, 0, 0, 7)
        val zeros = digits.prefix { i => digits.at(i) != 0 }
        val run = digits.prefix(zeros) { i => digits.at(i) == 0 }
        val combined = digits.prefix { i => digits.at(i) != 7 }
        (run: Interval) == (combined: Interval)
      . assert(_ == true)

      test(m"`lead` spans the matching prefix and stops at the first mismatch"):
        val interval: Interval = text.prefix { i => text(i) != 'l' }
        interval.size
      . assert(_ == 2)

      test(m"`lead` spans the whole extent when everything matches"):
        val interval: Interval = text.prefix { _ => true }
        interval.size
      . assert(_ == 5)

      test(m"`pare` drops the matching suffix, respecting the floor"):
        val digits = Array.of(3, 7, 0, 0, 0)
        val interval: Interval = digits.pare(1) { i => digits.at(i) == 0 }
        interval.size
      . assert(_ == 2)

      test(m"`pare` never shrinks below its floor"):
        val zeros = Array.of(0, 0, 0)
        val interval: Interval = zeros.pare(1) { i => zeros.at(i) == 0 }
        interval.size
      . assert(_ == 1)

      test(m"`retrace` visits confined indices in reverse"):
        val builder = java.lang.StringBuilder()
        text.retrace { i => builder.append(text(i)) }
        builder.toString.tt
      . assert(_ == t"olleh")

    suite(m"Scribe tests"):
      test(m"`Array.scribe` fills through branded indices"):
        Array.scribe[Int](4) { scribe => range => scribe.iterate { i => scribe(i) = (i: Ordinal).n0*2 } }
        . to[List]
      . assert(_ == List(0, 2, 4, 6))

      test(m"a scribe reads back what it wrote"):
        var last = -1

        Array.scribe[Int](3): scribe =>
          range =>
            scribe.iterate { i => scribe(i) = 7 }
            scribe.iterate { i => last = scribe(i) }

        last
      . assert(_ == 7)

      test(m"`place` copies a whole frozen array, clamped to the space"):
        val source = Array.of(1, 2, 3, 4, 5)

        val target = Array.scribe[Int](4): scribe =>
          range => scribe.iterate { i => if (i: Ordinal) == Sec then scribe.place(source, i) }

        target.to[List]
      . assert(_ == List(0, 1, 2, 3))

      test(m"`append` writes sequentially and clamps at the end"):
        Array.scribe[Int](3): scribe =>
          _ =>
            scribe.append(5)
            scribe.append(6)
            scribe.append(7)
            scribe.append(8)
        . to[List]
      . assert(_ == List(5, 6, 7))

      test(m"`mark` counts appended elements"):
        var count = -1

        Array.scribe[Int](5): scribe =>
          _ =>
            scribe.append(1)
            scribe.append(2)
            count = scribe.mark

        count
      . assert(_ == 2)

      test(m"a surveyor skips whitespace and reports the run"):
        val line = t"   indent"
        var skipped = -1
        var next = ' '

        line.survey: surveyor =>
          skipped = (surveyor.pace(_ == ' '): Interval).size
          surveyor.point.let { i => next = line(i) }

        (skipped, next)
      . assert(_ == ((3, 'i')))

      test(m"a surveyor detects successive runs with their lengths"):
        val styles = Array.of(7L, 7L, 7L, 9L, 9L, 3L)
        var lengths: List[Int] = Nil

        styles.survey: surveyor =>
          while surveyor.more do
            surveyor.point.let: start =>
              val style = styles.at(start)
              lengths ::= (surveyor.pace(_ == style): Interval).size

        lengths.reverse
      . assert(_ == List(3, 2, 1))

      test(m"a negated `pace` stops at the delimiter, and `remainder` brands the rest"):
        val csv = t"key:value"
        var key = t""
        var rest = -1

        csv.survey: surveyor =>
          val name = surveyor.pace(_ != ':')
          val builder = java.lang.StringBuilder()
          csv.iterate(name) { i => builder.append(csv(i)) }
          key = builder.toString.tt
          rest = (surveyor.remainder: Interval).size

        (key, rest)
      . assert(_ == ((t"key", 6)))

      test(m"`peek` tests the current element without advancing"):
        val text = t"-x"

        text.survey: surveyor =>
          val dash = surveyor.peek(_ == '-')
          val same = surveyor.peek(_ == '-')
          surveyor.advance()
          (dash, same, surveyor.peek(_ == '-'), surveyor.peek(_ == 'x'))
      . assert(_ == ((true, true, false, true)))

      test(m"`matches` compares a pattern without advancing"):
        val data = t"abcdef"

        data.survey: surveyor =>
          surveyor.advance()
          val hit = surveyor.matches(t"bcd") { (left, right) => left == right }
          val miss = surveyor.matches(t"bce") { (left, right) => left == right }
          val long = surveyor.matches(t"bcdefgh") { (left, right) => left == right }
          (hit, miss, long, surveyor.passed)
      . assert(_ == ((true, false, false, 1)))

      test(m"`glimpse` lends a branded lookahead window without advancing"):
        val text = t"hello"

        text.survey: surveyor =>
          val window = surveyor.glimpse(3).let { interval => (interval: Interval).size }
          val overrun = surveyor.glimpse(9)
          (window, overrun, surveyor.passed)
      . assert(_ == ((3, Unset, 0)))

      test(m"`next` consumes elements one at a time"):
        val text = t"ab"
        val builder = java.lang.StringBuilder()

        text.survey: surveyor =>
          while surveyor.more do surveyor.next(()) { char => builder.append(char) }
          surveyor.next(builder.append('!')) { char => builder.append(char) }

        builder.toString.tt
      . assert(_ == t"ab!")

      test(m"`take` consumes a counted, clamped run"):
        val text = t"abcde"
        var sizes: List[Int] = Nil

        text.survey: surveyor =>
          sizes ::= (surveyor.take(2): Interval).size
          sizes ::= (surveyor.take(9): Interval).size

        sizes.reverse
      . assert(_ == List(2, 3))

      test(m"`triples` visits whole groups and returns the branded remainder"):
        val data = Array.of[Byte](1, 2, 3, 4, 5, 6, 7, 8)
        var sums: List[Int] = Nil
        var rest = -1

        val remainder = data.triples { (a, b, c) => sums ::= a + b + c }
        rest = (remainder: Interval).size

        (sums.reverse, rest)
      . assert(_ == ((List(6, 15), 2)))

      test(m"`pairs` and `quads` group evenly with empty remainders"):
        val data = Array.of[Byte](1, 2, 3, 4)
        var pairSum = 0
        var quadSum = 0

        val pairRest = data.pairs { (a, b) => pairSum += a + b }
        val quadRest = data.quads { (a, b, c, d) => quadSum += a + b + c + d }

        (pairSum, (pairRest: Interval).size, quadSum, (quadRest: Interval).size)
      . assert(_ == ((10, 0, 10, 0)))

      test(m"`adjacent` visits every overlapping pair"):
        val text = t"abcd"
        val builder = java.lang.StringBuilder()
        text.adjacent { (left, right) => builder.append(left).nn.append(right).nn.append('.') }
        builder.toString.tt
      . assert(_ == t"ab.bc.cd.")

      test(m"a lattice mints whole rows within the storage"):
        // 10 elements, rows of 3 spaced 4 apart: rows at 0, 4 — a third row (start 8,
        // needing 8+3 <= 10) does not fit.
        val data = Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
        var rows: List[List[Int]] = Nil

        data.lattice(3, 4, 0): lattice =>
          lattice.rows: (y, row) =>
            var elements: List[Int] = Nil
            data.iterate(row) { i => elements ::= data.at(i) }
            rows ::= elements.reverse

        rows.reverse
      . assert(_ == List(List(0, 1, 2), List(4, 5, 6)))

      test(m"`point` linearizes in-range coordinates and rejects others"):
        val data = Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

        data.lattice(3, 4, 0): lattice =>
          val hit = lattice.point(2, 1).let { i => data.at(i) }
          (hit, lattice.point(3, 1), lattice.point(0, 3), lattice.height)
      . assert(_ == ((6, Unset, Unset, 3)))

      test(m"a lattice with an offset addresses a sub-plane"):
        val data = Array.of(9, 9, 0, 1, 2, 3)
        var sum = 0

        data.lattice(2, 2, 2): lattice =>
          lattice.rows { (y, row) => data.iterate(row) { i => sum += data.at(i) } }
          sum += lattice.height*100

        sum
      . assert(_ == 206)

      test(m"a lattice over a scribe writes through branded rows"):
        Array.scribe[Int](6): scribe =>
          _ =>
            scribe.lattice(2, 3, 0): lattice =>
              lattice.rows { (y, row) => scribe.iterate(row) { i => scribe(i) = y + 1 } }
        . to[List]
      . assert(_ == List(1, 1, 0, 2, 2, 0))

      test(m"an exhausted surveyor has no point and an empty remainder"):
        val text = t"ab"

        text.survey: surveyor =>
          surveyor.pace { _ => true }
          (surveyor.more, surveyor.point, (surveyor.remainder: Interval).size)
      . assert(_ == ((false, Unset, 0)))

      test(m"the scan family applies to a scribe"):
        var kept = -1

        Array.scribe[Int](5): scribe =>
          range =>
            scribe.iterate { i => scribe(i) = (i: Ordinal).n0 }
            kept = (scribe.pare(0) { i => scribe(i) > 2 }: Interval).size

        kept
      . assert(_ == 3)

    // test(m"Display a PID"):
    //   Pid(2999).toString
    // .assert(_ == "↯2999")

    suite(m"Mean tests"):
      test(m"Simple median"):
        Iterable[Double](7, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21).mean
      . assert(_.lay(false)(_ === 12.454545 ± 0.00001))

      test(m"Simple median, different pivot"):
        Iterable[Double](25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7).mean
      . assert(_.lay(false)(_ === 12.454545 ± 0.00001))

      test(m"Simple median, even items"):
        Iterable[Double](25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7, 8).mean
      . assert(_.lay(false)(_ === 12.1 ± 0.1))

      test(m"Simple median, even items, different order"):
        Iterable[Double](8, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7).mean
      . assert(_.lay(false)(_ === 12.1 ± 0.1))

      test(m"Simple median, even items, different elements"):
        Iterable[Double](10, 125, -1, 124, -2, -3, 123, -4, 122, -5, 121, 9).mean
      . assert(_.lay(false)(_ === 51.6 ± 0.1))

      test(m"Mean of temperatures"):
        Iterable(Fahrenheit(10), Fahrenheit(10), Fahrenheit(40)).mean2
      . assert(_ == Fahrenheit(20))

      test(m"Mean of even number of temperatures"):
        Iterable(Celsius(40), Celsius(35), Celsius(45), Celsius(75)).mean2
      . assert(_ == Celsius(48.75))

      test(m"Mean of quantities"):
        Iterable(10*Metre/Second, 30*Metre/Second, 5*Metre/Second, 20*Metre/Second).mean
      . assert(_ == 16.25*Metre/Second)

    suite(m"bin tests"):
      test(m"Specify a byte"):
        val x: Byte = bin"10101010"
        x
      . assert(_ == -86)

      test(m"Specify a short"):
        val x: Short = bin"01111111 11111111"
        x
      . assert(_ == 32767)

      test(m"Specify an integer"):
        val x: Int = bin"00000000 11111111 11111111 11111111"
        x
      . assert(_ == 16777215)

      test(m"Specify a long"):
        val x: Long = bin"10101010 10101010 10101010 10101010 00000000 11111111 11111111 11111111"
        x
      . assert(_ == -6148914694083051521L)

      test(m"Too many bits"):
        demilitarize:
          val long: Long =
            bin"010101010 10101010 10101010 10101010 00000000 11111111 11111111 11111111"

          long
        .map(_.message)
      . assert(_ == List(t"[↯SN-167] a binary literal must be 8, 16, 32 or 64 bits long"))

      test(m"Incorrect bit count"):
        demilitarize:
          val x: Long = bin"0101010 10101010 10101010 10101010 00000000 11111111 11111111 11111111"
          x
        .map(_.message)
      . assert(_ == List(t"[↯SN-167] a binary literal must be 8, 16, 32 or 64 bits long"))

      test(m"Too many bits for type"):
        demilitarize:
          val x: Byte = bin"00011111 11111111"
          x
      . assert(_.nonEmpty)

      test(m"Non-binary content"):
        demilitarize:
          bin"00011112 11111111"
        .map(_.message)
      . assert(_ == List(t"[↯SN-118] a binary value can only contain characters '0' or '1'"))

    suite(m"hex tests"):
      test(m"Specify some bytes"):
        hex"bacdf1e9".to[List]
      . assert(_ == Data(-70, -51, -15, -23).to[List])

      test(m"Specify some bytes in uppercase with a space"):
        hex"BACD F1E9".to[List]
      . assert(_ == Data(-70, -51, -15, -23).to[List])

      test(m"Non-even number of bytes"):
        demilitarize:
          hex"bacdf1e"
        .map(_.message)
      . assert(_ == List(t"[↯SN-137] a hexadecimal value must have an even number of digits"))

      test(m"Non-hex content"):
        demilitarize:
          hex"bacdf1eg"
        .map(_.message)
      . assert(_ == List(t"[↯SN-148] g is not a valid hexadecimal character"))

      /*test(m"Convert a byte to hex"):
        126.toByte.hex
      . assert(_ == t"7e")

      test(m"Convert a short to hex"):
        32767.toShort.hex
      . assert(_ == t"7fff")

      test(m"Convert an integer to hex"):
        123456789.hex
      . assert(_ == t"75bcd15")

      test(m"Convert a long to hex"):
        654321123456789L.hex
      . assert(_ == t"2531a0221f715")*/

      // test(m"Pattern match hex"):
      //   t"1234" match
      //     case Hex(value) => value
      //     case _          => 0
      // .assert(_ == 4660)

    suite(m"Collections tests"):
      val numbers = List(t"one", t"two", t"four", t"six", t"eight", t"nine")

      // test(m"Index unique numbers by their first letter"):
      //   safely:
      //     numbers.indexBy(_.prim)
      // .assert(_ == Map('o' -> t"one", 't' -> t"two", 'f' -> t"four", 's' -> t"six", 'e' -> t"eight", 'n' -> t"nine"))

      //test(m"Index unique numbers by their length"):
      //  capture[DuplicateIndexError]:
      //    numbers.indexBy(_.length)
      //.assert(_ == DuplicateIndexError())

      test(m"Sift some options"):
        List(None, Some(1), Some(2), None).sift[None.type]
      . assert(_ == List(None, None))

      test(m"Sift on singleton type"):
        List.range(0, 10).sift[5]
      . assert(_ == List(5))

      test(m"Sift on a union of singleton types"):
        List.range(0, 10).sift[5 | 7]
      . assert(_ == List(5, 7))

      test(m"Map a List to twins"):
        List(1, 2, 3).bi
      . assert(_ == List((1, 1), (2, 2), (3, 3)))

      test(m"Map a Set to triples"):
        Set(1, 2, 3).tri
      . assert(_ == Set((1, 1, 1), (2, 2, 2), (3, 3, 3)))

      test(m"Take a snapshot of an array"):
        val array = scala.Array[Int](1, 2, 3, 4, 5)
        array(1) = 17
        val snapshot: Array[Int]^{} = array.snapshot
        array(1) = 42
        snapshot.to[List]
      . assert(_ == List(1, 17, 3, 4, 5))

      test(m"Take Map#upsert as an insertion"):
        val map = Map(1 -> "one", 2 -> "two")
        map.upsert(3, _.or("three"))
      . assert(_ == Map(1 -> "one", 2 -> "two", 3 -> "three"))

      test(m"Take Map#upsert as an update"):
        val map = Map(1 -> "one", 2 -> "two")
        map.upsert(2, _.or("")+"!")
      . assert(_ == Map(1 -> "one", 2 -> "two!"))

      test(m"Collation"):
        val map1: Map[Int, List[String]] = Map(1 -> List("one"), 2 -> List("two"))
        val map2: Map[Int, List[String]] = Map(2 -> List("deux"), 3 -> List("trois"))
        map1.collate(map2): (left, right) =>
          left + right
      . assert(_ == (Map(1 -> List("one"), 2 -> List("two", "deux"), 3 -> List("trois"))
            : Map[Int, List[String]]))

      test(m"runs"):
        List(1, 2, 2, 1, 1, 1, 4, 4).runs
      . assert(_ == List(List(1), List(2, 2), List(1, 1, 1), List(4, 4)))

      test(m"runsBy"):
        List(1, 2, 2, 1, 1, 1, 4, 4).runsBy(_%3)
      . assert(_ == List(List(1), List(2, 2), List(1, 1, 1, 4, 4)))

    suite(m"Longest train tests"):
      test(m"Find longest train of zeros in middle"):
        List(1, 0, 0, 2, 3, 4, 0, 0, 0, 5, 6, 0, 7).longestTrain(_ == 0)
      . assert(_ == (6, 3))

      test(m"Find longest train of zeros at start"):
        List(0, 0, 0, 2, 3, 4, 0, 0, 1, 5, 6, 0, 7).longestTrain(_ == 0)
      . assert(_ == (0, 3))

      test(m"Find longest train of zeros at end"):
        List(0, 0, 1, 2, 3, 4, 0, 0, 1, 5, 6, 0, 0, 0, 0).longestTrain(_ == 0)
      . assert(_ == (11, 4))

    suite(m"Optional tests"):
      val absentInt: Optional[Int] = Unset
      val setInt: Optional[Int] = 42

      test(m"Check whether absent value is absent"):
        absentInt.absent
      . assert(_ == true)

      test(m"Check thet set value is not absent"):
        setInt.absent
      . assert(_ == false)

      test(m"Assume a set value is present"):
        val x: Int = unsafely(setInt.assume)
        x
      . assert(_ == 42)

      test(m"Provide an alternative for an absent value"):
        absentInt.or(1)
      . assert(_ == 1)

      test(m"Provide an alternative for a set value"):
        setInt.or(1)
      . assert(_ == 42)

      test(m"Presume a default value for an absent value"):
        absentInt.presume
      . assert(_ == 0)

      test(m"Convert an absent value to an Option"):
        absentInt.option
      . assert(_ == None)

      test(m"Convert a set value to an Option"):
        setInt.option
      . assert(_ == Some(42))

      test(m"Fold over a Optional"):
        absentInt.lay(0)(_ + 1)
      . assert(_ == 0)

      test(m"Fold over a set Optional"):
        setInt.lay(0)(_ + 1)
      . assert(_ == 43)

      test(m"Map over an absent Optional"):
        absentInt.let(_ + 1)
      . assert(_ == Unset)

      test(m"Map over a set Optional"):
        setInt.let(_ + 1)
      . assert(_ == 43)

      test(m"Construct a new Optional from a null value"):
        val x: String | Null = null
        Optional(x)
      . assert(_ == Unset)

      test(m"Construct a new Optional from a possibly-null value"):
        val x: String | Null = ""
        Optional(x)
      . assert(_ == "")

      test(m"Convert an option to an optional"):
        val x: Option[Int] = Some(42)
        x.optional
      . assert(_ == 42)

      test(m"Convert an empty Option to an optional"):
        val x: Option[Int] = None
        x.optional
      . assert(_ == Unset)

      // test(m"Presume a value for an empty Option"):
      //   val x: Option[List[Int]] = None
      //   x.presume
      // .assert(_ == Nil)

    suite(m"PID & exit status tests"):
      test(m"Zero exit-status is OK"):
        Exit(0)
      . assert(_ == Exit.Ok)

      test(m"Positive exit-status is a failure"):
        Exit(1)
      . assert(_ == Exit.Fail(1))

      test(m"Ok has exit status 0"):
        Exit.Ok
      . assert(_() == 0)

      test(m"Failure has non-zero exit status"):
        Exit.Fail(3)
      . assert(_() == 3)

    suite(m"Data tests"):
      test(m"Construct a `Data` literal"):
        Data(1, 2, 3)
      . assert(_.length == 3)

      // test(m"Construct a `Data` value from a Long"):
      //   Data(Long.MaxValue)
      // .assert(_.length == 8)

      test(m"Construct an empty `Data`"):
        Data()
      . assert(_.length == 0)

    suite(m"Bytes tests"):
      test(m"Construct a simple Bytes"):
        4.b: Bytes
      . assert(_ == Bytes(4))

      test(m"Divide one Bytes by an integer"):
        1024.b/128
      . assert(_ == 8.b)

      test(m"Divide one `Bytes` by another"):
        1024.b/128.b
      . assert(_ == 8.0)

      // test(m"Construct a simple Bytes in kB"):
      //   4.kb: Bytes
      // .assert(_ == Bytes(4096))

      // test(m"Construct a simple Bytes in MB"):
      //   4.mb: Bytes
      // .assert(_ == Bytes(4096*1024L))

      // test(m"Construct a simple Bytes in GB"):
      //   4.gb: Bytes
      // .assert(_ == Bytes(4096*1024L*1024L))

      // test(m"Construct a simple Bytes in TB"):
      //   4.tb: Bytes
      // .assert(_ == Bytes(4096*1024L*1024L*1024L))

      /*test(m"Compare bytes with >"):
        4.gb > 4.mb
      . assert(_ == true)

      test(m"Compare bytes with >="):
        4.gb >= 4.mb*1024
      . assert(_ == true)*/

      // test(m"Sort some byte sizes"):
      //   List(1.b, 1.mb, 1.kb).sorted
      // .assert(_ == List(1.b, 1.kb, 1.mb))

    // suite(m"Y-combinator test"):
    //   test(m"Check factorial implementation"):
    //     def factorial(n: Int): Int = fix[Int] { i => if i <= 0 then 1 else i*recur(i - 1) } (n)
    //     factorial(4)
    //   .assert(_ == 24)

    // While the collection aliases remain transparent, the stdlib member `to(Factory)` shadows
    // the kind-polymorphic `to` for collection receivers, so `Text` (no such member) is the
    // receiver these tests exercise; collection receivers activate as the aliases become opaque.
    suite(m"Convertible tests"):
      test(m"Text to List of chars"):
        "abc".tt.to[List]
      . assert(_ == List('a', 'b', 'c'))

      test(m"Text to Set of chars deduplicates"):
        "aba".tt.to[Set]
      . assert(_ == Set('a', 'b'))

      test(m"Text to Sequence of chars"):
        "abc".tt.to[Sequence]
      . assert(_ == Sequence('a', 'b', 'c'))

      test(m"Text to Text is the identity"):
        "abc".tt.to[Text]
      . assert(_ == "abc".tt)

      test(m"Result type of to[List] is inferred fully applied"):
        val list: List[Char] = "xy".tt.to[List]
        list.size
      . assert(_ == 2)

    suite(m"Vacuiscible tests"):
      test(m"non-empty Text is not nil"):
        "abc".tt.nil
      . assert(_ == false)

      test(m"empty Text is nil"):
        "".tt.nil
      . assert(_ == true)

    suite(m"confine tests"):
      test(m"confined Map key accesses bare value"):
        val map = Map(1 -> "one".tt, 2 -> "two".tt)
        map.confine(1).let(map(_))
      . assert(_ == "one".tt)

      test(m"absent Map key does not confine"):
        val map = Map(1 -> "one".tt)
        map.confine(9).let(map(_))
      . assert(_ == Unset)
