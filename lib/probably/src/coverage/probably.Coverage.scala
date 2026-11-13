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
package probably

// The state which coverage-instrumented classes write to, and a host reads: which probes each
// test reached. A class is instrumented by `probably.Instrumenter`, which recompiles it from its
// TASTy with an array store at every method entry and branch; this object decides which array.
//
// Each test, while it runs, has a `Collector`, held in an `InheritableThreadLocal`: `enter` and
// `exit` (called by `Runner`) bracket the test body on the thread which runs it, and a thread
// started within the bracket inherits the collector, as does any thread that thread starts. So a
// test is credited with what the threads it owns do. A thread which outlives its test, or one
// which was started before it (a pool's), has no open collector, and what it reaches is
// credited to no test: it is recorded against the empty name.
//
// Instrumented code asks for its unit's array once per method, with `probes`, and then stores
// into it directly, so the cost of a probe is that of an array store, and the cost of entering
// a method is a thread-local read. Stores race benignly: every one writes `true`.
//
// Everything here is written against the JDK alone, and its signatures name only JDK types:
// instrumented code may be any class at all — one this library's own dependencies are made of —
// and the host reads the results reflectively, across a classloader boundary.
object Coverage:
  private type Units = Array[Array[Boolean] | Null]

  final class Collector private[Coverage] (val test: String):
    @volatile private var open: Boolean = true
    @volatile private var units: Units = new Array[Array[Boolean] | Null](16)

    private[Coverage] def live: Boolean = open
    private[Coverage] def close(): Unit = open = false

    private[Coverage] def probes(unit: Int, size: Int): Array[Boolean] =
      val current: Units = units
      val found: Array[Boolean] | Null = if unit < current.length then current(unit) else null
      if found == null then allocate(unit, size) else found

    private def allocate(unit: Int, size: Int): Array[Boolean] = synchronized:
      val current: Units = units

      val sized: Units =
        if unit < current.length then current else
          val grown: Units = new Array[Array[Boolean] | Null](math.max(unit + 1, current.length*2))
          System.arraycopy(current, 0, grown, 0, current.length)
          grown

      val found: Array[Boolean] | Null = sized(unit)

      val result: Array[Boolean] =
        if found == null then
          val fresh: Array[Boolean] = new Array[Boolean](size)
          sized(unit) = fresh
          fresh
        else found

      // The volatile write publishes the new array, and the element stored into it.
      units = sized
      result

    // A copy of what has been reached, with an empty array for a unit never entered; `null`
    // if no unit was.
    private[Coverage] def snapshot(): Array[Array[Boolean]] | Null = synchronized:
      val current: Units = units
      var touched: Boolean = false
      val copy: Array[Array[Boolean]] = new Array[Array[Boolean]](current.length)
      var index: Int = 0

      while index < current.length do
        val found: Array[Boolean] | Null = current(index)

        copy(index) =
          if found == null then new Array[Boolean](0) else
            touched = true
            java.util.Arrays.copyOf(found, found.length).nn

        index += 1

      if touched then copy else null

    private[Coverage] def reset(): Unit = synchronized:
      units = new Array[Array[Boolean] | Null](16)

  @volatile private var active: Boolean = false
  private val orphans: Collector = Collector("")
  private val current: InheritableThreadLocal[Collector | Null] = new InheritableThreadLocal()

  private val finished: java.util.HashMap[String, Array[Array[Boolean]]] =
    new java.util.HashMap()

  // Called by the host before it runs anything: until then, `enter` does nothing, so that a run
  // without coverage pays nothing for it.
  def activate(): Unit = active = true
  def enabled: Boolean = active

  // The array for a unit's probes, for the test which owns the calling thread. Called by
  // instrumented code, with the unit's index and its number of probes as constants.
  def probes(unit: Int, size: Int): Array[Boolean] =
    val collector: Collector | Null = current.get()

    if collector == null || !collector.live then orphans.probes(unit, size)
    else collector.probes(unit, size)

  // Opens a collector for a test on the calling thread, and returns what must be passed to
  // `exit` on the same thread when the test is over.
  def enter(test: String): Collector | Null =
    val previous: Collector | Null = current.get()
    if active then current.set(Collector(test))
    previous

  def exit(previous: Collector | Null): Unit =
    val collector: Collector | Null = current.get()

    if collector != null && collector.ne(previous) then
      collector.close()
      val reached: Array[Array[Boolean]] | Null = collector.snapshot()
      if reached != null then record(collector.test, reached)

    current.set(previous)

  // What each test has reached since the last call, by the name its collector was opened with:
  // one array of probes for each unit, by the unit's index, which is empty for a unit the test
  // did not enter. The empty name holds what was reached outside any test. A test run more than
  // once (a cell of a spread, say) has the union.
  def drain(): java.util.Map[String, Array[Array[Boolean]]] = finished.synchronized:
    val result: java.util.HashMap[String, Array[Array[Boolean]]] = new java.util.HashMap(finished)
    finished.clear()
    val reached: Array[Array[Boolean]] | Null = orphans.snapshot()
    orphans.reset()
    if reached != null then result.put("", reached)
    result

  private def record(test: String, reached: Array[Array[Boolean]]): Unit = finished.synchronized:
    val earlier: Array[Array[Boolean]] | Null = finished.get(test)
    finished.put(test, if earlier == null then reached else union(earlier, reached))

  private def union(left: Array[Array[Boolean]], right: Array[Array[Boolean]])
  :   Array[Array[Boolean]] =

    val result: Array[Array[Boolean]] =
      new Array[Array[Boolean]](math.max(left.length, right.length))

    var unit: Int = 0

    while unit < result.length do
      val one: Array[Boolean] = if unit < left.length then left(unit) else new Array[Boolean](0)
      val other: Array[Boolean] = if unit < right.length then right(unit) else new Array[Boolean](0)
      val merged: Array[Boolean] = new Array[Boolean](math.max(one.length, other.length))
      var probe: Int = 0

      while probe < merged.length do
        merged(probe) = (probe < one.length && one(probe)) || (probe < other.length && other(probe))
        probe += 1

      result(unit) = merged
      unit += 1

    result
