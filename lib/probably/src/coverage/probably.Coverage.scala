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

import java.util.concurrent as juc

// The state which coverage-instrumented classes write to, and a host reads: how many times each
// test reached each probe. A class is instrumented by `probably.Instrumenter`, which recompiles
// it from its TASTy with an increment at every method entry and branch; this object decides
// which counter.
//
// Each test, while it runs, has a `Collector`, and each thread working for it a `Tally` of its
// own, held in an `InheritableThreadLocal`: `enter` and `exit` (called by `Runner`) bracket the
// test body on the thread which runs it, and a thread started within the bracket gets a fresh
// tally for the same collector, as does any thread that thread starts. So a test is credited
// with what the threads it owns do. A thread which outlives its test, or one which was started
// before it (a pool's), has no open collector, and what it reaches is credited to no test: it is
// recorded against the empty name.
//
// Instrumented code asks for its unit's counters once per method, with `probes`, and then
// increments them directly, so the cost of a probe is that of an array increment, and the cost
// of entering a method is a thread-local read. Only the thread a tally belongs to writes to it,
// so no increment is lost; a sum is read when the test ends, and is exact for its own thread
// and for every thread it has joined. What a thread it started and left running adds after
// that, it may or may not be credited with.
//
// Everything here is written against the JDK alone, and its signatures name only JDK types:
// instrumented code may be any class at all — one this library's own dependencies are made of —
// and the host reads the results reflectively, across a classloader boundary.
object Coverage:
  private type Units = Array[Array[Long] | Null]

  // One thread's counters for one collector, which only that thread writes.
  final class Tally private[Coverage] (val collector: Collector):
    @volatile private var units: Units = new Array[Array[Long] | Null](16)
    @volatile private var owner: Thread | Null = null

    private[Coverage] def probes(unit: Int, size: Int): Array[Long] =
      val current: Units = units
      val found: Array[Long] | Null = if unit < current.length then current(unit) else null
      if found == null then allocate(unit, size) else found

    // Only ever called by the owning thread, which the first call records.
    private def allocate(unit: Int, size: Int): Array[Long] =
      if owner == null then owner = Thread.currentThread()
      val current: Units = units

      val sized: Units =
        if unit < current.length then current else
          val grown: Units = new Array[Array[Long] | Null](math.max(unit + 1, current.length*2))
          System.arraycopy(current, 0, grown, 0, current.length)
          grown

      val fresh: Array[Long] = new Array[Long](size)
      sized(unit) = fresh
      // The volatile write publishes the new array, and the element stored into it.
      units = sized
      fresh

    // Whether the thread which wrote this has finished, so that it will write no more.
    private[Coverage] def retired: Boolean =
      val thread: Thread | Null = owner
      thread != null && !thread.isAlive

    private[Coverage] def addTo(total: Totals): Unit = total.add(units)

  // A sum of counters, grown to fit whatever is added to it.
  private final class Totals:
    var units: Array[Array[Long]] = new Array[Array[Long]](0)

    def add(more: Units | Array[Array[Long]]): Unit =
      val adding: Units = more.asInstanceOf[Units]

      if adding.length > units.length then
        val grown: Array[Array[Long]] = new Array[Array[Long]](adding.length)
        System.arraycopy(units, 0, grown, 0, units.length)
        var unit: Int = units.length

        while unit < grown.length do
          grown(unit) = new Array[Long](0)
          unit += 1

        units = grown

      var unit: Int = 0

      while unit < adding.length do
        val counts: Array[Long] | Null = adding(unit)

        if counts != null then
          if counts.length > units(unit).length
          then units(unit) = java.util.Arrays.copyOf(units(unit), counts.length).nn

          val sum: Array[Long] = units(unit)
          var probe: Int = 0

          while probe < counts.length do
            sum(probe) += counts(probe)
            probe += 1

        unit += 1

    def subtract(earlier: Array[Array[Long]]): Unit =
      var unit: Int = 0

      while unit < earlier.length && unit < units.length do
        val counts: Array[Long] = earlier(unit)
        var probe: Int = 0

        while probe < counts.length && probe < units(unit).length do
          units(unit)(probe) -= counts(probe)
          probe += 1

        unit += 1

    def copy(): Array[Array[Long]] =
      val result: Array[Array[Long]] = new Array[Array[Long]](units.length)
      var unit: Int = 0

      while unit < units.length do
        result(unit) = java.util.Arrays.copyOf(units(unit), units(unit).length).nn
        unit += 1

      result

    // Whether any probe has been reached.
    def reached: Boolean =
      var unit: Int = 0
      var found: Boolean = false

      while !found && unit < units.length do
        var probe: Int = 0

        while !found && probe < units(unit).length do
          found = units(unit)(probe) != 0L
          probe += 1

        unit += 1

      found

  final class Collector private[Coverage] (val test: String):
    @volatile private var open: Boolean = true
    private val tallies: juc.ConcurrentLinkedQueue[Tally] = juc.ConcurrentLinkedQueue()

    // The sum of the tallies of threads which have finished, which need not be kept.
    private val finished: Totals = Totals()

    private[Coverage] def live: Boolean = open
    private[Coverage] def close(): Unit = open = false

    private[Coverage] def tally(): Tally =
      val fresh: Tally = Tally(this)
      tallies.add(fresh)
      fresh

    // The sum of every tally's counters so far.
    private[Coverage] def total(): Totals = synchronized:
      val iterator: java.util.Iterator[Tally] = tallies.iterator().nn

      while iterator.hasNext do
        val tally: Tally = iterator.next().nn

        if tally.retired then
          tally.addTo(finished)
          iterator.remove()

      val result: Totals = Totals()
      result.add(finished.units)
      tallies.forEach(_.nn.addTo(result))
      result

  @volatile private var active: Boolean = false
  private val orphans: Collector = Collector("")

  // What of the orphans' counts was last drained, to be subtracted from their next total.
  private var drained: Array[Array[Long]] = new Array[Array[Long]](0)

  private val current: InheritableThreadLocal[Tally | Null] =
    new InheritableThreadLocal[Tally | Null]():
      override def childValue(parent: Tally | Null): Tally | Null =
        if parent == null || !parent.collector.live then null else parent.collector.tally()

  private val orphan: ThreadLocal[Tally] = ThreadLocal.withInitial(() => orphans.tally()).nn

  private val finished: java.util.HashMap[String, Array[Array[Long]]] = new java.util.HashMap()

  // Called by the host before it runs anything: until then, `enter` does nothing, so that a run
  // without coverage pays nothing for it.
  def activate(): Unit = active = true
  def enabled: Boolean = active

  // The counters for a unit's probes, for the test which owns the calling thread. Called by
  // instrumented code, with the unit's index and its number of probes as constants.
  def probes(unit: Int, size: Int): Array[Long] =
    val tally: Tally | Null = current.get()

    if tally == null || !tally.collector.live then orphan.get().nn.probes(unit, size)
    else tally.probes(unit, size)

  // Opens a collector for a test on the calling thread, and returns what must be passed to
  // `exit` on the same thread when the test is over.
  def enter(test: String): Tally | Null =
    val previous: Tally | Null = current.get()
    if active then current.set(Collector(test).tally())
    previous

  def exit(previous: Tally | Null): Unit =
    val tally: Tally | Null = current.get()

    if tally != null && tally.ne(previous) then
      val collector: Collector = tally.collector
      collector.close()
      val total: Totals = collector.total()
      if total.reached then record(collector.test, total)

    current.set(previous)

  // How many times each test has reached each probe since the last call, by the name its
  // collector was opened with: one array of counts for each unit, by the unit's index, which is
  // empty for a unit the test did not enter. The empty name holds what was reached outside any
  // test. A test run more than once (a cell of a spread, say) has the sum.
  def drain(): java.util.Map[String, Array[Array[Long]]] = finished.synchronized:
    val result: java.util.HashMap[String, Array[Array[Long]]] = new java.util.HashMap(finished)
    finished.clear()
    val total: Totals = orphans.total()
    val sum: Array[Array[Long]] = total.copy()
    total.subtract(drained)
    drained = sum
    if total.reached then result.put("", total.units)
    result

  private def record(test: String, total: Totals): Unit = finished.synchronized:
    val earlier: Array[Array[Long]] | Null = finished.get(test)
    if earlier != null then total.add(earlier)
    finished.put(test, total.units)
