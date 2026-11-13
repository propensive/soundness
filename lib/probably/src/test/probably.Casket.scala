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

// What the coverage tests instrument: a class with one of every kind of place a probe goes.
// It is written with the JDK's types alone, so that everything in it is its own code and none
// of it an inline method's expansion, which gets no probes.
//
// `Lottery` is what a test calls it through: the recompiled class is another class of the same
// name, loaded by another classloader, and the interface is the one thing the two share.
trait Lottery:
  def choose(value: Int): String
  def sum(limit: Int): Int
  def risky(value: Int): Int
  def delegate(value: Int): Unit
  def defer(value: Int, gate: juc.CountDownLatch, done: juc.CountDownLatch): Unit

class Casket(threshold: Int) extends Lottery:
  val metal: String = if threshold > 2 then "gold" else "lead"

  def choose(value: Int): String =
    if value > threshold && value%2 == 0 then "even" else
      value match
        case 1 => "one"
        case 3 => "three"
        case _ => "other"

  def sum(limit: Int): Int =
    var total: Int = 0
    var index: Int = 0

    while index < limit do
      total += index
      index += 1

    total

  def risky(value: Int): Int = try 10/value catch case _: ArithmeticException => -1

  // Chooses on a thread of its own, which is over before this returns.
  def delegate(value: Int): Unit =
    val task: Runnable = () => choose(value)
    val thread: Thread = new Thread(task)
    thread.start()
    thread.join()

  // Chooses on a thread of its own, once `gate` opens, and says so through `done`.
  def defer(value: Int, gate: juc.CountDownLatch, done: juc.CountDownLatch): Unit =
    val task: Runnable = () =>
      gate.await()
      choose(value)
      done.countDown()

    new Thread(task).start()
