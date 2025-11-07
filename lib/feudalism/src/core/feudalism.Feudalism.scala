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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package feudalism

import fulminate.*
import proscenium.*
import rudiments.*

import java.util.concurrent.atomic as juca
import java.util.concurrent.locks as jucl

object Feudalism:
  import State.{Inactive, Reading, Writing}

  private enum State[value]:
    case Inactive(value: value)
    case Reading(value: value, waitingToWrite: Set[Thread], waitingToRead: Set[Thread])
    case Writing(value: value, waitingToWrite: Set[Thread], waitingToRead: Set[Thread])

    def release(): State[value] = this match
      case Inactive(value) => Inactive(value)

      case Reading(value, writers, readers) =>
        if writers.isEmpty then
          if readers.isEmpty then Inactive(value)
          else jucl.LockSupport.unpark(readers.head) yet Reading(value, writers, readers.tail)
        else jucl.LockSupport.unpark(writers.head) yet Reading(value, writers.tail, readers)

      case Writing(value, writers, readers) =>
        if writers.isEmpty then
          if readers.isEmpty then Inactive(value)
          else jucl.LockSupport.unpark(readers.head) yet Writing(value, writers, readers.tail)
        else jucl.LockSupport.unpark(writers.head) yet Writing(value, writers.tail, readers)

    def read(thread: Thread): State[value] = this match
      case Inactive(value)                  => Reading(value, Set(), Set())
      case Reading(value, writers, readers) => Reading(value, writers, readers + thread)
      case Writing(value, writers, readers) => Writing(value, writers, readers + thread)

    def write(thread: Thread): State[value] = this match
      case Inactive(value)                  => Writing(value, Set(), Set())
      case Reading(value, writers, readers) => Reading(value, writers + thread, readers)
      case Writing(value, writers, readers) => Writing(value, writers + thread, readers)

  opaque type Mutex[value] = juca.AtomicReference[State[value]]

  object Mutex:
    def apply[value](value: value): Mutex[value] =
      juca.AtomicReference[State[value]](Inactive(value))

  extension [value](mutex: Mutex[value])
    def isolate[result](lambda: value => result): result =
      @tailrec
      def recur(): result = mutex.getAndUpdate(_.nn.write(Thread.currentThread.nn)) match
        case Inactive(value) => lambda(value)
        case _               => jucl.LockSupport.park(mutex) yet recur()

      recur().also:
        mutex.getAndUpdate:
          case state@Writing(_, _, _) => state.release()
          case _                      => panic(m"Status should be Writing")

    def use[result](lambda: value => result): result =
      @tailrec
      def recur(): result = mutex.getAndUpdate(_.nn.read(Thread.currentThread.nn)).nn match
        case Inactive(value)                  => lambda(value)
        case Reading(value, writers, readers) => lambda(value)
        case Writing(value, writers, readers) => jucl.LockSupport.park(mutex) yet recur()

      recur().also:
        mutex.getAndUpdate:
          case state@Reading(_, _, _) => state.release()
          case _                      => panic(m"Expected status to be Reading")

    def replace(lambda: value => value): value =
      @tailrec
      def recur(): value = mutex.getAndUpdate(_.nn.write(Thread.currentThread.nn)) match
        case Inactive(value) => lambda(value)
        case _               => jucl.LockSupport.park(mutex) yet recur()

      recur().tap: value2 =>
        mutex.getAndUpdate:
          case state@Writing(_, _, _) => Inactive(value2)
          case _                      => panic(m"Status should be Writing")

    def update(block: => value): value =
      @tailrec
      def recur(): value = mutex.getAndUpdate(_.nn.write(Thread.currentThread.nn)) match
        case Inactive(_) => block
        case _           => jucl.LockSupport.park(mutex) yet recur()

      recur().tap: value2 =>
        mutex.getAndUpdate:
          case state@Writing(_, _, _) => Inactive(value2)
          case _                      => panic(m"Status should be Writing")

export Feudalism.Mutex
