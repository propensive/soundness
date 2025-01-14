/*
    Feudalism, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package feudalism

import fulminate.*
import rudiments.*

import language.experimental.captureChecking

import java.util.concurrent.atomic as juca
import java.util.concurrent.locks as jucl

object Feudalism:
  import State.{Inactive, Reading, Writing}

  private enum State[ValueType]:
    case Inactive(value: ValueType)
    case Reading(value: ValueType, waitingToWrite: Set[Thread], waitingToRead: Set[Thread])
    case Writing(value: ValueType, waitingToWrite: Set[Thread], waitingToRead: Set[Thread])

    def release(): State[ValueType] = this match
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

    def read(thread: Thread): State[ValueType] = this match
      case Inactive(value)                  => Reading(value, Set(), Set())
      case Reading(value, writers, readers) => Reading(value, writers, readers + thread)
      case Writing(value, writers, readers) => Writing(value, writers, readers + thread)

    def write(thread: Thread): State[ValueType] = this match
      case Inactive(value)                  => Writing(value, Set(), Set())
      case Reading(value, writers, readers) => Reading(value, writers + thread, readers)
      case Writing(value, writers, readers) => Writing(value, writers + thread, readers)

  opaque type Mutex[ValueType] = juca.AtomicReference[State[ValueType]]

  object Mutex:
    def apply[ValueType](value: ValueType): Mutex[ValueType] =
      juca.AtomicReference[State[ValueType]](Inactive(value))

  extension [ValueType](mutex: Mutex[ValueType])
    def isolate[ResultType](lambda: ValueType => ResultType): ResultType =
      @tailrec
      def recur(): ResultType = mutex.getAndUpdate(_.nn.write(Thread.currentThread.nn)) match
        case Inactive(value) => lambda(value)
        case _               => jucl.LockSupport.park(mutex) yet recur()

      recur().also:
        mutex.getAndUpdate:
          case state@Writing(_, _, _) => state.release()
          case _                      => panic(m"Status should be Writing")

    def use[ResultType](lambda: ValueType => ResultType): ResultType =
      @tailrec
      def recur(): ResultType = mutex.getAndUpdate(_.nn.read(Thread.currentThread.nn)).nn match
        case Inactive(value)                  => lambda(value)
        case Reading(value, writers, readers) => lambda(value)
        case Writing(value, writers, readers) => jucl.LockSupport.park(mutex) yet recur()

      recur().also:
        mutex.getAndUpdate:
          case state@Reading(_, _, _) => state.release()
          case _                      => panic(m"Expected status to be Reading")

    def replace(lambda: ValueType => ValueType): ValueType =
      @tailrec
      def recur(): ValueType = mutex.getAndUpdate(_.nn.write(Thread.currentThread.nn)) match
        case Inactive(value) => lambda(value)
        case _               => jucl.LockSupport.park(mutex) yet recur()

      recur().tap: value2 =>
        mutex.getAndUpdate:
          case state@Writing(_, _, _) => Inactive(value2)
          case _                      => panic(m"Status should be Writing")

    def update(block: => ValueType): ValueType =
      @tailrec
      def recur(): ValueType = mutex.getAndUpdate(_.nn.write(Thread.currentThread.nn)) match
        case Inactive(_) => block
        case _           => jucl.LockSupport.park(mutex) yet recur()

      recur().tap: value2 =>
        mutex.getAndUpdate:
          case state@Writing(_, _, _) => Inactive(value2)
          case _                      => panic(m"Status should be Writing")

export Feudalism.Mutex
