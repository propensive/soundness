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
package turbulence

import language.experimental.pureFunctions

import java.util.concurrent as juc

import anticipation.*
import denominative.*
import parasite.*
import rudiments.*

import probates.awaitProbate

case class Multiplexer[key, element]()(using Monitor):
  private case class Removal(key: key)

  private val active: TrieMap[key, Unit] = TrieMap()
  private val queue: juc.LinkedBlockingQueue[element | Removal] = juc.LinkedBlockingQueue()

  def close(): Unit = active.keys.each(remove(_))

  @tailrec
  private[turbulence] final def pump(key: key, stream: LazyList[element])(using Worker): Unit =
    if stream.nil then remove(key) else
      relent()
      queue.put(stream.head)
      pump(key, stream.tail)

  // Each source is drained by its own task — a child of the multiplexer's surrounding `Monitor`, so
  // it is supervised and cleaned up when that scope ends rather than escaping as a daemon. If a
  // source fails (its stream throws), the key is removed — so the consumer sees that source end
  // rather than blocking forever on a `Removal` the failed pump never enqueued — isolating the
  // failure to that source rather than letting it escape.
  def add(key: key, stream: LazyList[element]): Unit =
    active(key) =
      contain:
        case _ => remove(key); Remedy.Accept

    // The whole body is built here and crosses to the fiber as a neutral thunk:
    // an async body may not capture the enclosing instance, and even a cast to
    // `Multiplexer[key, element]` inside the fiber would re-charge `this`
    // through the class type parameters' prefix.
    val body: AnyRef =
      ((worker: AnyRef) =>
          try pump(key, stream)(using worker.asInstanceOf[Worker^])
          catch case _: Exception => remove(key))
      . asInstanceOf[AnyRef]

    async:
      body.asInstanceOf[AnyRef => Unit](summon[Worker].asInstanceOf[AnyRef])

  private[turbulence] def remove(key: key): Unit = if !active.nil then queue.put(Removal(key))

  def stream: LazyList[element] = queue.take().nn.absolve match
    case Removal(key) =>
      active -= key
      if active.nil then LazyList() else stream

    case value =>
      value.asInstanceOf[element] #:: stream
