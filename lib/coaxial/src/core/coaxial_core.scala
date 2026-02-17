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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package coaxial

import anticipation.*
import contingency.*
import parasite.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

import Control.*

extension [bindable: Bindable](socket: bindable)
  def listen[input](using Monitor, Codicil)[result](lambda: bindable.Input => bindable.Output)
  :   SocketService raises BindError =

      val binding = bindable.bind(socket)

      val bindLoop = loop:
        val connection = bindable.connect(binding)
        bindable.transmit(binding, connection, lambda(connection))

      val task = async(bindLoop.run())

      new SocketService:
        def stop(): Unit =
          bindLoop.stop()
          bindable.stop(binding)
          safely(task.await())


extension [endpoint: Serviceable as serviceable](endpoint: endpoint)
  def transmit[message: Transmissible](input: message): Stream[Data] =
    val connection = serviceable.connect(endpoint)

    serviceable.transmit(connection, message.serialize(input))
    serviceable.receive(connection)


  def exchange[state](initialState: state)[message: Ingressive](initialMessage: message = Data())
    ( handle: (state: state) ?=> message => Control[state] )
  :   state =

      val connection = serviceable.connect(endpoint)

      def recur(input: Stream[Data], state: state): state = input.flow(state):
        handle(using state)(message.deserialize(head)) match
          case Continue(state2) => recur(tail, state2.or(state))
          case Terminate        => state

          case Reply(message, state2) =>
            serviceable.transmit(connection, Stream(message))
            recur(tail, state2.or(state))

          case Conclude(message, state2) =>
            serviceable.transmit(connection, Stream(message))
            state2.or(state)

      recur(serviceable.receive(connection), initialState).also(serviceable.close(connection))


extension [endpoint: Routable as routable](endpoint: endpoint)
  def transmit[transmissible: Transmissible](message: transmissible)(using Monitor)
  :   Unit raises StreamError =

      routable.transmit(routable.connect(endpoint), transmissible.serialize(message))
