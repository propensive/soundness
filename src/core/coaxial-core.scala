/*
    Coaxial, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package coaxial

import anticipation.*
import contingency.*
import parasite.*
import rudiments.*
import turbulence.*
import vacuous.*

import Control.*

extension [SocketType](socket: SocketType)
  def listen[InputType](using bindable: Bindable[SocketType])(using Monitor, Codicil)[ResultType]
     (lambda: bindable.Input => bindable.Output)
          : SocketService raises BindError =

    val binding = bindable.bind(socket)

    val bindLoop = loop:
      val connection = bindable.connect(binding)
      async(bindable.transmit(binding, connection, lambda(connection)))

    val task = async(bindLoop.run())

    new SocketService:
      def stop(): Unit =
        bindLoop.stop()
        bindable.stop(binding)
        safely(task.await())

extension [EndpointType](endpoint: EndpointType)
  def connect[StateType](initialState: StateType)[MessageType](initialMessage: MessageType = Bytes())
     (handle: (state: StateType) ?=> MessageType => Control[StateType])
     (using serviceable: Serviceable[EndpointType], receivable: Receivable[MessageType])
          : StateType =

    val connection = serviceable.connect(endpoint)

    def recur(input: LazyList[Bytes], state: StateType): StateType = input.flow(state):
      handle(using state)(receivable.deserialize(head)) match
        case Continue(state2) => recur(tail, state2.or(state))
        case Terminate        => state

        case Reply(message, state2) =>
          serviceable.transmit(connection, message)
          recur(tail, state2.or(state))

        case Conclude(message, state2) =>
          serviceable.transmit(connection, message)
          state2.or(state)

    recur(serviceable.receive(connection), initialState).also:
      serviceable.close(connection)

  def transmit[MessageType](message: MessageType)
     (using transmissible: Transmissible[MessageType], addressable: Addressable[EndpointType])
     (using Monitor)
          : Unit raises StreamError =

    addressable.transmit(addressable.connect(endpoint), transmissible.serialize(message))
