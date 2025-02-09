/*
    Coaxial, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

import Control.*

extension [SocketType: Bindable as bindable](socket: SocketType)
  def listen[InputType](using Monitor, Codicil)[ResultType]
     (lambda: bindable.Input => bindable.Output)
  :     SocketService raises BindError =

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

extension [EndpointType: Serviceable as serviceable](endpoint: EndpointType)
  def request[MessageType: Transmissible](input: MessageType)
  :     Stream[Bytes] =
    val connection = serviceable.connect(endpoint)

    serviceable.transmit(connection, MessageType.serialize(input))
    serviceable.receive(connection)//.tap(serviceable.close(connection))

  def exchange[StateType](initialState: StateType)[MessageType: Ingressive]
     (initialMessage: MessageType = Bytes())
     (handle: (state: StateType) ?=> MessageType => Control[StateType])
  :     StateType =

    val connection = serviceable.connect(endpoint)

    def recur(input: Stream[Bytes], state: StateType): StateType = input.flow(state):
      handle(using state)(MessageType.deserialize(head)) match
        case Continue(state2) => recur(tail, state2.or(state))
        case Terminate        => state

        case Reply(message, state2) =>
          serviceable.transmit(connection, Stream(message))
          recur(tail, state2.or(state))

        case Conclude(message, state2) =>
          serviceable.transmit(connection, Stream(message))
          state2.or(state)

    recur(serviceable.receive(connection), initialState).also(serviceable.close(connection))

extension [EndpointType: Addressable as addressable](endpoint: EndpointType)
  def transmit[MessageType: Transmissible](message: MessageType)(using Monitor)
  :     Unit raises StreamError =

    addressable.transmit(addressable.connect(endpoint), MessageType.serialize(message))
