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

import probably.*
import gossamer.*
import vacuous.*
import rudiments.*
import spectacular.*
import parasite.*
import superlunary.*
import contingency.*, errorHandlers.throwUnsafely
import inimitable.*
import nettlesome.*
import jacinta.*, jsonPrinters.minimal
import anticipation.*
import hieroglyph.*, charEncoders.utf8

object Tests extends Suite(t"Coaxial tests"):
  def run(): Unit = unsafely:

    val u = udp"3876".json.show
    println(Json.parse(u).as[UdpPort])

    supervise:
      val async = Async:
        val udpServer = external[UdpPort, Text]('{ port =>
          unsafely:
            supervise:
              val promise: Promise[Text] = Promise()
              val server = port.listen[Text]: in =>
                UdpResponse.Reply(jvmInstanceId.show.sysBytes).also(promise.fulfill(in.data.uString))
              
              promise.await()
        })

        test(t"Test UDP server"):
          udpServer(udp"3962")
        .assert()

      
      test(t"Send UDP messages until port opens"):
        Thread.sleep(5000)
        println("transmitting")
        udp"3962".transmit(jvmInstanceId.show)
      .assert()
      
      async.await()