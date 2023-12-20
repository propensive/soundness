/*
    Coaxial, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import perforate.*, errorHandlers.throwUnsafely
import inimitable.*
import nettlesome.*


object Tests extends Suite(t"Coaxial tests"):
  def run(): Unit = supervise:
    val async = Async:
      val udpServer = external[String, String]('{ response =>
        supervise:
          val promise: Promise[Unit] = Promise()
          coaxial.Socket.listen(udp"3962"): in =>
            UdpResponse.Reply(jvmInstanceId.show.sysBytes).also(promise.fulfill(()))
          promise.await()
          s"Running $response"
      })

      test(t"Test UDP server"):
        udpServer("server")
      .assert(_ == "Running server")

    test(t"One plus one"):
      1 + 1
    .assert(_ == 2)
  
    async.await()
