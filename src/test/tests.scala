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
          udp"3962".listen: in =>
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
