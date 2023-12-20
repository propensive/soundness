package coaxial

import probably.*
import gossamer.*
import vacuous.*
import rudiments.*
import spectacular.*
import parasite.*
import superlunary.*
import perforate.*
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
        val udpServer = external[UdpPort, String]('{ port =>
          unsafely:
            supervise:
              val out = port.toString
              val promise: Promise[Unit] = Promise()
              val server = port.listen: in =>
                UdpResponse.Reply(jvmInstanceId.show.sysBytes).also(promise.fulfill(()))
              
              promise.await()
              server.stop()
              s"Running $out"
        })

        test(t"Test UDP server"):
          udpServer(udp"3962")
        .assert(_ == "Running server")

      
      test(t"Send UDP messages until port opens"):
        Thread.sleep(5000)
        println("transmitting")
        udp"3962".transmit(jvmInstanceId.show)
      .assert()
      
      async.await()