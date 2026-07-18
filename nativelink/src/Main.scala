// A native application exercising a representative slice of the Native-capable modules, so that
// `soundness.nativelink.binary` LINKS them through the Scala Native `tools` linker — turning any
// reachable reference to an unimplemented `java.*` API into a link-time error (the native
// counterpart of `jslink`). It also runs a real C foreign call lowered by xenophile's `NativeInvoke`.
package nativelink

import soundness.*
import prepositional.*

object Main:
  // The libc definitions, declared with the hellenism-free string-path `Interface` (hellenism's
  // `cp"…"` does not cross-compile to native). `getpid` is a nullary C function returning `int`.
  given libc: (Interface in Native at "/nativelink/libc.h") =
    Interface[Native]("/nativelink/libc.h")

  def main(args: Array[String]): Unit =
    val out = java.lang.System.out.nn

    // vacuous
    val present: Optional[Int] = 34
    val absent: Optional[Int] = Unset
    out.println((present.or(0) + absent.or(8)).toString)

    // gossamer
    out.println(t"hello, ${args.length} args".s)

    // hypotenuse
    val n: U64 = 42
    out.println(n.toString)

    // spectacular
    out.println(43.show.s)

    // aviation
    out.println(calendars.gregorianCalendar.daysInYear(Year(2000)).toString)

    // xenophile native C FFI: real libc calls lowered by `NativeInvoke` — a nullary call and
    // (v2) calls with primitive arguments.
    val pid: Int = Foreign["library", Native].getpid().invoke[Int]
    out.println("pid via native FFI: "+pid)

    val absolute: Int = Foreign["library", Native].abs(-7).invoke[Int]
    out.println("abs(-7) via native FFI: "+absolute)

    val power: Double = Foreign["library", Native].pow(2.0, 10.0).invoke[Double]
    out.println("pow(2, 10) via native FFI: "+power)

    // (v3) a string argument marshalled to a `CString`, and a string result read back.
    val length: Long = Foreign["library", Native].strlen(t"hello, world").invoke[Long]
    out.println("strlen(hello, world) via native FFI: "+length)

    val home: Text = Foreign["library", Native].getenv(t"HOME").invoke[Text]
    out.println("getenv(HOME) via native FFI: "+home.s)

    // galilei: a real filesystem round-trip driven straight through the native `FilesystemBackend`
    // (Scala Native javalib) — create a directory, observe it, delete it, observe it gone.
    import galilei.filesystemBackends.native
    val fs = summon[FilesystemBackend on Linux]
    val dir: Path on Linux = unsafely((% / "var" / "tmp" / "soundness-native-fs").on[Linux])
    if fs.exists(dir, false) then unsafely(fs.delete(dir))
    out.println("fs: exists before mkdir = "+fs.exists(dir, false))
    unsafely(fs.createDirectory(dir))
    out.println("fs: exists after mkdir  = "+fs.exists(dir, false))
    unsafely(fs.delete(dir))
    out.println("fs: exists after delete = "+fs.exists(dir, false))

    // Write a file through the backend's `open`, then read it back — real file-content I/O.
    import charEncoders.utf8Encoder
    val file: Path on Linux = unsafely((% / "var" / "tmp" / "soundness-native-file").on[Linux])
    if fs.exists(file, false) then unsafely(fs.delete(file))
    unsafely:
      fs.open(file, List(OpenFlag.Write, OpenFlag.Create)): handle =>
        handle.writer(LazyList(t"hello native fs".in[Data]))
    out.println("fs: file size after write = "+unsafely(fs.stat(file, false)).size)
    val readBytes = unsafely(fs.open(file, List(OpenFlag.Read))(_.reader().map(_.length).sum))
    out.println("fs: bytes read back       = "+readBytes)
    unsafely(fs.delete(file))

    // ambience: read an environment variable through the core `Environment` backend, which uses
    // `System.getenv` — SN-supported, so it works on native with no separate backend (the same is
    // true of turbulence's stdio and capricious's randomness: their core givens use SN-supported
    // `System.*`/`java.util.Random` APIs).
    import environments.javaEnvironment
    out.println("ambience HOME = "+summon[Environment].variable(t"HOME").or(t"?").s)

    // coaxial: a UDP loopback round-trip straight through the native `SocketBackend` — bind a
    // datagram socket, dispatch a payload to it, receive it back.
    import coaxial.socketBackends.native
    val sb = summon[SocketBackend]
    val udpPort = Port.unsafe[Udp](55555)
    val server = sb.listenUdp(udpPort, Unset, Nil)
    val courier = sb.routeUdpPort(udpPort, Unset, Nil)
    val payload: Data = t"ping".in[Data]
    unsafely(sb.dispatch(courier, summon[Data is Streamable by Data over Credit].stream(payload)))
    val packet = unsafely(sb.receive(server))
    sb.unbind(server)
    out.println("udp: received "+packet.data.length+" bytes = "+packet.data.to(List).map(_.toChar).mkString)

    // ...and a TCP loopback round-trip: listen, dial back into the listener, write the payload and
    // hang up (so the server-side read sees EOF), then accept and read it back through the duplex.
    // The kernel's listen backlog completes the handshake and buffers the payload, so the
    // single-threaded dial → write → accept → read sequence never blocks.
    val tcpPort = Port.unsafe[Tcp](55556)
    val tcpServer = sb.listenTcp(tcpPort, Unset, Nil)
    val client = sb.dialTcpPort(tcpPort, Unset, Nil)
    sb.request(client, summon[Data is Streamable by Data over Credit].stream(payload))
    sb.hangUp(client)
    val duplex = unsafely(sb.accept(tcpServer))
    val received = unsafely(duplex.source.memoize)
    duplex.close()
    sb.shutdown(tcpServer)
    out.println("tcp: received "+received.length+" bytes = "+received.to(List).map(_.toChar).mkString)

    // coaxial TLS: a real HTTPS exchange through the OpenSSL-backed `SecureEndpoint` — the one
    // networked scenario, so gated on `SOUNDNESS_CI_ONLINE` like wasm-e2e's outgoing-HTTP check.
    if java.lang.System.getenv("SOUNDNESS_CI_ONLINE") == "1" then
      import internetAccess.online
      val secure = coaxial.SecureEndpoint(t"example.com", 443)
      val tlsDuplex = summon[coaxial.SecureEndpoint is Connectable].connect(secure, Unset)
      val request = t"HEAD / HTTP/1.0\r\nHost: example.com\r\nConnection: close\r\n\r\n".in[Data]
      tlsDuplex.send(summon[Data is Streamable by Data over Credit].stream(request))
      val response = unsafely(tlsDuplex.source.memoize)
      tlsDuplex.close()
      val status = response.to(List).takeWhile(_ != 13).map(_.toChar).mkString
      out.println("tls: "+status+" ("+response.length+" bytes)")
