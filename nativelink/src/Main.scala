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
