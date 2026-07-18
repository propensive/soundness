// A tiny native application exercising a representative slice of the Native-capable modules, so
// that `soundness.nativelink.binary` LINKS them through the Scala Native `tools` linker — turning
// any reachable reference to an unimplemented `java.*` API into a link-time error, and (via the
// xenophile call below) actually running a C foreign-function invocation lowered by `NativeInvoke`.
// The runtime counterpart to `soundness.native`, which only compiles to NIR.
package nativelink

import soundness.*
import prepositional.*

object Main:
  // The libc definitions, declared with the hellenism-free string-path `Interface` (hellenism's
  // `cp"…"` does not cross-compile to native). `getpid` is a nullary C function returning `int`.
  given libc: (Interface in Native at "/nativelink/libc.h") =
    Interface[Native]("/nativelink/libc.h")

  def main(args: Array[String]): Unit =
    val present: Optional[Int] = 34
    val absent: Optional[Int] = Unset
    System.out.nn.println("soundness native: "+(present.or(0) + absent.or(8)))

    // A real C foreign call, lowered by xenophile's `NativeInvoke` to a `dlsym`+`CFuncPtr` call.
    val pid: Int = Foreign["library", Native].getpid().invoke[Int]
    System.out.nn.println("pid via native FFI: "+pid)
