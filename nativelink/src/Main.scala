// A tiny native application exercising a representative slice of the Native-capable modules, so
// that `soundness.nativelink.binary` LINKS them through the Scala Native `tools` linker — turning
// any reachable reference to an unimplemented `java.*` API into a link-time error. The runtime
// counterpart to `soundness.native`, which only compiles to NIR. Grow the imports and body as more
// modules are enabled for the native axis.
package nativelink

import vacuous.*

object Main:
  def main(args: Array[String]): Unit =
    val present: Optional[Int] = 34
    val absent: Optional[Int] = Unset
    System.out.nn.println("soundness native: "+(present.or(0) + absent.or(8)))
