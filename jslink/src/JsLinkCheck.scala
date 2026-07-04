package jslink

import soundness.*

// A tiny Scala.js entry point exercising a representative slice of JS-capable
// modules so `jslink.fastLinkJS` forces their code reachable; the linker then
// reports any reference to a `java.*`/`javax.*` class Scala.js lacks. This is the
// runtime (link-level) counterpart to the static reference audit.
object Main:
  def main(args: Array[String]): Unit =
    val text: Text = t"hello, ${args.length} args"
    val json: Json = unsafely(t"""{ "values": [1, 2, 3] }""".decode[Json])
    java.lang.System.out.nn.println(text.s+json.toString)
