package jslink

import soundness.*

import charEncoders.utf8Encoder

// A Scala.js entry point that exercises a representative slice of the JS-capable
// modules, so `jslink.fastLinkJS` forces their code reachable and the linker
// reports any reachable reference to a `java.*`/`javax.*` class Scala.js lacks —
// the runtime (link-level) counterpart to the static reference audit.
// The TypeScript definitions reused from xenophile's test resources; the `Interface` given points
// the `Foreign` navigation at them so `foo.ping()` type-checks against the `ping(): string` member.
// Declared with the hellenism-free string-literal overload, so hellenism need not join the link.
type TsDefs = Interface in Typescript at "/xenophile/definitions.ts"
given tsDefs: TsDefs = Interface[Typescript]("/xenophile/definitions.ts")

object Main:
  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit =
    val out = java.lang.System.out.nn

    // xenophile (JS materializer): `Foreign["Foo", Typescript].ping().call[Text]()` expands to a
    // real Scala.js dynamic call — `js.Dynamic.global.selectDynamic("Foo").applyDynamic("ping")()` —
    // decoded through the `Text` boundary codec. Linking it proves the emitted interop is valid.
    // The chain must be inline (not bound to a `val`), exactly like the `Wasm` `call`.
    out.println(Foreign["Foo", Typescript].ping().call[Text]().s)

    // gossamer (Text) + kaleidoscope (regex, via interpolation/text ops)
    out.println(t"hello, ${args.length} args".s)

    // jacinta (JSON parse) + wisteria (derived decoder into a case class)
    val point: Point = unsafely(t"""{ "x": 1, "y": 2 }""".read[Json].as[Point])
    out.println(point.x.toString)

    // ypsiloid (YAML parse — exercises the pooled-parser path)
    val yaml = unsafely(t"x: 1\ny: 2\n".read[Yaml])
    out.println(yaml.toString)

    // hypotenuse (numeric / bit types)
    val n: U64 = 42
    out.println(n.toString)

    // spectacular (Show)
    out.println(43.show.s)

    // aviation (calendar arithmetic — the SAM-fixed givens)
    out.println(calendars.gregorianCalendar.daysInYear(Year(2000)).toString)

    // parasite (the eager single-threaded JavaScript threading model, #1450): forking, gathering
    // and awaiting must link without reaching any JVM threading primitive.
    locally:
      import threading.javascriptThreading
      import probates.cancelProbate
      val gathered = unsafely(supervise(List(async(6), async(7)).sequence.await()))
      out.println(gathered.toString)
