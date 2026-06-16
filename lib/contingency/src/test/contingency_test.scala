                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                                ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package contingency

import soundness.*
import contingency.strategies.throwUnsafely

import errorDiagnostics.stackTracesDiagnostics

case class VarargsError(arguments: Text*)(using Diagnostics) extends Error(m"varargs error")
case class VarargsError2(argument: Text, arguments: Text*)(using Diagnostics) extends Error(m"varargs error 2")

case class ErrorA(value: Int)(using Diagnostics) extends Error(m"error a: $value")
case class ErrorB(value: Int)(using Diagnostics) extends Error(m"error b: $value")
case class ErrorC(value: Int)(using Diagnostics) extends Error(m"error c: $value")

case class Accumulated(values: List[Int])(using Diagnostics)
extends Error(m"accumulated ${values.length} errors")

object Tests extends Suite(m"Contingency"):

  def action(): Unit raises VarargsError = ()

  def succeed(s: Text): Text raises ErrorA = s
  def succeedStr(s: String): String raises ErrorA = s

  def failA(n: Int): String raises ErrorA =
    raise(ErrorA(n))
    "fallback"

  def failB(n: Int): String raises ErrorB =
    raise(ErrorB(n))
    "fallback"

  def run(): Unit =
    suite(m"whereas / recover / mitigate / accrue"):
      test(m"Ensure varargs parameter is handled exhaustively"):
        demilitarize:
          whereas:
            case VarargsError(arguments) => ()
          . recover(action())

      . assert(_.nonEmpty)

      test(m"Varargs are handled safely"):
        whereas:
          case VarargsError(arguments*) => ()
        . recover(action())
      . assert()

      test(m"Recover substitutes a value for a raised error"):
        whereas:
          case ErrorA(n) => s"recovered:$n"
        . recover(failA(7))
      . assert(_ == "recovered:7")

      test(m"Recover passes through body value when no error"):
        whereas:
          case ErrorA(_) => "recovered"
        . recover(succeedStr("ok"))
      . assert(_ == "ok")

      test(m"Recover handles the correct case among alternatives"):
        whereas:
          case ErrorA(n) => s"a:$n"
          case ErrorB(n) => s"b:$n"
        . recover(failB(3))
      . assert(_ == "b:3")

      test(m"Mitigate transforms an inner error to an outer type"):
        whereas:
          case ErrorB(n) => n
        . recover:
            whereas:
              case ErrorA(n) => ErrorB(n + 100)
            . mitigate:
                raise(ErrorA(5))
                0
      . assert(_ == 105)

      test(m"Mitigation chain: A becomes B becomes C"):
        whereas:
          case ErrorC(n) => n
        . recover:
            whereas:
              case ErrorB(n) => ErrorC(n + 10)
            . mitigate:
                whereas:
                  case ErrorA(n) => ErrorB(n + 1)
                . mitigate:
                    raise(ErrorA(4))
                    0
      . assert(_ == 15)

      test(m"Accrue collects multiple raises into the outer recovery"):
        whereas:
          case Accumulated(values) => values
        . recover:
            whereas:
              case ErrorA(_) => ()
            . accrue(Accumulated(Nil)):
                (sum, ex) => ex match
                  case ErrorA(n) => Accumulated(sum.values :+ n)
                  case _         => sum
            . and:
                raise(ErrorA(1))
                raise(ErrorA(2))
                List.empty[Int]
      . assert(_ == List(1, 2))

      test(m"Accrue passes the body value through when nothing raised"):
        whereas:
          case Accumulated(_) => "error"
        . recover:
            whereas { case ErrorA(_) => () }.accrue(Accumulated(Nil))((sum, _) => sum):
              "clean"
      . assert(_ == "clean")

      test(m"whereas.recover should compile inside `inline def` (see #534)"):
        demilitarize:
          inline def inlineRecover: String =
            whereas:
              case ErrorA(n) => s"recovered:$n"
            . recover(failA(7))
          inlineRecover
      . aspire(_.isEmpty)

      test(m"whereas.mitigate should compile inside `inline def` (see #534)"):
        demilitarize:
          inline def inlineMitigate: Int =
            whereas:
              case ErrorB(n) => n
            . recover:
                whereas:
                  case ErrorA(n) => ErrorB(n + 100)
                . mitigate:
                    raise(ErrorA(5))
                    0
          inlineMitigate
      . aspire(_.isEmpty)

      test(m"whereas.recover works inside `inline def` via a non-inline helper"):
        def helperRecover: String =
          whereas:
            case ErrorA(n) => s"recovered:$n"
          . recover(failA(7))

        inline def inlineRecover: String = helperRecover
        inlineRecover
      . assert(_ == "recovered:7")

    suite(m"raise / abort / raises type"):
      test(m"Method with raises is callable under an in-scope Tactic"):
        whereas:
          case ErrorA(_) => t"unused"
        . recover(succeed(t"hello"))
      . assert(_ == t"hello")

      test(m"val without ascription gets the result type, not Tactic ?=> T"):
        val s = succeed(t"present")
        s == t"present"
      . assert(_ == true)

      test(m"abort terminates with Nothing return type"):
        def alwaysFail: Int raises ErrorA = abort(ErrorA(99))
        capture[ErrorA](alwaysFail).value
      . assert(_ == 99)

      test(m"raise in statement position returns Unit"):
        def raisedStatement: Unit raises ErrorA = raise(ErrorA(1))
        capture[ErrorA](raisedStatement).value
      . assert(_ == 1)

    suite(m"safely / unsafely"):
      test(m"safely returns the value when no error is raised"):
        safely(succeed(t"present"))
      . assert(_ == t"present")

      test(m"safely returns Unset on a raised error"):
        safely(failA(2))
      . assert(_ == Unset)

      test(m"unsafely yields the value when no error is raised"):
        unsafely(succeed(t"plain"))
      . assert(_ == t"plain")

      test(m"unsafely throws when an error is raised"):
        try { unsafely(failA(5)); "no-throw".tt } catch case _: Exception => t"threw"
      . assert(_ == t"threw")

    suite(m"capture / attempt / amalgamate"):
      test(m"capture returns the raised error"):
        capture[ErrorA](failA(11))
      . assert(_.value == 11)

      test(m"capture aborts via ExpectationError if body succeeds"):
        safely[ExpectationError[String]]:
          capture[ErrorA](succeedStr("clean"))
        . absent
      . assert(_ == true)

      test(m"attempt yields Success on a clean body"):
        attempt[ErrorA](succeed(t"x"))
      . assert(_ == Attempt.Success(t"x"))

      test(m"attempt yields Failure on a raised error"):
        attempt[ErrorA](failA(13)) match
          case Attempt.Failure(error) => error.value
          case _                      => -1
      . assert(_ == 13)

      test(m"amalgamate returns the body value when no error"):
        amalgamate[ErrorA](succeedStr("ok"))
      . assert(_ == "ok")

      test(m"amalgamate returns the error in a union when raised"):
        amalgamate[ErrorA](failA(21)) match
          case ErrorA(n) => n
          case other     => -1
      . assert(_ == 21)

      test(m"throwErrors runs the body under CanThrow scope"):
        import unsafeExceptions.canThrowAny

        try
          throwErrors[ErrorA](unsafely(failA(8)))
          "no-throw"
        catch case _: ErrorA => "threw"
      . assert(_ == "threw")

      test(m"certify is a no-op for ThrowTactic"):
        try
          certify[ErrorA]()
          "ok"
        catch case _: Exception => "threw"
      . assert(_ == "ok")

    suite(m"Attempt methods"):
      test(m"Attempt.Success.failure is false"):
        Attempt.Success(1).failure
      . assert(_ == false)

      test(m"Attempt.Failure.failure is true"):
        Attempt.Failure(ErrorA(1)).failure
      . assert(_ == true)

      test(m"Attempt.Success.success is true"):
        Attempt.Success(1).success
      . assert(_ == true)

      test(m"Attempt.Failure.success is false"):
        Attempt.Failure(ErrorA(1)).success
      . assert(_ == false)

      test(m"Attempt.map transforms Success value"):
        Attempt.Success[Int, ErrorA](5).map(_ * 2)
      . assert(_ == Attempt.Success(10))

      test(m"Attempt.map passes Failure through unchanged"):
        Attempt.Failure[Int, ErrorA](ErrorA(7)).map(_ * 2)
      . assert(_ == Attempt.Failure(ErrorA(7)))

      test(m"Attempt.handle transforms a failure exception"):
        Attempt.Failure[Int, ErrorA](ErrorA(3)).handle:
          case ErrorA(n) => ErrorB(n + 1)
        . match
          case Attempt.Failure(ErrorB(n)) => n
          case _                          => -1
      . assert(_ == 4)

      test(m"Attempt.handle leaves Success unchanged"):
        Attempt.Success[Int, ErrorA](9).handle { case _ => ErrorB(0) }
      . assert(_ == Attempt.Success(9))

      test(m"Attempt.acknowledge runs side-effect on Failure"):
        var seen: Int = -1
        Attempt.Failure[Int, ErrorA](ErrorA(4)).acknowledge { case ErrorA(n) => seen = n }
        seen
      . assert(_ == 4)

      test(m"Attempt.acknowledge is a no-op on Success"):
        var seen: Int = -1
        Attempt.Success[Int, ErrorA](6).acknowledge { case ErrorA(n) => seen = n }
        seen
      . assert(_ == -1)

      test(m"Attempt.recover handles Failure with handler value"):
        Attempt.Failure[Int, ErrorA](ErrorA(2)).recover { case ErrorA(n) => n * 10 }
      . assert(_ == 20)

      test(m"Attempt.recover returns Success value when present"):
        Attempt.Success[Int, ErrorA](5).recover { case _ => -1 }
      . assert(_ == 5)

      test(m"Attempt.apply yields value of Success"):
        Attempt.Success[Int, ErrorA](7).apply()
      . assert(_ == 7)

      test(m"Attempt.apply aborts on Failure"):
        capture[ErrorA](Attempt.Failure[Int, ErrorA](ErrorA(8)).apply()).value
      . assert(_ == 8)

    suite(m"Optional extensions"):
      test(m"lest returns the value when Optional is set"):
        val opt: Optional[Int] = 42
        opt.lest(ErrorA(0))
      . assert(_ == 42)

      test(m"lest aborts when Optional is Unset"):
        val opt: Optional[Int] = Unset
        capture[ErrorA](opt.lest(ErrorA(99))).value
      . assert(_ == 99)

      test(m"dare returns Unset when block raises"):
        val opt: Optional[Int] = 5
        opt.dare[ErrorA] { v => raise(ErrorA(v)); 0 }
      . assert(_ == Unset)

      test(m"dare returns the result when block succeeds"):
        val opt: Optional[Int] = 5
        opt.dare[ErrorA](v => v * 2)
      . assert(_ == 10)

      test(m"dare on Unset returns Unset"):
        val opt: Optional[Int] = Unset
        opt.dare[ErrorA](v => v * 2)
      . assert(_ == Unset)

      test(m"survive filters errors from a sequence"):
        List(1, 2, 3, 4).survive[Int]: n =>
          if n % 2 == 0 then n else raise(ErrorA(n)) yet 0
        . to(List)
      . assert(_ == List(2, 4))

    suite(m"defer / Deferred"):
      test(m"defer holds the unapplied body; apply() runs it under the in-scope tactic"):
        def failing(n: Int): String raises ErrorA = raise(ErrorA(n)) yet "fallback"

        whereas:
          case ErrorA(n) => s"recovered:$n"
        . recover:
            val held = contingency.defer(failing(7))
            held()
      . assert(_ == "recovered:7")

      test(m"defer re-evaluates the body on each apply() with the current tactic"):
        var counter = 0
        def increment(): Int raises ErrorA =
          counter += 1
          counter

        whereas:
          case ErrorA(_) => -1
        . recover:
            val held = contingency.defer(increment())
            held()
            held()
            held()
      . assert(_ == 3)

      test(m"defer of a plain value is identity-with-wrapping"):
        whereas:
          case ErrorA(n) => n
        . recover:
            contingency.defer(17)()
      . assert(_ == 17)

    suite(m"Foci / Pointer / track / validate / focus"):
      test(m"Pointer.Self has empty text"):
        Pointer.Self.text
      . assert(_ == t"")

      test(m"Pointer with single label encodes that label"):
        Pointer(t"x").text
      . assert(_ == t"x")

      test(m"Pointer chains labels with dots"):
        Pointer(t"a")(t"b")(t"c").text
      . assert(_ == t"a.b.c")

      test(m"Pointer.apply with no args is Self"):
        Pointer() == Pointer.Self
      . assert(_ == true)

      test(m"Foci.default has length 0"):
        summon[Foci[Pointer]].length
      . assert(_ == 0)

      test(m"Foci.default success is false (no recorded errors)"):
        summon[Foci[Pointer]].success
      . assert(_ == false)

      test(m"Foci.default tainted is false"):
        summon[Foci[Pointer]].tainted
      . assert(_ == false)

      test(m"Foci.default register is a no-op"):
        val foci = summon[Foci[Pointer]]
        foci.register(ErrorA(1))
        foci.length
      . assert(_ == 0)

      test(m"Foci.default fold returns initial"):
        summon[Foci[Pointer]].fold(t"start")((_, acc) => _ => acc)
      . assert(_ == t"start")

      test(m"TrackFoci records errors"):
        val foci = TrackFoci[Pointer]()
        foci.register(ErrorA(1))
        foci.register(ErrorA(2))
        foci.length
      . assert(_ == 2)

      test(m"TrackFoci success after registers is false"):
        val foci = TrackFoci[Pointer]()
        foci.register(ErrorA(1))
        foci.success
      . assert(_ == false)

      test(m"TrackFoci fold visits each error"):
        val foci = TrackFoci[Pointer]()
        foci.register(ErrorA(10))
        foci.register(ErrorA(20))
        foci.fold(0)((_, acc) => { case ErrorA(n) => acc + n })
      . assert(_ == 30)

      test(m"track collects accrued errors into accrual"):
        whereas:
          case Accumulated(values) => values
        . recover:
            track[Pointer](Accumulated(Nil)):
              case ErrorA(n) => Accumulated(accrual.values :+ n)
            . within:
                raise(ErrorA(1))
                raise(ErrorA(2))
                List.empty[Int]
      . assert(_ == List(1, 2))

      test(m"focus supplements registered errors with a focus"):
        val foci = TrackFoci[Pointer]()
        given Foci[Pointer] = foci
        focus[Pointer, Unit](prior.or(Pointer.Self)(t"field")):
          foci.register(ErrorA(0))
        foci.length
      . assert(_ == 1)

      test(m"validate yields the accrual when errors are raised"):
        val v: Accumulated = validate[Pointer](Accumulated(Nil)):
          case ErrorA(n) => Accumulated(accrual.values :+ n)
        . within:
            raise(ErrorA(7))
            raise(ErrorA(8))
        v.values
      . assert(_ == List(7, 8))

    suite(m"Errors / Validation / ExpectationError"):
      test(m"Errors with no entries communicates the count"):
        Errors().message.text.starts(t"0 accrued errors")
      . assert(_ == true)

      test(m"Errors with one entry includes focus and message"):
        val errs = Errors(t"field" -> ErrorA(5))
        errs(t"field").let { case e: Error => e.message.text.contains(t"error a") }.or(false)
      . assert(_ == true)

      test(m"Errors lookup returns Unset for absent focus"):
        val errs = Errors(t"x" -> ErrorA(1))
        errs(t"missing")
      . assert(_ == Unset)

      test(m"Errors + adds a focus/error pair"):
        val errs = Errors() + (t"field", ErrorA(7))

        errs(t"field").lay(-1):
          case e: ErrorA => e.value
      . assert(_ == 7)

      test(m"Validation with no messages renders 'no messages'"):
        Validation().text.text
      . assert(_ == "no messages")

      test(m"Validation with one message renders 'one message'"):
        (Validation() + (Pointer(t"f"), m"bad")).text.text.starts(t"one message")
      . assert(_ == true)

      test(m"Validation.apply returns Unset when pointer absent"):
        Validation()(Pointer(t"x"))
      . assert(_ == Unset)

      test(m"Validation.apply returns the message when present"):
        val v = Validation() + (Pointer(t"f"), m"bad")
        v(Pointer(t"f")).let { case m: Message => m.text }.or(t"")
      . assert(_ == t"bad")

      test(m"ExpectationError communicates the result type"):
        ExpectationError("anything").message.text.contains(t"expected to fail")
      . assert(_ == true)

    suite(m"Mitigable / Fatal / Unchecked / strategies"):
      test(m"Mitigable instance can transform via contramap"):
        import unsafeExceptions.canThrowAny
        given mitigable: ErrorA is Mitigable to ErrorB = a => ErrorB(a.value + 1)
        given outer: Tactic[ErrorB] = strategies.throwUnsafely
        import strategies.mitigation
        try { raise(ErrorA(40)); 0 }
        catch case e: ErrorB => e.value
      . assert(_ == 41)

      test(m"throwSafely requires CanThrow and throws on record"):
        import unsafeExceptions.canThrowAny
        val tactic: Tactic[ErrorA] = strategies.throwSafely[ErrorA, Any]
        try { tactic.record(ErrorA(55)); -1 }
        catch case e: ErrorA => e.value
      . assert(_ == 55)

      test(m"uncheckedErrors: erased Unchecked given enables raise → throw"):
        erased given uncheckedErrorA: ErrorA is Unchecked =
          new Unchecked { type Self = ErrorA }
        import strategies.uncheckedErrors
        try { raise(ErrorA(99)); -1 }
        catch case e: ErrorA => e.value
      . assert(_ == 99)


    suite(m"Tactic shape"):
      test(m"ThrowTactic.certify is a no-op"):
        import unsafeExceptions.canThrowAny
        val t = new ThrowTactic[ErrorA, Any]()
        t.certify()
        "ok"
      . assert(_ == "ok")

      test(m"ThrowTactic.record throws the error immediately"):
        import unsafeExceptions.canThrowAny
        val t = new ThrowTactic[ErrorA, Any]()
        try { t.record(ErrorA(2)); -1 } catch case e: ErrorA => e.value
      . assert(_ == 2)

      test(m"Tactic.contramap routes records through the transform"):
        import unsafeExceptions.canThrowAny
        val outer = ThrowTactic[ErrorB, Any]()
        val inner = outer.contramap[ErrorA](a => ErrorB(a.value + 100))
        try { inner.record(ErrorA(5)); -1 } catch case e: ErrorB => e.value
      . assert(_ == 105)

    suite(m"Whereas internals"):
      test(m"Whereas.Escape carries a value and has no stack trace"):
        val e = Whereas.Escape(t"payload")
        e.value == t"payload" && e.fillInStackTrace() == e
      . assert(_ == true)

      test(m"Whereas.AccrueTactic.changed is false before any record"):
        val acc = Whereas.AccrueTactic[ErrorA, Accumulated]
          (Accumulated(Nil), (sum, ex) => ex match
            case ErrorA(n) => Accumulated(sum.values :+ n)
            case _         => sum)
          (using errorDiagnostics.stackTracesDiagnostics)
        acc.changed
      . assert(_ == false)

      test(m"Whereas.AccrueTactic.accumulated reflects records"):
        val acc = Whereas.AccrueTactic[ErrorA, Accumulated]
          (Accumulated(Nil), (sum, ex) => ex match
            case ErrorA(n) => Accumulated(sum.values :+ n)
            case _         => sum)
          (using errorDiagnostics.stackTracesDiagnostics)
        acc.record(ErrorA(1))
        acc.record(ErrorA(2))
        acc.accumulated.values
      . assert(_ == List(1, 2))

    suite(m"Diagnostics (errorDiagnostics.emptyDiagnostics vs stackTraces)"):
      test(m"empty produces an error with no stack frames"):
        val err: ErrorA =
          given Diagnostics = errorDiagnostics.emptyDiagnostics
          ErrorA(1)
        err.getStackTrace.nn.length
      . assert(_ == 0)

      test(m"stackTraces produces an error with at least one stack frame"):
        val err: ErrorA =
          given Diagnostics = errorDiagnostics.stackTracesDiagnostics
          ErrorA(2)
        err.getStackTrace.nn.length > 0
      . assert(_ == true)

      test(m"empty: fillInStackTrace is a no-op (no-trace exception stays no-trace)"):
        given Diagnostics = errorDiagnostics.emptyDiagnostics
        val err = ErrorA(3)
        err.fillInStackTrace()
        err.getStackTrace.nn.length
      . assert(_ == 0)

      test(m"stackTraces: top frame originates in this file"):
        val err: ErrorA =
          given Diagnostics = errorDiagnostics.stackTracesDiagnostics
          ErrorA(4)
        err.getStackTrace.nn.head.nn.getFileName.nn
      . assert(_.contains("contingency_test"))

      test(m"Diagnostics.capture is distinct from Diagnostics.omit"):
        Diagnostics.capture != Diagnostics.omit
      . assert(_ == true)

      test(m"captured-trace error still serialises its message correctly"):
        val err: ErrorA =
          given Diagnostics = errorDiagnostics.stackTracesDiagnostics
          ErrorA(99)
        err.message.text
      . assert(_ == t"error a: 99")

      test(m"empty-trace error still serialises its message correctly"):
        val err: ErrorA =
          given Diagnostics = errorDiagnostics.emptyDiagnostics
          ErrorA(99)
        err.message.text
      . assert(_ == t"error a: 99")
