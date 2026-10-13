// The tutorial's tests stand alone rather than inside a Suite's run(): a place for them,
// declared to a runner whose report is never rendered, so an assertion can run without a
// test harness.
import soundness.*

given tutorial: (Testable of Impromptu) = Testable.detached
given runner: Runner[Report] = Runner()

runner.report.declare(tutorial)
