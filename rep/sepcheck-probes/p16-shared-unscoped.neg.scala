// P16: the classifier lattice has no place for a capability that is both shared and
// level-exempt. `caps.Unscoped extends ExclusiveCapability`, so a class extending both
// `SharedCapability` and `Unscoped` "inherits two unrelated classifier traits" and is rejected
// (3.9.1-dev-p17; earlier toolchains computed the least classifier as `Nothing` and said
// nothing). Both halves are wanted at once by contingency's ambient tactics — shared, so that
// aliasing one tactic is not a separation overlap (p15), and Unscoped, so that a use-site
// mint can flow into a `raises` result (rep/DECISIONS.md "capturing-raises SOLVED at source").
// The second expectation shows the retained strategy is still tracked when the classifier
// clash is ignored. The fork's `SharedUnscoped` classifier (rep/shared-unscoped) is the fix.
//FLAGS: -language:experimental.separationChecking
//EXPECT: inherits two unrelated classifier traits
//EXPECT: cannot flow into capture set \{\}
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.SharedCapability:
  def abort(error: E): Nothing

class Throwing[E] extends Tactic[E], caps.Unscoped:
  def abort(error: E): Nothing = throw new RuntimeException(error.toString)

def keep(): () -> Unit =
  val tactic = Throwing[String]()
  () => tactic.abort("retained by a pure closure")
