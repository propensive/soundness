// P6: under separation checking, mutable fields may only live in Stateful classes.
// Calibrates how much of a file can opt in without re-parenting its classes.
//EXPECT: error
import language.experimental.captureChecking
import language.experimental.separationChecking

class Plain:
  private var count: Int = 0
  def bump(): Unit = count += 1
