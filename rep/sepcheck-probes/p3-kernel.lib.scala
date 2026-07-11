// P3 lib: a sepcheck-enabled "kernel" unit defining a Mutable type with update methods.
// Compiled separately; consumers link against it like a downstream module against zephyrine.
package sepprobe

import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Kernel extends Mutable:
  private var value: Int = 0
  def get: Int = value
  update def bump(): Unit = value += 1
