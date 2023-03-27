package plutocrat

import probably.*
import gossamer.*

object Tests extends Suite(t"Plutocrat tests"):
  def run(): Unit =
    test(t"Show a monetary value"):
      val amount = Eur(3.01)
      t"Received $amount"
    .assert(_ == t"Received €3.01")

