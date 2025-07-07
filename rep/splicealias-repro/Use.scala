package repro

import language.experimental.captureChecking

object Use:
  val tc: TC { type Self = String } = Macro.make[String]
  def go(): String = tc.get
