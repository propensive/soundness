package rudiments

import language.experimental.captureChecking

object ExitStatus:
  def apply(value: Int): ExitStatus = if value == 0 then Ok else Fail(value)

enum ExitStatus:
  case Ok
  case Fail(status: Int)

  def apply(): Int = this match
    case Ok           => 0
    case Fail(status) => status

case class Pid(value: Long):
  override def toString(): String = "PID:"+value
