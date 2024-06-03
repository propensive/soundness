package anticipation

trait GenericLogger:
  def fine(message: => into Text): Unit
  def info(message: => into Text): Unit
  def warn(message: => into Text): Unit
  def fail(message: => into Text): Unit
