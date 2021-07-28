package simplistic

trait CssSelection[-T]:
  def selection(value: T): String