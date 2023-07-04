package anticipation

trait SimpleShow[-ValueType]:
  def show(value: ValueType): String
