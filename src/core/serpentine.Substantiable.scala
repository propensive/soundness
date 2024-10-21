package serpentine

trait Substantiable:
  type Self
  def exists(value: Self): Boolean
