package serpentine

export Serpentine.{?, ^, /}

extension [ValueType](value: ValueType)(using substantiable: ValueType is Substantiable)
  def exists(): Boolean = substantiable.exists(value)
