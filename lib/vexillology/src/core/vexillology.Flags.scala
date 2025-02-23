package vexillology

import java.lang as jl

import distillate.*

object Vexillology:
  opaque type Flags[EnumType] = Long

  object Flags:
    def apply[EnumType](): Flags[EnumType] = 0L

  extension [EnumType: Enumerable](flags: Flags[EnumType])
    def apply(value: EnumType & Singleton): Boolean =
      (flags & (1L << EnumType.index(value))) > 0

    def update(value: EnumType & Singleton, enabled: Boolean): Flags[EnumType] =
      if enabled then flags | (1L << EnumType.index(value))
      else flags & ~(1L << EnumType.index(value))

    def set(using enumerable: EnumType is Enumerable): Set[EnumType] =
      var value: Long = flags
      var set: Set[EnumType] = Set()

      while (value != 0) do
        val position = jl.Long.numberOfTrailingZeros(value)
        set += enumerable.values(position)
        value &= value - 1

      set

export Vexillology.Flags
