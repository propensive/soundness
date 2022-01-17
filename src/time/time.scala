package clairvoyant

import annotation.implicitNotFound

@implicitNotFound("an implicit Timekeeping instance is required to determine what type should be used to represent instants in time")
trait Timekeeping:
  type Type
  def from(long: Long): Type
  def to(value: Type): Long

object timekeeping:
  given long: Timekeeping with
    type Type = Long
    def from(long: Long): Long = long
    def to(value: Long): Long = value

  given date: Timekeeping with
    type Type = java.util.Date
    def from(long: Long): java.util.Date = java.util.Date(long)
    def to(value: java.util.Date): Long = value.getTime

def now()(using time: Timekeeping): time.Type = time.from(System.currentTimeMillis)