package clairvoyant

trait MakeTime:
  type T
  def fromLong(long: Long): T
  def toLong(value: T): Long

object timeTypes:
  given long: MakeTime with
    type T = Long
    def fromLong(long: Long): Long = long
    def toLong(value: Long): Long = value

  given date: MakeTime with
    type T = java.util.Date
    def fromLong(long: Long): java.util.Date = java.util.Date(long)
    def toLong(value: java.util.Date): Long = value.getTime