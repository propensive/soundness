package aviation

import quantitative.*

trait Horology:
  type Primary
  type Secondary
  type Tertiary
  type TimeRepr

  def addPrimary(time: Clockface, n: Primary): Clockface
  def addSecondary(time: Clockface, n: Secondary): Clockface
  def addTertiary(time: Clockface, n: Tertiary): Clockface

object Horology:
  given Horology as sexagesimal:
    type Primary = Base24
    type Secondary = Base60
    type Tertiary = Base60
    type TimeRepr = Time

    def addPrimary(time: Clockface, n: Base24): Clockface = time.copy(hour = Base24(time.hour + n))

    def addSecondary(time: Clockface, n: Base60): Clockface =
      val minute: Base60 = Base60(time.minute + n)
      val hour: Base24 = Base24(time.hour + (time.minute + n)/60)
      time.copy(hour = hour, minute = minute)

    def addTertiary(time: Clockface, n: Base60): Clockface =
      val second: Base60 = Base60(time.second + n)
      val minute: Base60 = Base60(time.minute + (time.second + n)/60)
      val hour: Base24 = Base24(time.hour + (time.minute + (time.second + n)/60)/60)
      Clockface(hour, minute, second)