                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ultimatum

import anticipation.*
import gossamer.*
import spectacular.*

// Compact renderings of the numbers that appear beside a gauge, where every cell is contested and
// a full-precision figure would crowd out the bar itself.
object Magnitude:
  // A duration as the two largest units that carry information: `41s`, `2m41s`, `1h04m`, `3d02h`.
  // Never more than two, and never a leading zero unit, so the field stays narrow and stops
  // jittering once it is past a minute.
  def interval(seconds: Double): Text =
    val total = seconds.max(0.0).toLong

    if total < 60 then t"${total}s" else
      val minutes = total/60
      val hours = minutes/60
      val days = hours/24

      if minutes < 60 then t"${minutes}m${pad(total%60)}s"
      else if hours < 24 then t"${hours}h${pad(minutes%60)}m"
      else t"${days}d${pad(hours%24)}h"

  // A count, abbreviated once it stops being readable at a glance: `947`, `1.2k`, `15k`, `3.4M`.
  def count(value: Long): Text =
    if value < 1000 then value.show
    else if value < 1000000 then scaled(value, 1000.0, t"k")
    else if value < 1000000000L then scaled(value, 1000000.0, t"M")
    else scaled(value, 1000000000.0, t"G")

  // A percentage, always three cells wide (`  0%` … `100%` less its sign), so a bar's right-hand
  // figure never shifts as it fills.
  def percentage(fraction: Fraction): Text =
    val value = fraction.percentage
    if value >= 100 then t"100%" else if value >= 10 then t" $value%" else t"  $value%"

  private def pad(value: Long): Text = if value < 10 then t"0$value" else value.show

  // One decimal place below ten, none above: `1.2k` but `15k`, so the field is at most four cells.
  private def scaled(value: Long, divisor: Double, suffix: Text): Text =
    val scale = value/divisor

    if scale >= 10 then t"${scale.toLong}$suffix" else
      val tenths = (scale*10).toLong
      t"${tenths/10}.${tenths%10}$suffix"
