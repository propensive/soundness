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
package anticipation

import scala.compiletime.ops.any.*
import scala.compiletime.ops.int.*

// A phantom description of one channel in a packed pixel layout, such as `Red[10]`: a colour
// component with a bit depth, never instantiated. A pixel layout is a tuple of channels, most
// significant first, e.g. `(Red[10], Green[12], Blue[10])`. The `label` singleton gives every
// channel family a key which the match types in the companion use to locate a channel within a
// layout, without needing a match-type case for each channel family. The hierarchy is open: domain-specific channels can be introduced downstream so
// long as their labels are distinct.
object Channel:
  type Label[channel] <: String = channel match
    case Channel[label, bits] => label

  type Bits[channel] <: Int = channel match
    case Channel[label, bits] => bits

  type TotalBits[layout <: Tuple] <: Int = layout match
    case EmptyTuple   => 0
    case head *: tail => Bits[head] + TotalBits[tail]

  type Has[layout <: Tuple, label <: String] <: Boolean = layout match
    case EmptyTuple   => false
    case head *: tail => (Label[head] == label) match
      case true  => true
      case false => Has[tail, label]

  // The shift of the labelled channel is the total width of the channels packed below it. This
  // deliberately fails to reduce if the layout lacks the channel, statically gating accessors
  // like `.red` and `.cyan` to layouts which have them.
  type Shift[layout <: Tuple, label <: String] <: Int = layout match
    case head *: tail => (Label[head] == label) match
      case true  => TotalBits[tail]
      case false => Shift[tail, label]

  type Depth[layout <: Tuple, label <: String] <: Int = layout match
    case head *: tail => (Label[head] == label) match
      case true  => Bits[head]
      case false => Depth[tail, label]

  // The narrowest primitive which can store one pixel of the layout.
  type Storage[layout <: Tuple] <: Byte | Short | Int | Long = (TotalBits[layout] <= 8) match
    case true  => Byte
    case false => (TotalBits[layout] <= 16) match
      case true  => Short
      case false => (TotalBits[layout] <= 32) match
        case true  => Int
        case false => Long

// `@unexported`: `Channel` would clash with perihelion's WebSocket `Channel` in the `soundness`
// umbrella; reach the pixel channel machinery via `anticipation.Channel`.
@unexported
trait Channel[label <: String & Singleton, bits <: Int]

trait Red[bits <: Int] extends Channel["red", bits]
trait Green[bits <: Int] extends Channel["green", bits]
trait Blue[bits <: Int] extends Channel["blue", bits]
trait Alpha[bits <: Int] extends Channel["alpha", bits]
trait Cyan[bits <: Int] extends Channel["cyan", bits]
trait Magenta[bits <: Int] extends Channel["magenta", bits]
trait Yellow[bits <: Int] extends Channel["yellow", bits]
trait Key[bits <: Int] extends Channel["key", bits]
trait Grey[bits <: Int] extends Channel["grey", bits]
