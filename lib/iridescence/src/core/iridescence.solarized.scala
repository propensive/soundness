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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package iridescence

import prepositional.*

trait Solarized extends Theme:
  type Form = Srgb
  def red: Color in Srgb     = Srgb(0.863, 0.196, 0.184)
  def yellow: Color in Srgb  = Srgb(0.710, 0.537, 0.000)
  def blue: Color in Srgb    = Srgb(0.149, 0.545, 0.824)
  def green: Color in Srgb   = Srgb(0.522, 0.600, 0.000)
  def orange: Color in Srgb  = Srgb(0.796, 0.294, 0.086)
  def cyan: Color in Srgb    = Srgb(0.165, 0.631, 0.596)
  def magenta: Color in Srgb = Srgb(0.827, 0.212, 0.510)
  def violet: Color in Srgb  = Srgb(0.424, 0.443, 0.769)
  def base03: Color in Srgb  = Srgb(0.000, 0.169, 0.212)
  def base02: Color in Srgb  = Srgb(0.027, 0.212, 0.259)
  def base01: Color in Srgb  = Srgb(0.345, 0.431, 0.459)
  def base00: Color in Srgb  = Srgb(0.396, 0.482, 0.514)
  def base0: Color in Srgb   = Srgb(0.514, 0.580, 0.588)
  def base1: Color in Srgb   = Srgb(0.576, 0.631, 0.631)
  def base2: Color in Srgb   = Srgb(0.933, 0.910, 0.835)
  def base3: Color in Srgb   = Srgb(0.992, 0.965, 0.890)

  val colors: List[Color in Srgb] =
    List(red, yellow, blue, green, orange, cyan, magenta, violet, base3, base03)
