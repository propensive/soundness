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
package fulminate

import anticipation.*

// A bolt code names a diagnostic of one of Propensive's projects, in one form across all of them:
// `↯`, and one or more groups, each begun by `/`. A group is the letter of a project and its
// numbers — a subdivision of the project, an error in it, and optionally a variant of the error
// and a site it is reported from — so `↯/s.87.3.2` is the second variant of the third error of
// Soundness's eighty-seventh module.
//
// A code of several groups is a compound: the first group is the error which was reported, and
// those after it are the error it was reported on account of — its `cause`, which is a code in
// its own right. `↯/s.87.1/t.1.7` is Soundness's error `87.1` carrying TEL's `↯/t.1.7`.
//
// The record of what each code means is the `bolt` repository, https://github.com/propensive/bolt.
object Bolt:
  // The letter by which bolt codes name Soundness.
  final val Soundness = 's'

  // The code of a diagnostic which has none.
  val Uncoded: Bolt = Bolt(' ', 0, 0)

  // The code a text begins with, written with or without brackets, and how many of the text's
  // characters it occupies, including a space which follows it; `Uncoded` and zero if the text
  // does not begin with a code.
  def prefix(text: Text): (Bolt, Int) =
    val string = text.s
    val bracketed = string.startsWith("[")
    var index = if bracketed then 1 else 0
    var valid = string.startsWith("↯", index)

    if valid then index += 1

    def digits(): Int =
      val start = index
      def digit: Boolean = string.charAt(index) >= '0' && string.charAt(index) <= '9'
      while index < string.length && digit do index += 1

      if index == start || index - start > 9 then
        valid = false
        0
      else string.substring(start, index).nn.toInt

    def more: Boolean = valid && index < string.length && string.charAt(index) == '.'

    // The groups from `index` on, the last of them innermost.
    def group(): Bolt | Null =
      if !valid || index >= string.length || string.charAt(index) != '/' then null
      else
        val letter = if index + 1 < string.length then string.charAt(index + 1) else ' '
        val lower = letter >= 'a' && letter <= 'z'

        if !lower || index + 2 >= string.length || string.charAt(index + 2) != '.' then
          valid = false
          null
        else
          index += 3
          val subdivision = digits()

          if !more then
            valid = false
            null
          else
            index += 1
            val error = digits()

            val variant =
              if more then
                index += 1
                digits()
              else 0

            val site =
              if more then
                index += 1
                digits()
              else 0

            if more then valid = false
            Bolt(letter, subdivision, error, variant, site, group())

    val bolt = group()

    if bracketed then
      if valid && index < string.length && string.charAt(index) == ']' then index += 1
      else valid = false

    if !valid || bolt == null then (Uncoded, 0)
    else
      if index < string.length && string.charAt(index) == ' ' then index += 1
      (bolt, index)

  // The code a text consists of; `Uncoded` if it is not exactly a code.
  def parse(text: Text): Bolt =
    val (bolt, length) = prefix(text)
    if length == text.s.length then bolt else Uncoded

// `variant` and `site` are zero where a code has none; a code with a site but no variant has the
// site as its third number, and is written with `variant` as that number and `site` zero.
case class Bolt
  ( project:     Char,
    subdivision: Int,
    error:       Int,
    variant:     Int         = 0,
    site:        Int         = 0,
    cause:       Bolt | Null = null ):

  def coded: Boolean = subdivision != 0

  // The error which was reported, without what it was reported on account of.
  def carrier: Bolt = copy(cause = null)

  // This code carrying another: the compound which reports this error on account of that one,
  // which goes innermost if this is already a compound.
  def carrying(inner: Bolt): Bolt =
    if !inner.coded then this
    else
      val next = cause
      copy(cause = if next == null then inner else next.carrying(inner))

  private def group: String =
    val third = if variant != 0 || site != 0 then s".$variant" else ""
    val fourth = if site != 0 then s".$site" else ""
    val next = cause
    s"/$project.$subdivision.$error$third$fourth${if next == null then "" else next.group}"

  // The code as it is written, `↯/s.87.3.2`; empty if there is none.
  def text: Text = if !coded then "".tt else ("↯"+group).tt

  // The code in brackets, as it is written before a message.
  def label: Text = if !coded then "".tt else s"[↯$group]".tt

  // The bracketed code in colour, for a terminal.
  def colorLabel: Text =
    if !coded then "".tt
    else
      val esc = 27.toChar
      val gray   = s"$esc[38;2;128;128;128m"
      val orange = s"$esc[38;2;255;165;0m"
      val cyan   = s"$esc[38;2;0;200;255m"
      val reset  = s"$esc[0m"

      def colored(bolt: Bolt): String =
        val numbered = bolt.variant != 0 || bolt.site != 0
        val third = if numbered then s"$gray.$cyan${bolt.variant}" else ""
        val fourth = if bolt.site != 0 then s"$gray.$cyan${bolt.site}" else ""
        val next = bolt.cause
        val rest = if next == null then "" else colored(next)

        s"$gray/$orange${bolt.project}$gray.$cyan${bolt.subdivision}$gray.$cyan${bolt.error}"
        + third + fourth + rest

      s"$gray[$orange↯${colored(this)}$gray]$reset".tt

  override def toString: String = text.s
