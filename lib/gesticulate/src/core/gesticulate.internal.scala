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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package gesticulate

import scala.collection.immutable.Seq

import proscenium.compat.*

import scala.quoted.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gigantism.*
import gossamer.*
import rudiments.*
import vacuous.*

import caseSensitivity.caseInsensitive
import proximities.levenshteinProximity

object internal:
  // Validation runs at macro-expansion time, so it must not reflectively
  // touch other classes in this same Mill module — when zinc batches the
  // macro implementation, its caller, and any class it would call (here
  // `Media`) into one recompile, the macro classloader cannot resolve a
  // sibling class. Keep this self-contained: only depend on classes from
  // other modules (anticipation/gossamer/fulminate/vacuous), never on
  // gesticulate's own types.
  private lazy val systemMediaTypes: Set[Text] =
    Optional(getClass.getResourceAsStream("/gesticulate/media.types")).lay(Set()): stream =>
      Set.from:
        scala.io.Source.fromInputStream(stream)
        . getLines()
        . map(Text(_))
        . map(_.cut(t"\t").head.lower)

  private val validGroups: Set[Text] =
    Set
      ( t"application", t"audio", t"image", t"message", t"multipart",
        t"text", t"video", t"font", t"example", t"model" )

  private val specials: Set[Char] =
    Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '+')

  def validateLiteral(text: Text): Optional[Message] =
    val parts: List[Text] = text.cut(t";").map(_.trim)

    parts.absolve match
      case Nil          => m"empty media type"
      case basic :: _   => validateBasic(basic)

  private def validateBasic(basic: Text): Optional[Message] =
    basic.cut(t"/").absolve match
      case List(group, subtype) =>
        val groupLower = group.lower

        if !validGroups.has(groupLower) then m"$group is not a valid media group"
        else validateSubtype(groupLower, subtype)

      case _ =>
        m"a media type must have exactly one '/' character"

  private def validateSubtype(group: Text, subtype: Text): Optional[Message] =
    // The full subtype may be a `main+suffix` (e.g. `svg+xml`); the
    // character check applies to each segment individually since `+`
    // is itself a separator, not a body character.
    val segments: List[Text] = subtype.cut(t"+")

    val badChar: Option[Char] = segments.iterator.flatMap: seg =>
      seg.chars.find: c =>
        c.isWhitespace || c.isControl || specials.has(c)

    . nextOption()

    badChar match
      case Some(char) =>
        m"$char is not a valid character in a media-type subtype"

      case None =>
        val main: Text = segments.head
        val isStandard =
          !main.starts(t"vnd.") && !main.starts(t"prs.") &&
            !main.starts(t"x.") && !main.starts(t"x-")

        if !isStandard then Unset
        else
          val canonical: Text = t"$group/${subtype.lower}"

          if systemMediaTypes.nil || systemMediaTypes.has(canonical) then Unset
          else
            val suggestion = systemMediaTypes.minBy(_.proximity(canonical))
            m"$canonical is not a registered media type; did you mean $suggestion?"

  def mediaInterpolator[parts <: Tuple: Type](insertions: Expr[Seq[Any]]): Macro[MediaType] =
    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)
    if parts.length != 1 then halt(m"a media type literal cannot have substitutions")

    val raw: String = parts.head

    internal.validateLiteral(raw.tt).let(halt(_))

    '{unsafely(Media.parse(${Expr(raw)}.tt))}
