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
package phoenicia

import anticipation.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Typesettable:
  // How a medium obtains a typeface: the font file itself, carried inside the document; a URL to
  // fetch it from; a font the reader already has installed; a stylesheet that declares it; or
  // nothing at all, for a generic family every medium can set.
  object Source:
    enum Format:
      case Ttf, Otf, Woff, Woff2

  enum Source:
    case Embedded(sfnt: Sfnt)
    case Linked(url: Text, format: Optional[Source.Format])
    case Local(name: Text)
    case Imported(url: Text)
    case Generic

  // The instance `embedded` returns; a named class gives it a stable type.
  class Embedding[family <: Label](sfnts: List[Sfnt]) extends Typesettable:
    type Self = Typeface of family
    type Form = Medium
    val sources: List[Source] = sfnts.map(Source.Embedded(_))
    val coverage: Coverage = Coverage.combine(sfnts.map(Coverage.of(_)))

  // A provision made of font files, which every medium can carry, so it is `in Medium`. Several
  // files — regular, bold, italic — make one provision whose coverage is their union; a variable
  // font's axes are read from its `fvar` table.
  def embedded[family <: Label](sfnts: Sfnt*): Typeface of family is Typesettable in Medium =
    Embedding[family](List.from(sfnts))

  // A provision from a font file at a known location — a classpath resource, `cp"/fonts/x.ttf"`
  // — whose path becomes the provision's `Locus`. A font made from it is then checked against the
  // file as the code compiles, where the file is on the compiler's classpath:
  //
  //     given inter: ((Typeface of "Inter") is Typesettable in Medium at "/fonts/Inter.ttf") =
  //       Typesettable.embedded(cp"/fonts/Inter.ttf")
  def embedded
    [ family <: Label,
      locus <: Label,
      source <: Locative { type Locus = locus } : Streamable by Data over Credit ]
    ( source: source )
  :   Typeface of family is Typesettable in Medium at locus =

    new Embedding[family](List(Sfnt(source))) { type Locus = locus }

// The provision of a typeface in a medium. An instance says where the medium gets the font
// (`sources`) and which faces it can then set (`coverage`). A requester asks for a provision
// `in (? >: medium)`: an instance `in Medium`, such as an embedded font, serves every medium,
// while one `in Web` serves only the web. Instances are pure data, and live where the medium
// lives: the companion of `Web` holds the generic families, the companion of `Print` holds PDF's
// standard fourteen, and a project declares its own as givens in an object it imports. A provision
// from a file at a known location carries the path as its `Locus`, for the compile-time check.
@missingContext
  ( "phoenicia: no provision is in scope for this typeface in this medium. Declare one, such as "+
    "`given inter: (Typeface of \"Inter\") is Typesettable in Medium = "+
    "Typesettable.embedded(sfnt)` for a font file, or a provision the medium supplies, such as "+
    "`Web.imported(url, coverage)`." )
trait Typesettable extends Typeclass.Pure, Formal, Locative:
  type Self <: Typeface
  def sources: List[Typesettable.Source]
  def coverage: Coverage
