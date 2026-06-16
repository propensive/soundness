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
package xylophone

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import serpentine.*
import vacuous.*

object XPath extends Root("/"):
  type Plane = XPath

  given navigable: [text <: Text] => text is Navigable on XPath = identity(_)
  given admissible: [text <: Text] => text is Admissible on XPath = _ => ()

  // XPath path step encodings. Element steps include their 1-indexed ordinal
  // among siblings sharing the same local name. Attribute steps are prefixed
  // with `@`, the XPath convention, and may only appear as the final step
  // of a path.
  def elementStep(name: Text, ordinal: Int = 1): Text = t"$name[$ordinal]"
  def attributeStep(name: Text): Text = t"@$name"

  given filesystem: XPath is Filesystem:
    val name: Text = "XPath"
    val parent: Text = ".."
    val self: Text = "."
    val separator: Text = "/"

  // The underlying serpentine `Path` already encodes with the leading `/`
  // separator (Path.encode = root + descent.reverse.join(sep)). Re-using
  // it directly avoids the double-slash that `t"/${path.path}"` produced.
  given XPath is Encodable in Text = path =>
    if path.path.descent.length == 0 then t"/" else path.path.encode

  inline given interpolable: XPath is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   XPath =

      ${xylophone.internal.xpath[parts, origins]('insertions)}

  // Parses an absolute XPath (`/`, `/root[1]/child[2]`, `/root/@attr`),
  // reporting the offset of any error. Each step is interpreted by `parseStep`;
  // an unrecognised (e.g. empty, or `name[x]` with a non-numeric ordinal) step
  // is a `BadStep`.
  given decodable: Tactic[XPathError] => XPath is Decodable in Text = text =>
    val string = text.s

    if string.isEmpty || string.charAt(0) != '/'
    then abort(XPathError(XPathError.Reason.ExpectedSlash, 0))
    else
      var xpath: XPath = XPath()
      var offset = 1

      while offset < string.length do
        val slash = string.indexOf('/', offset)
        val end = if slash < 0 then string.length else slash

        parseStep(string.substring(offset, end).nn.tt) match
          case Left(name)       => xpath = xpath.attribute(name)
          case Right((name, n)) => xpath = xpath.element(name, n)
          case Unset            => abort(XPathError(XPathError.Reason.BadStep, offset))

        offset = end + 1

      xpath

  // Parse a stored descent segment back into an addressable step. Returns
  // `Right(name, ordinal)` for an element step, `Left(name)` for an
  // attribute step, or `Unset` for an unrecognised segment.
  def parseStep(segment: Text): Optional[Either[Text, (Text, Int)]] =
    if segment.length == 0 then Unset
    else if segment.s.charAt(0) == '@' then Left(segment.s.drop(1).tt)
    else
      val s = segment.s
      val openBracket = s.lastIndexOf('[')

      if openBracket >= 0 && s.endsWith("]") then
        val name = s.substring(0, openBracket).nn.tt
        val ordinalText = s.substring(openBracket + 1, s.length - 1).nn

        try Right((name, Integer.parseInt(ordinalText)))
        catch case _: NumberFormatException => Unset
      else
        Right((segment, 1))

case class XPath(path: Path on XPath = XPath):
  def element(name: Text, ordinal: Int = 1): XPath =
    XPath(path / XPath.elementStep(name, ordinal))

  def attribute(name: Text): XPath =
    XPath(path / XPath.attributeStep(name))

  // Prepend `name[ordinal]` at the root end of the path, leaving the rest
  // of the descent intact. Used by `Xml`'s Wisteria derivation: each outer
  // `focus` block runs *after* the inner one, and needs to push its label
  // to the front of the accumulated XPath (so `/parent[1]/child[1]` lands
  // root-first), not append at the leaf end like `element` does.
  private[xylophone] def prepend(name: Text, ordinal: Int = 1): XPath =
    val step = XPath.elementStep(name, ordinal)
    XPath(Path[XPath, XPath.type, Tuple]("/", path.descent :+ step))
