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
package savagery

import scala.collection.mutable.ListBuffer

import anticipation.*
import cardinality.*
import contingency.*
import distillate.*
import fulminate.*
import geodesy.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import xylophone.*
import zephyrine.*

object SvgParser:
  def labelOf(xml: Xml): Text = xml match
    case e: Element => e.label
    case _          => t"<unknown>"


  def findSvg(nodes: Seq[Node])(using Tactic[SvgError]): Element =
    nodes.collectFirst { case e: Element if e.label == t"svg" => e }.getOrElse:
      abort(SvgError(SvgError.Reason.NotAnSvg(t"<missing>")))


  def rootElement(xml: Xml)(using Tactic[SvgError]): Element = xml match
    case e: Element if e.label == t"svg" => e
    case Fragment(nodes*)                => findSvg(nodes)
    case other                           => abort(SvgError(SvgError.Reason.NotAnSvg(labelOf(other))))


  private def numAttr(elem: Element, name: Text, default: Float = 0.0f): Float =
    elem.attributes.at(name).let { text => safely(text.decode[Double].toFloat).or(default) }
    . or(default)


  def decodeSvg(elem: Element)(using Tactic[SvgError]): Svg =
    val width = numAttr(elem, t"width")
    val height = numAttr(elem, t"height")

    val defs = ListBuffer[SvgDef]()
    val figures = ListBuffer[Figure]()

    def walk(parent: Element): Unit = parent.children.each:
      case child: Element => child.label match
        case t"defs" => child.children.each:
          case dd: Element => decodeSvgDef(dd).let { svgDef => defs += svgDef }
          case _           => ()

        case t"g" =>
          walk(child)

        case _ =>
          decodeFigure(child).let { figure => figures += figure }

      case _ =>
        ()

    walk(elem)
    Svg(width, height, defs.toList, figures.toList)


  private def decodeFigure(elem: Element)(using Tactic[SvgError]): Optional[Figure] =
    elem.label match
      case t"rect"    => decodeRectangle(elem)
      case t"circle"  => decodeCircle(elem)
      case t"ellipse" => decodeEllipse(elem)
      case t"path"    => decodePath(elem)
      case _          => Unset


  private def decodeRectangle(elem: Element): Rectangle =
    Rectangle
     (Point(numAttr(elem, t"x"), numAttr(elem, t"y")),
      numAttr(elem, t"width"),
      numAttr(elem, t"height"))


  private def decodeCircle(elem: Element): Ellipse =
    val cx = numAttr(elem, t"cx")
    val cy = numAttr(elem, t"cy")
    val r = numAttr(elem, t"r")
    Ellipse(Point(cx, cy), r, r, Angle(0))


  private def decodeEllipse(elem: Element): Ellipse =
    val cx = numAttr(elem, t"cx")
    val cy = numAttr(elem, t"cy")
    val rx = numAttr(elem, t"rx")
    val ry = numAttr(elem, t"ry")
    Ellipse(Point(cx, cy), rx, ry, Angle(0))


  private def decodePath(elem: Element)(using Tactic[SvgError]): Outline =
    val d = elem.attributes.at(t"d").or(t"")
    val ops = parsePathData(d)
    val id = elem.attributes.at(t"id").let(SvgId(_))
    val transforms = elem.attributes.at(t"transform").let(parseTransforms).or(Nil)
    Outline(ops = ops.reverse, id = id, transforms = transforms)


  private def decodeSvgDef(elem: Element)
        (using Tactic[SvgError])
  :     Optional[SvgDef] =

    elem.label match
      case t"linearGradient" => decodeLinearGradient(elem)
      case _                 => Unset


  private def decodeLinearGradient(elem: Element)
        (using Tactic[SvgError])
  :     LinearGradient[Color in Srgb] =

    val id = elem.attributes.at(t"id").let(SvgId(_)).or(SvgId(t""))

    val stops: Seq[Stop[Color in Srgb]] = elem.children.toSeq.collect:
      case e: Element if e.label == t"stop" => decodeStop(e)

    LinearGradient(id, stops*)


  private def decodeStop(elem: Element)(using Tactic[SvgError]): Stop[Color in Srgb] =
    val rawOffset = elem.attributes.at(t"offset")
                  . let { text => safely(text.decode[Double]).or(0.0) }
                  . or(0.0)

    val clamped = rawOffset.max(0.0).min(1.0)
    val offset: 0.0 ~ 1.0 = NumericRange.apply[0.0, 1.0](clamped)
    val colorText = elem.attributes.at(t"stop-color").or(t"#000000")
    Stop(offset, parseColor(colorText))


  // SVG path-data tokeniser + dispatcher. Supports M/m, L/l, H/h, V/v, C/c, Q/q, Z/z.
  // Absolute H/V are converted to relative shifts (lossy — Savagery has no
  // absolute-horizontal-only stroke variant).
  private def parsePathData(d: Text)(using Tactic[SvgError]): List[Stroke] =
    if d.s.trim.nn.isEmpty then return Nil

    val s = d.s
    var pos = 0
    val ops = ListBuffer[Stroke]()

    def peek: Char = if pos < s.length then s.charAt(pos) else 0.toChar

    def skipWs(): Unit =
      while pos < s.length && {
        val c = s.charAt(pos)
        c == ' ' || c == ',' || c == '\t' || c == '\n' || c == '\r'
      }
      do pos += 1

    def isCommand(c: Char): Boolean = "MmLlHhVvCcQqZzAaSsTt".indexOf(c.toInt) >= 0

    def isNumberStart(c: Char): Boolean =
      c == '-' || c == '+' || c == '.' || (c >= '0' && c <= '9')

    def parseNum(): Float =
      skipWs()
      val start = pos
      if pos < s.length && (s.charAt(pos) == '-' || s.charAt(pos) == '+') then pos += 1

      while pos < s.length && {
        val c = s.charAt(pos)
        (c >= '0' && c <= '9') || c == '.' || c == 'e' || c == 'E' ||
            ((c == '-' || c == '+') && pos > start && (s.charAt(pos - 1) == 'e' || s.charAt(pos - 1) == 'E'))
      }
      do pos += 1

      if start == pos then abort(SvgError(SvgError.Reason.MalformedPathData(d)))
      else
        try s.substring(start, pos).nn.toFloat
        catch case _: NumberFormatException =>
          abort(SvgError(SvgError.Reason.MalformedPathData(d)))

    var lastCmd: Char = ' '

    skipWs()
    while pos < s.length do
      val c = peek

      if isCommand(c) then
        pos += 1
        lastCmd = c
        skipWs()

      lastCmd match
        case 'M' =>
          val x = parseNum()
          val y = parseNum()
          ops += Stroke.Move(Point(x, y))
          lastCmd = 'L' // implicit-line-after-move

        case 'm' =>
          val dx = parseNum()
          val dy = parseNum()
          ops += Stroke.Move(Shift(dx, dy))
          lastCmd = 'l'

        case 'L' =>
          val x = parseNum()
          val y = parseNum()
          ops += Stroke.Draw(Point(x, y))

        case 'l' =>
          val dx = parseNum()
          val dy = parseNum()
          ops += Stroke.Draw(Shift(dx, dy))

        case 'H' | 'h' =>
          val dx = parseNum()
          ops += Stroke.Draw(Shift(dx, 0.0f))

        case 'V' | 'v' =>
          val dy = parseNum()
          ops += Stroke.Draw(Shift(0.0f, dy))

        case 'C' =>
          val ax = parseNum(); val ay = parseNum()
          val bx = parseNum(); val by = parseNum()
          val px = parseNum(); val py = parseNum()
          ops += Stroke.Cubic(Point(ax, ay), Point(bx, by), Point(px, py))

        case 'c' =>
          val ax = parseNum(); val ay = parseNum()
          val bx = parseNum(); val by = parseNum()
          val px = parseNum(); val py = parseNum()
          ops += Stroke.Cubic(Shift(ax, ay), Shift(bx, by), Shift(px, py))

        case 'Q' =>
          val ax = parseNum(); val ay = parseNum()
          val px = parseNum(); val py = parseNum()
          ops += Stroke.Quadratic(Point(ax, ay), Point(px, py))

        case 'q' =>
          val ax = parseNum(); val ay = parseNum()
          val px = parseNum(); val py = parseNum()
          ops += Stroke.Quadratic(Shift(ax, ay), Shift(px, py))

        case 'Z' | 'z' =>
          ops += Stroke.Close
          // After Z, expect a new command. Don't continue with implicit Z.
          lastCmd = ' '

        case _ =>
          abort(SvgError(SvgError.Reason.MalformedPathData(d)))

      skipWs()

    ops.toList


  // Transform list parser. Recognises translate/scale/rotate/skewX/skewY/matrix.
  // Unknown function names are silently skipped.
  private def parseTransforms(t: Text): List[Transform] =
    val s = t.s
    var pos = 0
    val xs = ListBuffer[Transform]()

    def skipWs(): Unit =
      while pos < s.length && {
        val c = s.charAt(pos)
        c == ' ' || c == ',' || c == '\t' || c == '\n' || c == '\r'
      }
      do pos += 1

    def parseNum(): Optional[Float] =
      skipWs()
      val start = pos
      if pos < s.length && (s.charAt(pos) == '-' || s.charAt(pos) == '+') then pos += 1

      while pos < s.length && {
        val c = s.charAt(pos)
        (c >= '0' && c <= '9') || c == '.' || c == 'e' || c == 'E'
      }
      do pos += 1

      if start == pos || (pos == start + 1 && (s.charAt(start) == '-' || s.charAt(start) == '+'))
      then Unset
      else try s.substring(start, pos).nn.toFloat catch case _: NumberFormatException => Unset

    while pos < s.length do
      skipWs()
      if pos < s.length then
        val nameStart = pos

        while pos < s.length && {
          val c = s.charAt(pos)
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        }
        do pos += 1

        val name = s.substring(nameStart, pos).nn
        skipWs()

        if pos < s.length && s.charAt(pos) == '(' then
          pos += 1
          val args = ListBuffer[Float]()
          skipWs()

          while pos < s.length && s.charAt(pos) != ')' do
            parseNum().let { num => args += num }
            skipWs()

          if pos < s.length then pos += 1 // skip )

          (name, args.toList) match
            case ("translate", List(dx, dy))            => xs += Transform.Translate(Shift(dx, dy))
            case ("translate", List(dx))                => xs += Transform.Translate(Shift(dx, 0.0f))
            case ("scale", List(x))                     => xs += Transform.Scale(x, Unset)
            case ("scale", List(x, y))                  => xs += Transform.Scale(x, y)
            case ("rotate", List(angle))                => xs += Transform.Rotate(Angle.degrees(angle))
            case ("skewX", List(angle))                 => xs += Transform.SkewX(Angle.degrees(angle))
            case ("skewY", List(angle))                 => xs += Transform.SkewY(Angle.degrees(angle))
            case ("matrix", List(a, b, c, d, e, f))     => xs += Transform.Matrix(a, b, c, d, e, f)
            case _                                      => () // ignore unknown
        else
          if pos == nameStart then pos += 1 // avoid infinite loop on stray punctuation

    xs.toList


  // Color parser: handles #rgb, #rrggbb, rgb(r,g,b), and a few named colours.
  private def parseColor(c: Text)(using Tactic[SvgError]): Color in Srgb =
    val s = c.s.trim.nn

    def hex2(off: Int): Double =
      Integer.parseInt(s.substring(off, off + 2).nn, 16)/255.0

    def hex1(off: Int): Double =
      val n = Integer.parseInt(s.substring(off, off + 1).nn, 16)
      (n*16 + n)/255.0

    if s.startsWith("#") then
      val hex = s.substring(1).nn

      try
        if hex.length == 3 then Srgb(hex1(1), hex1(2), hex1(3))
        else if hex.length == 6 then Srgb(hex2(1), hex2(3), hex2(5))
        else abort(SvgError(SvgError.Reason.MalformedColor(c)))
      catch case _: NumberFormatException =>
        abort(SvgError(SvgError.Reason.MalformedColor(c)))
    else if s.startsWith("rgb(") && s.endsWith(")") then
      val inner = s.substring(4, s.length - 1).nn
      val parts = inner.split(",").nn.toList.map(_.nn.trim.nn)

      def parseChannel(part: String): Double =
        if part.endsWith("%") then part.substring(0, part.length - 1).nn.toDouble/100.0
        else part.toDouble/255.0

      if parts.length == 3 then
        try Srgb(parseChannel(parts(0)), parseChannel(parts(1)), parseChannel(parts(2)))
        catch case _: NumberFormatException =>
          abort(SvgError(SvgError.Reason.MalformedColor(c)))
      else abort(SvgError(SvgError.Reason.MalformedColor(c)))
    else
      s.toLowerCase.nn match
        case "red"     => Srgb(1.0, 0.0, 0.0)
        case "green"   => Srgb(0.0, 0.502, 0.0)
        case "blue"    => Srgb(0.0, 0.0, 1.0)
        case "black"   => Srgb(0.0, 0.0, 0.0)
        case "white"   => Srgb(1.0, 1.0, 1.0)
        case "yellow"  => Srgb(1.0, 1.0, 0.0)
        case "cyan"    => Srgb(0.0, 1.0, 1.0)
        case "magenta" => Srgb(1.0, 0.0, 1.0)
        case _         => abort(SvgError(SvgError.Reason.MalformedColor(c)))
