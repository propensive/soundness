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
package savagery

import scala.collection.mutable.ListBuffer

import anticipation.*
import cardinality.*
import cataclysm.formatting.compactCssFormatting
import cataclysm.{Css, FontFace}
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import geodesy.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*
import xylophone.*
import zephyrine.*

object Svg:
  // The `<svg>` document element, with its definitions and figures, as `svg.in[Xml]`.
  given encodable: Svg is Encodable in Xml = _.markup

  // SVG's vocabulary is fixed and validated by the figure types, so the XML beneath is parsed
  // against the free-form schema; no `XmlSchema` is asked of the caller.
  given aggregable
  :   ( parseTactic: Tactic[Parse.Error], xmlTactic: Tactic[Xml.Error], svgTactic: Tactic[Svg.Error] )
  =>  ( (Svg is Aggregable by Text)^{parseTactic, xmlTactic, svgTactic} ) =

    source =>
      given XmlSchema = XmlSchema.Freeform
      val xml: Xml = summon[Xml is Aggregable by Text].aggregate(source)
      Svg.Parser.decodeSvg(Svg.Parser.rootElement(xml))

  given loadable
  :   ( parseTactic: Tactic[Parse.Error] )
  =>  ( xmlTactic: Tactic[Xml.Error] )
  =>  ( svgTactic: Tactic[Svg.Error] )
  =>  ( (Svg is Loadable by Text)^{parseTactic, xmlTactic, svgTactic} ) =
    source =>
      given XmlSchema = XmlSchema.Freeform
      fromXml(summon[(Xml is Loadable by Text)^].load(source))

  // The byte form: the XML is parsed from the bytes directly.
  given loadableData
  :   ( parseTactic: Tactic[Parse.Error] )
  =>  ( xmlTactic: Tactic[Xml.Error] )
  =>  ( svgTactic: Tactic[Svg.Error] )
  =>  ( buffering: Buffering )
  =>  ( (Svg is Loadable by Data)^{parseTactic, xmlTactic, svgTactic} ) =
    source =>
      given XmlSchema = XmlSchema.Freeform
      fromXml(summon[(Xml is Loadable by Data)^].load(source))

  private def fromXml(xmlDoc: Document[Xml])(using Tactic[Xml.Error], Tactic[Svg.Error])
  :   Document[Svg] =

    val svgElement = Svg.Parser.rootElement(xmlDoc.root)
    val parsedSvg: Svg = Svg.Parser.decodeSvg(svgElement)

    val encoding: Encoding = xmlDoc.metadata.encoding.let:
      case Encoding(encoding) => encoding
      case _                  => enc"UTF-8"

    . or(enc"UTF-8")

    Document[Svg](parsedSvg, encoding)

  given showable: [doc <: Document[Svg]] => doc is Showable =
    document =>
      val header = Xml.Header(t"1.0", document.metadata.name, Unset)

      val full: Xml = document.root.in[Xml].absolve match
        case node: Xml.Node       => Xml.Fragment(header, node)
        case Xml.Fragment(nodes*) => Xml.Fragment((header +: nodes)*)

      full.show

  // SvgError → Svg.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NotAnSvg(label: Text)                   extends Reason(1)
      case MalformedPathData(data: Text)           extends Reason(2)
      case MalformedColor(color: Text)             extends Reason(3)
      case MalformedLength(length: Text)           extends Reason(4)
      case MismatchedUnits(width: Text, height: Text) extends Reason(5)
      case MalformedViewBox(viewBox: Text)         extends Reason(6)

    given communicable: Reason is Communicable =
      case Reason.NotAnSvg(label)         => m"the root element was <$label> instead of <svg>"
      case Reason.MalformedPathData(data) => m"the path data $data could not be parsed"
      case Reason.MalformedColor(color)   => m"the color $color could not be parsed"
      case Reason.MalformedLength(length) => m"the length $length is not a number with a unit"
      case Reason.MalformedViewBox(box)   => m"the viewBox $box is not four numbers"

      case Reason.MismatchedUnits(width, height) =>
        m"the width $width and height $height have different units"

  // The units an `<svg>` element's `width` and `height` may carry; a bare number is user units.
  // (`Units`, not `Unit`, which would shadow `scala.Unit` throughout this object.)
  enum Units(val suffix: Text):
    case Em      extends Units(t"em")
    case Ex      extends Units(t"ex")
    case Px      extends Units(t"px")
    case In      extends Units(t"in")
    case Cm      extends Units(t"cm")
    case Mm      extends Units(t"mm")
    case Pt      extends Units(t"pt")
    case Pc      extends Units(t"pc")
    case Percent extends Units(t"%")

  // The `viewBox` attribute: the user-space rectangle the viewport shows.
  object ViewBox:
    given showable: ViewBox is Showable = box =>
      t"${box.x.show} ${box.y.show} ${box.width.show} ${box.height.show}"

  case class ViewBox(x: Float, y: Float, width: Float, height: Float)

  case class Error(reason: Svg.Error.Reason)(using Diagnostics)
  extends fulminate.Error(122, reason.number)(m"the SVG could not be parsed because $reason")

  // SvgParser → Svg.Parser
  object Parser:
    def labelOf(xml: Xml): Text = xml match
      case e: Xml.Element => e.label
      case _              => t"<unknown>"

    def findSvg(nodes: List[Xml.Node])(using Tactic[Svg.Error]): Xml.Element =
      nodes.reap { case e: Xml.Element if e.label == t"svg" => e }.or:
        abort(Svg.Error(Svg.Error.Reason.NotAnSvg(t"<missing>")))

    def rootElement(xml: Xml)(using Tactic[Svg.Error]): Xml.Element = xml match
      case e: Xml.Element if e.label == t"svg" => e
      case Xml.Fragment(nodes*)                => findSvg(nodes.to(List))

      case other =>
        abort(Svg.Error(Svg.Error.Reason.NotAnSvg(labelOf(other))))

    private def numAttr(elem: Xml.Element, name: Text, default: Float = 0.0f): Float =
      elem.attributes(name).let: text => safely(text.as[Double].toFloat).or(default)
      . or(default)

    // An SVG `<length>`: a number with an optional unit suffix. The suffix is matched first,
    // since `em` and `ex` would otherwise read as a malformed exponent.
    private def lengthAttr(elem: Xml.Element, name: Text)(using Tactic[Svg.Error])
    :   (Float, Optional[Units]) =

      elem.attributes(name).lay((0.0f, Unset)): text =>
        val trimmed = text.trim

        val unit: Optional[Units] =
          Units.values.find(unit => trimmed.ends(unit.suffix)).getOrElse(Unset)

        val number = unit.lay(trimmed)(unit => trimmed.skip(unit.suffix.length, Rtl))

        safely(number.as[Double].toFloat).let((_, unit)).or:
          abort(Svg.Error(Svg.Error.Reason.MalformedLength(text)))

    // Four numbers, separated by whitespace and/or a comma.
    private def viewBoxAttr(elem: Xml.Element)(using Tactic[Svg.Error]): Optional[ViewBox] =
      elem.attributes(t"viewBox").let: text =>
        def number(part: Text): Float =
          safely(part.as[Double].toFloat).or:
            abort(Svg.Error(Svg.Error.Reason.MalformedViewBox(text)))

        text.trim match
          case r"$x([^\s,]+)[\s,]+$y([^\s,]+)[\s,]+$width([^\s,]+)[\s,]+$height([^\s,]+)" =>
            ViewBox(number(x), number(y), number(width), number(height))

          case _ =>
            abort(Svg.Error(Svg.Error.Reason.MalformedViewBox(text)))

    def decodeSvg(elem: Xml.Element)(using Tactic[Svg.Error]): Svg =
      val (width, widthUnit) = lengthAttr(elem, t"width")
      val (height, heightUnit) = lengthAttr(elem, t"height")

      // One unit serves both dimensions; a viewport that mixes them is not representable.
      val unit: Optional[Units] =
        if widthUnit == heightUnit then widthUnit
        else if elem.attributes(t"width").absent then heightUnit
        else if elem.attributes(t"height").absent then widthUnit
        else
          abort:
            Svg.Error:
              Svg.Error.Reason.MismatchedUnits
                ( elem.attributes(t"width").or(t""), elem.attributes(t"height").or(t"") )

      val viewBox = viewBoxAttr(elem)

      val defs = ListBuffer[Def]()
      val figures = ListBuffer[Figure]()

      elem.children.each:
        case child: Xml.Element => child.label match
          case t"defs" => child.children.each:
            case dd: Xml.Element => decodeSvgDef(dd).let: svgDef => defs += svgDef
            case _               => ()

          case _ =>
            decodeFigure(child).let: figure => figures += figure

        case _ =>
          ()

      Svg(width, height, defs.to(List), figures.to(List), unit = unit, viewBox = viewBox)

    private def decodeFigure(elem: Xml.Element)(using Tactic[Svg.Error]): Optional[Figure] =
      elem.label match
        case t"rect"     => decodeRectangle(elem)
        case t"circle"   => decodeCircle(elem)
        case t"ellipse"  => decodeEllipse(elem)
        case t"path"     => decodePath(elem)
        case t"g"        => decodeGroup(elem)
        case t"polyline" => decodePolyline(elem, false)
        case t"polygon"  => decodePolyline(elem, true)
        case t"text"     => decodeLettering(elem)
        case _           => Unset

    // The attributes any figure may carry: read by every decoder, so that a figure's identifier,
    // transform list and inline style survive a round trip whatever its shape.
    private def idAttr(elem: Xml.Element): Optional[Id] = elem.attributes(t"id").let(Id(_))

    private def transformsAttr(elem: Xml.Element): List[Transform] =
      elem.attributes(t"transform").let(parseTransforms).or(Nil)

    // An inline `style` attribute is `name: value` declarations separated by semicolons; each is
    // split at its first colon, and a declaration without one is dropped.
    private def styleAttr(elem: Xml.Element): Optional[Css.Style] =
      elem.attributes(t"style").let: text =>
        val declarations = ListBuffer[(Text, Text)]()

        text.cut(t";").each: declaration =>
          declaration.offsetOf(t":").let: colon =>
            if colon != Prim then
              val name = declaration.before(colon).trim
              if !name.nil then declarations += ((name, declaration.after(colon).trim))

        if declarations.isEmpty then Unset else Css.Style.of(declarations.to(List))

    private def decodeRectangle(elem: Xml.Element): Rectangle =
      Rectangle
        ( Point(numAttr(elem, t"x"), numAttr(elem, t"y")),
          numAttr(elem, t"width"),
          numAttr(elem, t"height"),
          transformsAttr(elem),
          styleAttr(elem),
          idAttr(elem) )

    private def decodeCircle(elem: Xml.Element): Ellipse =
      val cx = numAttr(elem, t"cx")
      val cy = numAttr(elem, t"cy")
      val r = numAttr(elem, t"r")
      Ellipse(Point(cx, cy), r, r, Angle(0), transformsAttr(elem), styleAttr(elem), idAttr(elem))

    private def decodeEllipse(elem: Xml.Element): Ellipse =
      val cx = numAttr(elem, t"cx")
      val cy = numAttr(elem, t"cy")
      val rx = numAttr(elem, t"rx")
      val ry = numAttr(elem, t"ry")

      Ellipse
        ( Point(cx, cy), rx, ry, Angle(0), transformsAttr(elem), styleAttr(elem), idAttr(elem) )

    private def decodePath(elem: Xml.Element)(using Tactic[Svg.Error]): Outline =
      val d = elem.attributes(t"d").or(t"")
      val ops = parsePathData(d)
      Outline(ops.reverse, styleAttr(elem), idAttr(elem), transformsAttr(elem))

    private def decodeGroup(elem: Xml.Element)(using Tactic[Svg.Error]): Group =
      val figures = ListBuffer[Figure]()

      elem.children.each:
        case child: Xml.Element => decodeFigure(child).let: figure => figures += figure
        case _                  => ()

      Group(figures.to(List), idAttr(elem), styleAttr(elem), transformsAttr(elem))

    // A `points` attribute is pairs of numbers, separated by whitespace or commas within and
    // between pairs alike; an odd trailing number is dropped.
    private def decodePolyline(elem: Xml.Element, closed: Boolean): Polyline =
      val numbers = ListBuffer[Float]()

      elem.attributes(t"points").or(t"").s.split("[\\s,]+").nn.iterator.map(_.nn).foreach: number =>
        if !number.isEmpty then
          try numbers += number.toFloat catch case _: NumberFormatException => ()

      val points = ListBuffer[Point]()
      var index = 0

      while index + 1 < numbers.length do
        points += Point(numbers(index), numbers(index + 1))
        index += 2

      Polyline(points.to(List), closed, idAttr(elem), styleAttr(elem), transformsAttr(elem))

    private def decodeLettering(elem: Xml.Element): Lettering =
      val text: Text = elem.children.readable.toList.collect { case Xml.Text(text) => text }
        . to(List).join

      val anchor: Lettering.Anchor = elem.attributes(t"text-anchor") match
        case t"middle" => Lettering.Anchor.Middle
        case t"end"    => Lettering.Anchor.End
        case _         => Lettering.Anchor.Start

      val baseline: Optional[Lettering.Baseline] = elem.attributes(t"dominant-baseline") match
        case t"alphabetic" => Lettering.Baseline.Alphabetic
        case t"middle"     => Lettering.Baseline.Middle
        case t"hanging"    => Lettering.Baseline.Hanging
        case _             => Unset

      Lettering
        ( Point(numAttr(elem, t"x"), numAttr(elem, t"y")),
          text,
          anchor,
          baseline,
          idAttr(elem),
          styleAttr(elem),
          transforms = transformsAttr(elem) )


    private def decodeSvgDef(elem: Xml.Element)
      ( using Tactic[Svg.Error] )
    :   Optional[Def] =

      elem.label match
        case t"linearGradient" => decodeLinearGradient(elem)
        case _                 => Unset


    private def decodeLinearGradient(elem: Xml.Element)
      ( using Tactic[Svg.Error] )
    :   LinearGradient[Color in Srgb] =

      val id = elem.attributes(t"id").let(Id(_)).or(Id(t""))

      val stops: List[Stop[Color in Srgb]] =

        elem.children.readable.toList.collect:
          case e: Xml.Element if e.label == t"stop" => decodeStop(e)

        . to(List)

      LinearGradient(id, stops*)


    private def decodeStop(elem: Xml.Element)(using Tactic[Svg.Error]): Stop[Color in Srgb] =
      val rawOffset = elem.attributes(t"offset")
        . let: text => safely(text.as[Double]).or(0.0)
        . or(0.0)

      val clamped = rawOffset.max(0.0).min(1.0)
      val offset: 0.0 ~ 1.0 = NumericRange.apply[0.0, 1.0](clamped)
      val colorText = elem.attributes(t"stop-color").or(t"#000000")
      Stop(offset, parseColor(colorText))

    // SVG path-data tokeniser + dispatcher. Supports M/m, L/l, H/h, V/v, C/c, Q/q, Z/z.
    // Absolute H/V are converted to relative shifts (lossy — Savagery has no
    // absolute-horizontal-only stroke variant).
    private def parsePathData(d: Text)(using Tactic[Svg.Error]): List[Stroke] =
      if d.blank then return Nil

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
          val prev = if pos > 0 then s.charAt(pos - 1) else ' '

          (c >= '0' && c <= '9') || c == '.' || c == 'e' || c == 'E' ||
            ((c == '-' || c == '+') && pos > start && (prev == 'e' || prev == 'E'))
        }
        do pos += 1

        if start == pos then abort(Svg.Error(Svg.Error.Reason.MalformedPathData(d)))
        else
          try s.substring(start, pos).nn.toFloat
          catch case _: NumberFormatException =>
            abort(Svg.Error(Svg.Error.Reason.MalformedPathData(d)))

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
            ops += Stroke.MoveTo(Point(x, y))
            lastCmd = 'L' // implicit-line-after-move

          case 'm' =>
            val dx = parseNum()
            val dy = parseNum()
            ops += Stroke.Move(Delta(dx, dy))
            lastCmd = 'l'

          case 'L' =>
            val x = parseNum()
            val y = parseNum()
            ops += Stroke.DrawTo(Point(x, y))

          case 'l' =>
            val dx = parseNum()
            val dy = parseNum()
            ops += Stroke.Draw(Delta(dx, dy))

          case 'H' | 'h' =>
            val dx = parseNum()
            ops += Stroke.Draw(Delta(dx, 0.0f))

          case 'V' | 'v' =>
            val dy = parseNum()
            ops += Stroke.Draw(Delta(0.0f, dy))

          case 'C' =>
            val ax = parseNum(); val ay = parseNum()
            val bx = parseNum(); val by = parseNum()
            val px = parseNum(); val py = parseNum()
            ops += Stroke.CubicTo(Point(ax, ay), Point(bx, by), Point(px, py))

          case 'c' =>
            val ax = parseNum(); val ay = parseNum()
            val bx = parseNum(); val by = parseNum()
            val px = parseNum(); val py = parseNum()
            ops += Stroke.Cubic(Delta(ax, ay), Delta(bx, by), Delta(px, py))

          case 'Q' =>
            val ax = parseNum(); val ay = parseNum()
            val px = parseNum(); val py = parseNum()
            ops += Stroke.QuadraticTo(Point(ax, ay), Point(px, py))

          case 'q' =>
            val ax = parseNum(); val ay = parseNum()
            val px = parseNum(); val py = parseNum()
            ops += Stroke.Quadratic(Delta(ax, ay), Delta(px, py))

          case 'Z' | 'z' =>
            ops += Stroke.Close
            // After Z, expect a new command. Don't continue with implicit Z.
            lastCmd = ' '

          case _ =>
            abort(Svg.Error(Svg.Error.Reason.MalformedPathData(d)))

        skipWs()

      ops.to(List)

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
              parseNum().let: num => args += num
              skipWs()

            if pos < s.length then pos += 1 // skip )

            (name, args.to(List)) match
              case ("translate", List(dx, dy))        => xs += Transform.Translate(Delta(dx, dy))
              case ("translate", List(dx))            => xs += Transform.Translate(Delta(dx, 0.0f))
              case ("scale", List(x))                 => xs += Transform.Scale(x, Unset)
              case ("scale", List(x, y))              => xs += Transform.Scale(x, y)
              case ("rotate", List(angle))            => xs += Transform.Rotate(Angle.degrees(angle))

              case ("skewX", List(angle)) =>
                xs += Transform.Skew(Angle.degrees(angle), Orientation.Horizontal)

              case ("skewY", List(angle)) =>
                xs += Transform.Skew(Angle.degrees(angle), Orientation.Vertical)

              case ("matrix", List(a, b, c, d, e, f)) =>
                xs += Transform.Matrix(Affine(a, b, c, d, e, f))

              case _                                  => () // ignore unknown
          else
            if pos == nameStart then pos += 1 // avoid infinite loop on stray punctuation

      xs.to(List)

    // Color parser: handles #rgb, #rrggbb, rgb(r,g,b), and a few named colours.
    private def parseColor(c: Text)(using Tactic[Svg.Error]): Color in Srgb =
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
          else abort(Svg.Error(Svg.Error.Reason.MalformedColor(c)))
        catch case _: NumberFormatException =>
          abort(Svg.Error(Svg.Error.Reason.MalformedColor(c)))
      else if s.startsWith("rgb(") && s.endsWith(")") then
        val inner = s.substring(4, s.length - 1).nn
        val parts = inner.split(",").nn.iterator.map(_.nn.trim.nn).toList

        def parseChannel(part: String): Double =
          if part.endsWith("%") then part.substring(0, part.length - 1).nn.toDouble/100.0
          else part.toDouble/255.0

        if parts.length == 3 then
          try Srgb(parseChannel(parts(0)), parseChannel(parts(1)), parseChannel(parts(2)))
          catch case _: NumberFormatException =>
            abort(Svg.Error(Svg.Error.Reason.MalformedColor(c)))
        else
          abort(Svg.Error(Svg.Error.Reason.MalformedColor(c)))
      else
        (s.toLowerCase.nn: @unchecked) match
          case "red"     => Srgb(1.0, 0.0, 0.0)
          case "green"   => Srgb(0.0, 0.502, 0.0)
          case "blue"    => Srgb(0.0, 0.0, 1.0)
          case "black"   => Srgb(0.0, 0.0, 0.0)
          case "white"   => Srgb(1.0, 1.0, 1.0)
          case "yellow"  => Srgb(1.0, 1.0, 0.0)
          case "cyan"    => Srgb(0.0, 1.0, 1.0)
          case "magenta" => Srgb(1.0, 0.0, 1.0)
          case _         => abort(Svg.Error(Svg.Error.Reason.MalformedColor(c)))

  // SvgId → Svg.Id
  object Id:
    def apply(id: Text): Id = id

    extension (id: Id) def text: Text = id

    // An `Id` is a `Text` underneath, so a bare rendering of its text would be indistinguishable
    // from a `Text`; the constructor form names the type it belongs to.
    given inspectable: [id <: Id] => id is Inspectable = id => t"Svg.Id(${(id: Id).text.inspect})"

  opaque type Id = Text

  // SvgDef → Svg.Def, with LinearGradient, its only subtype: a sealed trait pins its
  // subtypes to its file, so they nest together or not at all.
  object Def:
    given encodable: [definition <: Def] => definition is Encodable in Xml = _.markup

  sealed trait Def:
    private[savagery] def markup: Xml

  case class LinearGradient[color](id: Id, stops: Stop[color]*) extends Def:
    private[savagery] def markup: Xml =
      val nodes = List.from(stops.map(_.in[Xml])).nodes
      Xml.Element(t"linearGradient", Attributes(t"id" -> Id.text(id)), nodes)

// `width` and `height` are the viewport's size in `unit`, or in user units when it is unset;
// `viewBox` is the user-space rectangle the viewport shows, which defaults to the viewport's
// own size from the origin.
case class Svg
  ( width:      Float,
    height:     Float,
    defs:       List[Svg.Def]         = Nil,
    figures:    List[Figure]          = Nil,
    transforms: List[Transform]       = Nil,
    unit:       Optional[Svg.Units]   = Unset,
    viewBox:    Optional[Svg.ViewBox] = Unset )
extends Documentary:

  type Self = Svg
  type Metadata = Encoding

  private[savagery] def markup: Xml =
    val suffix: Text = unit.lay(t"")(_.suffix)

    val box: Svg.ViewBox = viewBox.or(Svg.ViewBox(0, 0, width, height))

    val attrs: Ledger[Text, Text] =
      Ledger
        ( t"xmlns"   -> t"http://www.w3.org/2000/svg",
          t"viewBox" -> box.show,
          t"width"   -> t"${width.show}$suffix",
          t"height"  -> t"${height.show}$suffix" )

    // The `@font-face` rules for every typeface the lettering names, as a `<style>` among the
    // definitions, so the SVG carries the fonts it uses and renders alike wherever it is shown.
    val css: Text = FontFace.stylesheet(Figure.fonts(figures)).show

    val styleElement: List[Xml] =
      if css == t"" then Nil
      else List(Xml.Element(t"style", Attributes.empty, List[Xml](Xml.Text(css)).nodes))

    val defsElement: List[Xml] =
      if defs.nil && styleElement.nil then Nil
      else List(Xml.Element(t"defs", Attributes.empty, (styleElement + defs.map(_.markup)).nodes))

    val figureNodes: List[Xml] =
      if transforms.nil then figures.map(_.markup)
      else
        val groupAttrs =
          Ledger(t"transform" -> transforms.map(_.encode).join(t" "))

        List(Xml.Element(t"g", Attributes.from(groupAttrs.to[Map]), figures.map(_.markup).nodes))

    val children: Array[Xml.Node]^{} = (defsElement + figureNodes).nodes
    Xml.Element(t"svg", Attributes.from(attrs.to[Map]), children)
