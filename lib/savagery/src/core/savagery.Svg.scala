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

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import xylophone.*
import zephyrine.*
import fulminate.*
import scala.collection.mutable.ListBuffer
import cardinality.*
import distillate.*
import geodesy.*
import iridescence.*
import symbolism.*

object Svg:
  given aggregable: (schema: XmlSchema)
  =>  (parseTactic: Tactic[Parse.Error], xmlTactic: Tactic[Xml.Error], svgTactic: Tactic[Svg.Error])
  =>  ((Svg is Aggregable by Text)^{parseTactic, xmlTactic, svgTactic}) =

    source =>
      val xml: Xml = summon[Xml is Aggregable by Text].aggregate(source)
      Svg.Parser.decodeSvg(Svg.Parser.rootElement(xml))

  given loadable: (XmlSchema)
  =>  (parseTactic: Tactic[Parse.Error])
  =>  (xmlTactic: Tactic[Xml.Error])
  =>  (svgTactic: Tactic[Svg.Error])
  =>  ((Svg is Loadable by Text)^{parseTactic, xmlTactic, svgTactic}) =

    source =>
      val xmlDoc: Document[Xml] = summon[(Xml is Loadable by Text)^].load(source)
      val svgElement = Svg.Parser.rootElement(xmlDoc.root)
      val parsedSvg: Svg = Svg.Parser.decodeSvg(svgElement)

      val encoding: Encoding =
        xmlDoc.metadata.encoding.let: name => Encoding.unapply(name).getOrElse(enc"UTF-8")
        . or(enc"UTF-8")

      Document[Svg](parsedSvg, encoding)

  given showable: [doc <: Document[Svg]] => doc is Showable =
    document =>
      val header = Header(t"1.0", document.metadata.name, Unset)

      val full: Xml = document.root.xml.absolve match
        case node: Node       => Fragment(header, node)
        case Fragment(nodes*) => Fragment((header +: nodes)*)

      full.show

  // SvgError → Svg.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NotAnSvg(label: Text)            extends Reason(1)
      case MalformedPathData(data: Text)    extends Reason(2)
      case MalformedColor(color: Text)      extends Reason(3)

    given communicable: Reason is Communicable =
      case Reason.NotAnSvg(label)         => m"the root element was <$label> instead of <svg>"
      case Reason.MalformedPathData(data) => m"the path data $data could not be parsed"
      case Reason.MalformedColor(color)   => m"the color $color could not be parsed"

  case class Error(reason: Svg.Error.Reason)(using Diagnostics)
  extends fulminate.Error(122, reason.number)(m"the SVG could not be parsed because $reason")

  // SvgParser → Svg.Parser
  object Parser:
    def labelOf(xml: Xml): Text = xml match
      case e: Element => e.label
      case _          => t"<unknown>"


    def findSvg(nodes: List[Node])(using Tactic[Svg.Error]): Element =
      nodes.reap { case e: Element if e.label == t"svg" => e }.or:
        abort(Svg.Error(Svg.Error.Reason.NotAnSvg(t"<missing>")))

    def rootElement(xml: Xml)(using Tactic[Svg.Error]): Element = xml match
      case e: Element if e.label == t"svg" => e
      case Fragment(nodes*)                => findSvg(nodes.to(List))

      case other =>
        abort(Svg.Error(Svg.Error.Reason.NotAnSvg(labelOf(other))))

    private def numAttr(elem: Element, name: Text, default: Float = 0.0f): Float =
      elem.attributes(name).let: text => safely(text.as[Double].toFloat).or(default)
      . or(default)

    def decodeSvg(elem: Element)(using Tactic[Svg.Error]): Svg =
      val width = numAttr(elem, t"width")
      val height = numAttr(elem, t"height")

      val defs = ListBuffer[Def]()
      val figures = ListBuffer[Figure]()

      def walk(parent: Element): Unit = parent.children.each:
        case child: Element => child.label match
          case t"defs" => child.children.each:
            case dd: Element => decodeSvgDef(dd).let: svgDef => defs += svgDef
            case _           => ()

          case t"g" =>
            walk(child)

          case _ =>
            decodeFigure(child).let: figure => figures += figure

        case _ =>
          ()

      walk(elem)
      Svg(width, height, defs.toList.to(List), figures.toList.to(List))

    private def decodeFigure(elem: Element)(using Tactic[Svg.Error]): Optional[Figure] =
      elem.label match
        case t"rect"    => decodeRectangle(elem)
        case t"circle"  => decodeCircle(elem)
        case t"ellipse" => decodeEllipse(elem)
        case t"path"    => decodePath(elem)
        case _          => Unset

    private def decodeRectangle(elem: Element): Rectangle =
      Rectangle
        ( Point(numAttr(elem, t"x"), numAttr(elem, t"y")),
         numAttr(elem, t"width"),
         numAttr(elem, t"height") )

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

    private def decodePath(elem: Element)(using Tactic[Svg.Error]): Outline =
      val d = elem.attributes(t"d").or(t"")
      val ops = parsePathData(d)
      val id = elem.attributes(t"id").let(Id(_))
      val transforms = elem.attributes(t"transform").let(parseTransforms).or(Nil)
      Outline(ops = ops.reverse, id = id, transforms = transforms)


    private def decodeSvgDef(elem: Element)
      ( using Tactic[Svg.Error] )
    :   Optional[Def] =

      elem.label match
        case t"linearGradient" => decodeLinearGradient(elem)
        case _                 => Unset


    private def decodeLinearGradient(elem: Element)
      ( using Tactic[Svg.Error] )
    :   LinearGradient[Color in Srgb] =

      val id = elem.attributes(t"id").let(Id(_)).or(Id(t""))

      val stops: List[Stop[Color in Srgb]] =

          elem.children.readable.toList.collect:
            case e: Element if e.label == t"stop" => decodeStop(e)
          . to(List)

      LinearGradient(id, stops*)


    private def decodeStop(elem: Element)(using Tactic[Svg.Error]): Stop[Color in Srgb] =
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

      ops.toList.to(List)

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

      xs.toList.to(List)

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
  sealed trait Def:
    def xml: Xml

  case class LinearGradient[color](id: Id, stops: Stop[color]*) extends Def:
    def xml: Xml =
      Element(t"linearGradient", Attributes(t"id" -> Id.text(id)), stops.map(_.xml).toSeq.nodes)

case class Svg
  ( width:      Float,
    height:     Float,
    defs:       List[Svg.Def]    = Nil,
    figures:    List[Figure]    = Nil,
    transforms: List[Transform] = Nil )
extends Documentary:

  type Self = Svg
  type Metadata = Encoding

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt

    val attrs: Ledger[Text, Text] =
      Ledger
        ( t"xmlns"   -> t"http://www.w3.org/2000/svg",
          t"viewBox" -> t"0 0 ${width.show} ${height.show}",
          t"width"   -> width.show,
          t"height"  -> height.show )

    val defsElement: List[Xml] =
      if defs.nil then Nil
      else List(Element(t"defs", Attributes.empty, defs.stdlib.map(_.xml).nodes))

    val figureNodes: List[Xml] =
      if transforms.nil then figures.stdlib.map(_.xml).to(List)
      else
        val groupAttrs =
          Ledger(t"transform" -> transforms.map(_.encode).join(t" "))
        List(Element(t"g", Attributes.from(groupAttrs.stdlib.to(Map)), figures.stdlib.map(_.xml).nodes))

    val children: Array[Node]^{} = (defsElement + figureNodes).stdlib.nodes
    Element(t"svg", Attributes.from(attrs.stdlib.to(Map)), children)
