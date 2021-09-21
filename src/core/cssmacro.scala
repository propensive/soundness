/*
    Cataract, version 0.1.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cataract

import rudiments.*
import gossamer.*
import iridescence.*

import scala.quoted.*

import language.dynamics

private[cataract] type Label = String & Singleton

object Macro:
  def rule(selector: Expr[Selector], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Rule] =
    '{Rule($selector, ${read(props)})}

  def keyframe(name: Expr[String], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Keyframe] =
    '{Keyframe($name, ${read(props)})}

  def read(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Style] =
    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[CssProperty]] =
      exprs match
        case '{($key: k & Label, $value: v)} +: tail =>
          val att = key.valueOrError
          val exp: Expr[PropertyDef[k & Label, v]] =
            Expr.summon[PropertyDef[k & Label, v]].getOrElse {
              val typeName = TypeRepr.of[v].show
              report.throwError(
                  s"cataract: no valid CSS element $att taking values of type $typeName exists")
            }
          
          '{CssProperty(dashed($key), $exp.show($value))} :: recur(tail)
        case _ =>
          Nil
    
    properties match
      case Varargs(exprs) =>
        '{Style(${Expr.ofSeq(recur(exprs))}*)}
      case _ =>
        report.throwError("cataract: expected varargs")

  private def words(string: String): List[String] =
    val i = string.indexWhere(_.isUpper, 1)
    
    if i < 0 then List(string.lower)
    else string.take(i).lower :: words(string.drop(i))

  private[cataract] def dashed(string: String): String = words(string).join("-")

case class PropertyDef[Name <: Label, -T: ShowProperty]():
  def show(value: T): String = summon[ShowProperty[T]].show(value)

object Selectable:
  given ident: Selectable[Selector] = identity(_)

  given [T](using sel: clairvoyant.CssSelection[T]): Selectable[T] = sel.selection(_) match
    case s".$cls" => Selector.Class(cls)
    case s"#$id"  => Selector.Id(id)
    case elem     => Selector.Element(elem)

trait Selectable[-T]:
  def selector(value: T): Selector

extension [T: Selectable](left: T)

  def :=(css: Style): Rule = Rule(summon[Selectable[T]].selector(left), css)

  def >>[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) >> summon[Selectable[S]].selector(right)
  
  def +[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) + summon[Selectable[S]].selector(right)
  
  def |[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) | summon[Selectable[S]].selector(right)
  
  def &[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) & summon[Selectable[S]].selector(right)
  
  def ~[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) ~ summon[Selectable[S]].selector(right)
  
object PropertyDef:
  given alignContent: PropertyDef["alignContent", String] = PropertyDef()
  given alignItems: PropertyDef["alignItems", String] = PropertyDef()
  given alignSelf: PropertyDef["alignSelf", String] = PropertyDef()
  given all: PropertyDef["all", String] = PropertyDef()
  given animation: PropertyDef["animation", String] = PropertyDef()
  given animationDelay: PropertyDef["animationDelay", Duration] = PropertyDef()
  given animationDirection: PropertyDef["animationDirection", String] = PropertyDef()
  given animationDuration: PropertyDef["animationDuration", Duration] = PropertyDef()
  given animationFillMode: PropertyDef["animationFillMode", AnimationFillMode] = PropertyDef()
  given animationIterationCount: PropertyDef["animationIterationCount", String] = PropertyDef()
  given animationName: PropertyDef["animationName", String] = PropertyDef()(using _.toString)
  given animationPlayState: PropertyDef["animationPlayState", String] = PropertyDef()
  given animationTimingFunction: PropertyDef["animationTimingFunction", String] = PropertyDef()
  given backfaceVisibility: PropertyDef["backfaceVisibility", String] = PropertyDef()
  given background: PropertyDef["background", String] = PropertyDef()
  given backgroundAttachment: PropertyDef["backgroundAttachment", String] = PropertyDef()
  given backgroundBlendMode: PropertyDef["backgroundBlendMode", String] = PropertyDef()
  given backgroundClip: PropertyDef["backgroundClip", String] = PropertyDef()
  given backgroundColor1: PropertyDef["backgroundColor", Color] = PropertyDef()
  given backgroundColor2: PropertyDef["backgroundColor", Transparent.type] = PropertyDef()
  given backgroundImage: PropertyDef["backgroundImage", String] = PropertyDef()
  given backgroundOrigin: PropertyDef["backgroundOrigin", String] = PropertyDef()
  given backgroundPosition: PropertyDef["backgroundPosition", String] = PropertyDef()
  given backgroundRepeat: PropertyDef["backgroundRepeat", String] = PropertyDef()
  given backgroundSize: PropertyDef["backgroundSize", String] = PropertyDef()
  given border: PropertyDef["border", (BorderStyle, Dimension, Color)] = PropertyDef()
  given borderBottom: PropertyDef["borderBottom", (BorderStyle, Dimension, Color)] = PropertyDef()
  given borderBottomColor1: PropertyDef["borderBottomColor", Color] = PropertyDef()
  given borderBottomColor2: PropertyDef["borderBottomColor", Transparent.type] = PropertyDef()
  given borderBottomLeftRadius: PropertyDef["borderBottomLeftRadius", Dimension] = PropertyDef()
  given borderBottomRightRadius: PropertyDef["borderBottomRightRadius", Dimension] = PropertyDef()
  given borderBottomStyle: PropertyDef["borderBottomStyle", BorderStyle] = PropertyDef()
  given borderBottomWidth: PropertyDef["borderBottomWidth", Dimension] = PropertyDef()
  given borderCollapse: PropertyDef["borderCollapse", String] = PropertyDef()
  given borderColor1: PropertyDef["borderColor", Color] = PropertyDef()
  given borderColor2: PropertyDef["borderColor", Transparent.type] = PropertyDef()
  given borderImage: PropertyDef["borderImage", String] = PropertyDef()
  given borderImageOutset: PropertyDef["borderImageOutset", String] = PropertyDef()
  given borderImageRepeat: PropertyDef["borderImageRepeat", String] = PropertyDef()
  given borderImageSlice: PropertyDef["borderImageSlice", String] = PropertyDef()
  given borderImageSource: PropertyDef["borderImageSource", String] = PropertyDef()
  given borderImageWidth: PropertyDef["borderImageWidth", Dimension] = PropertyDef()
  given borderLeft: PropertyDef["borderLeft", (BorderStyle, Dimension, Color)] = PropertyDef()
  given borderLeftColor1: PropertyDef["borderLeftColor", Color] = PropertyDef()
  given borderLeftColor2: PropertyDef["borderLeftColor", Transparent.type] = PropertyDef()
  given borderLeftStyle: PropertyDef["borderLeftStyle", BorderStyle] = PropertyDef()
  given borderLeftWidth: PropertyDef["borderLeftWidth", Dimension] = PropertyDef()
  given borderRadius: PropertyDef["borderRadius", Dimension] = PropertyDef()
  given borderRight: PropertyDef["borderRight", (BorderStyle, Dimension, Color)] = PropertyDef()
  given borderRightColor1: PropertyDef["borderRightColor", Color] = PropertyDef()
  given borderRightColor2: PropertyDef["borderRightColor", Transparent.type] = PropertyDef()
  given borderRightStyle: PropertyDef["borderRightStyle", BorderStyle] = PropertyDef()
  given borderRightWidth: PropertyDef["borderRightWidth", Dimension] = PropertyDef()
  given borderSpacing: PropertyDef["borderSpacing", Dimension] = PropertyDef()
  given borderStyle: PropertyDef["borderStyle", BorderStyle] = PropertyDef()
  given borderTop: PropertyDef["borderTop", (BorderStyle, Dimension, Color)] = PropertyDef()
  given borderTopColor1: PropertyDef["borderTopColor", Color] = PropertyDef()
  given borderTopColor2: PropertyDef["borderTopColor", Transparent.type] = PropertyDef()
  given borderTopLeftRadius: PropertyDef["borderTopLeftRadius", Dimension] = PropertyDef()
  given borderTopRightRadius: PropertyDef["borderTopRightRadius", Dimension] = PropertyDef()
  given borderTopStyle: PropertyDef["borderTopStyle", BorderStyle] = PropertyDef()
  given borderTopWidth: PropertyDef["borderTopWidth", Dimension] = PropertyDef()
  given borderWidth: PropertyDef["borderWidth", Dimension] = PropertyDef()
  given bottom: PropertyDef["bottom", Dimension] = PropertyDef()
  given boxDecorationBreak: PropertyDef["boxDecorationBreak", String] = PropertyDef()
  given boxShadow: PropertyDef["boxShadow", (Dimension, Dimension, Dimension, Color)] = PropertyDef()
  given boxSizing: PropertyDef["boxSizing", String] = PropertyDef()
  given breakAfter: PropertyDef["breakAfter", String] = PropertyDef()
  given breakBefore: PropertyDef["breakBefore", String] = PropertyDef()
  given breakInside: PropertyDef["breakInside", String] = PropertyDef()
  given captionSide: PropertyDef["captionSide", String] = PropertyDef()
  given caretColor1: PropertyDef["caretColor", Color] = PropertyDef()
  given caretColor2: PropertyDef["caretColor", Transparent.type] = PropertyDef()
  given clear: PropertyDef["clear", String] = PropertyDef()
  given clip: PropertyDef["clip", String] = PropertyDef()
  given color1: PropertyDef["color", Color] = PropertyDef()
  given color2: PropertyDef["color", Transparent.type] = PropertyDef()
  given columnCount: PropertyDef["columnCount", String] = PropertyDef()
  given columnFill: PropertyDef["columnFill", String] = PropertyDef()
  given columnGap: PropertyDef["columnGap", String] = PropertyDef()
  given columnRule: PropertyDef["columnRule", String] = PropertyDef()
  given columnRuleColor1: PropertyDef["columnRuleColor", Color] = PropertyDef()
  given columnRuleColor2: PropertyDef["columnRuleColor", Transparent.type] = PropertyDef()
  given columnRuleStyle: PropertyDef["columnRuleStyle", String] = PropertyDef()
  given columnRuleWidth: PropertyDef["columnRuleWidth", String] = PropertyDef()
  given columnSpan: PropertyDef["columnSpan", String] = PropertyDef()
  given columnWidth: PropertyDef["columnWidth", String] = PropertyDef()
  given columns: PropertyDef["columns", String] = PropertyDef()
  given content: PropertyDef["content", String] = PropertyDef()
  given counterIncrement: PropertyDef["counterIncrement", String] = PropertyDef()
  given counterReset: PropertyDef["counterReset", String] = PropertyDef()
  given cursor: PropertyDef["cursor", Cursor] = PropertyDef()
  given direction: PropertyDef["direction", String] = PropertyDef()
  given display: PropertyDef["display", Display] = PropertyDef()
  given emptyCells: PropertyDef["emptyCells", String] = PropertyDef()
  given filter: PropertyDef["filter", String] = PropertyDef()
  given flex: PropertyDef["flex", String] = PropertyDef()
  given flexBasis: PropertyDef["flexBasis", String] = PropertyDef()
  given flexDirection: PropertyDef["flexDirection", String] = PropertyDef()
  given flexFlow: PropertyDef["flexFlow", String] = PropertyDef()
  given flexGrow: PropertyDef["flexGrow", String] = PropertyDef()
  given flexShrink: PropertyDef["flexShrink", String] = PropertyDef()
  given flexWrap: PropertyDef["flexWrap", String] = PropertyDef()
  given float: PropertyDef["float", Float] = PropertyDef()
  given font: PropertyDef["font", String] = PropertyDef()
  given fontFamily: PropertyDef["fontFamily", Font] = PropertyDef()
  given fontFeatureSettings: PropertyDef["fontFeatureSettings", String] = PropertyDef()
  given fontKerning: PropertyDef["fontKerning", String] = PropertyDef()
  given fontLanguageOverride: PropertyDef["fontLanguageOverride", String] = PropertyDef()
  given fontSize: PropertyDef["fontSize", Dimension] = PropertyDef()
  given fontSizeAdjust: PropertyDef["fontSizeAdjust", String] = PropertyDef()
  given fontStretch: PropertyDef["fontStretch", String] = PropertyDef()
  given fontStyle: PropertyDef["fontStyle", FontStyle] = PropertyDef()
  given fontSynthesis: PropertyDef["fontSynthesis", String] = PropertyDef()
  given fontVariant: PropertyDef["fontVariant", String] = PropertyDef()
  given fontVariantAlternates: PropertyDef["fontVariantAlternates", String] = PropertyDef()
  given fontVariantCaps: PropertyDef["fontVariantCaps", String] = PropertyDef()
  given fontVariantEastAsian: PropertyDef["fontVariantEastAsian", String] = PropertyDef()
  given fontVariantLigatures: PropertyDef["fontVariantLigatures", String] = PropertyDef()
  given fontVariantNumeric: PropertyDef["fontVariantNumeric", String] = PropertyDef()
  given fontVariantPosition: PropertyDef["fontVariantPosition", String] = PropertyDef()
  given fontWeight1: PropertyDef["fontWeight", Int] = PropertyDef()
  given fontWeight2: PropertyDef["fontWeight", FontWeight] = PropertyDef()
  given gap: PropertyDef["gap", String] = PropertyDef()
  given grid: PropertyDef["grid", String] = PropertyDef()
  given gridArea: PropertyDef["gridArea", String] = PropertyDef()
  given gridAutoColumns: PropertyDef["gridAutoColumns", String] = PropertyDef()
  given gridAutoFlow: PropertyDef["gridAutoFlow", String] = PropertyDef()
  given gridAutoRows: PropertyDef["gridAutoRows", String] = PropertyDef()
  given gridColumn: PropertyDef["gridColumn", String] = PropertyDef()
  given gridColumnEnd: PropertyDef["gridColumnEnd", String] = PropertyDef()
  given gridColumnGap: PropertyDef["gridColumnGap", String] = PropertyDef()
  given gridColumnStart: PropertyDef["gridColumnStart", String] = PropertyDef()
  given gridGap: PropertyDef["gridGap", String] = PropertyDef()
  given gridRow: PropertyDef["gridRow", String] = PropertyDef()
  given gridRowEnd: PropertyDef["gridRowEnd", String] = PropertyDef()
  given gridRowGap: PropertyDef["gridRowGap", String] = PropertyDef()
  given gridRowStart: PropertyDef["gridRowStart", String] = PropertyDef()
  given gridTemplate: PropertyDef["gridTemplate", String] = PropertyDef()
  given gridTemplateAreas: PropertyDef["gridTemplateAreas", String] = PropertyDef()
  given gridTemplateColumns: PropertyDef["gridTemplateColumns", String] = PropertyDef()
  given gridTemplateRows: PropertyDef["gridTemplateRows", String] = PropertyDef()
  given hangingPunctuation: PropertyDef["hangingPunctuation", String] = PropertyDef()
  given height: PropertyDef["height", Dimension] = PropertyDef()
  given hyphens: PropertyDef["hyphens", String] = PropertyDef()
  given imageRendering: PropertyDef["imageRendering", String] = PropertyDef()
  given isolation: PropertyDef["isolation", String] = PropertyDef()
  given justifyContent: PropertyDef["justifyContent", String] = PropertyDef()
  given left: PropertyDef["left", Dimension] = PropertyDef()
  given letterSpacing: PropertyDef["letterSpacing", String] = PropertyDef()
  given lineBreak: PropertyDef["lineBreak", String] = PropertyDef()
  given lineHeight: PropertyDef["lineHeight", Dimension] = PropertyDef()
  given listStyle: PropertyDef["listStyle", String] = PropertyDef()
  given listStyleImage: PropertyDef["listStyleImage", String] = PropertyDef()
  given listStylePosition: PropertyDef["listStylePosition", String] = PropertyDef()
  given listStyleType: PropertyDef["listStyleType", String] = PropertyDef()
  given margin1: PropertyDef["margin", Dimension] = PropertyDef()
  given margin2: PropertyDef["margin", (Dimension, Dimension)] = PropertyDef()
  given margin3: PropertyDef["margin", (Dimension, Dimension, Dimension)] = PropertyDef()
  given margin4: PropertyDef["margin", (Dimension, Dimension, Dimension, Dimension)] = PropertyDef()
  given marginBottom: PropertyDef["marginBottom", Dimension] = PropertyDef()
  given marginLeft: PropertyDef["marginLeft", Dimension] = PropertyDef()
  given marginRight: PropertyDef["marginRight", Dimension] = PropertyDef()
  given marginTop: PropertyDef["marginTop", Dimension] = PropertyDef()
  given mask: PropertyDef["mask", String] = PropertyDef()
  given maskType: PropertyDef["maskType", String] = PropertyDef()
  given maxHeight: PropertyDef["maxHeight", Dimension] = PropertyDef()
  given maxWidth: PropertyDef["maxWidth", Dimension] = PropertyDef()
  given minHeight: PropertyDef["minHeight", Dimension] = PropertyDef()
  given minWidth: PropertyDef["minWidth", Dimension] = PropertyDef()
  given mixBlendMode: PropertyDef["mixBlendMode", MixBlendMode] = PropertyDef()
  given objectFit: PropertyDef["objectFit", String] = PropertyDef()
  given objectPosition: PropertyDef["objectPosition", String] = PropertyDef()
  given opacity: PropertyDef["opacity", String] = PropertyDef()
  given order: PropertyDef["order", String] = PropertyDef()
  given orphans: PropertyDef["orphans", String] = PropertyDef()
  given outline: PropertyDef["outline", String] = PropertyDef()
  given outlineColor1: PropertyDef["outlineColor", Color] = PropertyDef()
  given outlineColor2: PropertyDef["outlineColor", Transparent.type] = PropertyDef()
  given outlineOffset: PropertyDef["outlineOffset", String] = PropertyDef()
  given outlineStyle: PropertyDef["outlineStyle", String] = PropertyDef()
  given outlineWidth: PropertyDef["outlineWidth", String] = PropertyDef()
  given over: PropertyDef["over", String] = PropertyDef()
  given overflowWrap: PropertyDef["overflowWrap", String] = PropertyDef()
  given overflow: PropertyDef["overflow", (Overflow, Overflow)] = PropertyDef()
  given overflowX: PropertyDef["overflowX", Overflow] = PropertyDef()
  given overflowY: PropertyDef["overflowY", Overflow] = PropertyDef()
  given padding1: PropertyDef["padding", Dimension] = PropertyDef()
  given padding2: PropertyDef["padding", (Dimension, Dimension)] = PropertyDef()
  given padding3: PropertyDef["padding", (Dimension, Dimension, Dimension)] = PropertyDef()
  
  given padding4: PropertyDef["padding", (Dimension, Dimension, Dimension, Dimension)] =
    PropertyDef()
  
  given paddingBottom: PropertyDef["paddingBottom", Dimension] = PropertyDef()
  given paddingLeft: PropertyDef["paddingLeft", Dimension] = PropertyDef()
  given paddingRight: PropertyDef["paddingRight", Dimension] = PropertyDef()
  given paddingTop: PropertyDef["paddingTop", Dimension] = PropertyDef()
  given pageBreakAfter: PropertyDef["pageBreakAfter", String] = PropertyDef()
  given pageBreakBefore: PropertyDef["pageBreakBefore", String] = PropertyDef()
  given pageBreakInside: PropertyDef["pageBreakInside", String] = PropertyDef()
  given perspective: PropertyDef["perspective", String] = PropertyDef()
  given perspectiveOrigin: PropertyDef["perspectiveOrigin", String] = PropertyDef()
  given pointerEvents: PropertyDef["pointerEvents", PointerEvents] = PropertyDef()
  given position: PropertyDef["position", Position] = PropertyDef()
  given quotes: PropertyDef["quotes", String] = PropertyDef()
  given resize: PropertyDef["resize", String] = PropertyDef()
  given right: PropertyDef["right", Dimension] = PropertyDef()
  given rowGap: PropertyDef["rowGap", String] = PropertyDef()
  given scrollBehavior: PropertyDef["scrollBehavior", String] = PropertyDef()
  given tabSize: PropertyDef["tabSize", String] = PropertyDef()
  given tableLayout: PropertyDef["tableLayout", String] = PropertyDef()
  given textAlign: PropertyDef["textAlign", TextAlign] = PropertyDef()
  given textAlignLast: PropertyDef["textAlignLast", TextAlign] = PropertyDef()
  given textCombineUpright: PropertyDef["textCombineUpright", String] = PropertyDef()
  given textDecoration1: PropertyDef["textDecoration", TextDecorationLine] = PropertyDef()
  
  given textDecoration2: PropertyDef["textDecoration", (TextDecorationLine, String,
      TextDecorationStyle)] = PropertyDef()

  given textDecorationColor1: PropertyDef["textDecorationColor", Color] = PropertyDef()
  given textDecorationColor2: PropertyDef["textDecorationColor", Transparent.type] = PropertyDef()
  given textDecorationLine: PropertyDef["textDecorationLine", TextDecorationLine] = PropertyDef()
  given textDecorationStyle: PropertyDef["textDecorationStyle", TextDecorationStyle] = PropertyDef()
  given textIndent: PropertyDef["textIndent", Dimension] = PropertyDef()
  given textJustify: PropertyDef["textJustify", String] = PropertyDef()
  given textOrientation: PropertyDef["textOrientation", String] = PropertyDef()
  given textOverflow: PropertyDef["textOverflow", String] = PropertyDef()
  given textShadow: PropertyDef["textShadow", String] = PropertyDef()
  given textTransform: PropertyDef["textTransform", String] = PropertyDef()
  given textUnderlinePosition: PropertyDef["textUnderlinePosition", String] = PropertyDef()
  given top: PropertyDef["top", Dimension] = PropertyDef()
  given transform: PropertyDef["transform", String] = PropertyDef()
  given transformOrigin: PropertyDef["transformOrigin", String] = PropertyDef()
  given transformStyle: PropertyDef["transformStyle", String] = PropertyDef()
  given transition: PropertyDef["transition", String] = PropertyDef()
  given transitionDelay: PropertyDef["transitionDelay", String] = PropertyDef()
  given transitionDuration: PropertyDef["transitionDuration", String] = PropertyDef()
  given transitionProperty: PropertyDef["transitionProperty", String] = PropertyDef()
  given transitionTimingFunction: PropertyDef["transitionTimingFunction", String] = PropertyDef()
  given unicodeBidi: PropertyDef["unicodeBidi", String] = PropertyDef()
  given userSelect: PropertyDef["userSelect", UserSelect] = PropertyDef()
  given verticalAlign1: PropertyDef["verticalAlign", VerticalAlign] = PropertyDef()
  given verticalAlign2: PropertyDef["verticalAlign", Dimension] = PropertyDef()
  given visibility: PropertyDef["visibility", String] = PropertyDef()
  given whiteSpace: PropertyDef["whiteSpace", String] = PropertyDef()
  given widows: PropertyDef["widows", String] = PropertyDef()
  given width: PropertyDef["width", Dimension] = PropertyDef()
  given wordBreak: PropertyDef["wordBreak", String] = PropertyDef()
  given wordSpacing: PropertyDef["wordSpacing", String] = PropertyDef()
  given wordWrap: PropertyDef["wordWrap", String] = PropertyDef()
  given writingMode: PropertyDef["writingMode", String] = PropertyDef()
  given zIndex: PropertyDef["zIndex", Int] = PropertyDef()

  given inherit[L <: Label]: PropertyDef[L, Inherit.type] = PropertyDef()
  given initial[L <: Label]: PropertyDef[L, Initial.type] = PropertyDef()
  given transparent[L <: Label]: PropertyDef[L, Transparent.type] = PropertyDef()

object Inherit
object Transparent
object Initial

type Dimension = Length | Int

object ShowProperty:
  given ShowProperty[Length] = _.toString
  given ShowProperty[Duration] = _.toString
  given ShowProperty[Dimension] = _.toString
  
  given [A: ShowProperty, B: ShowProperty]: ShowProperty[(A, B)] = tuple =>
    str"${summon[ShowProperty[A]].show(tuple(0))} ${summon[ShowProperty[B]].show(tuple(1))}"
  
  given [A: ShowProperty, B: ShowProperty, C: ShowProperty]: ShowProperty[(A, B, C)] = tuple =>
    List(summon[ShowProperty[A]].show(tuple(0)), summon[ShowProperty[B]].show(tuple(1)),
        summon[ShowProperty[C]].show(tuple(2))).join(" ")
  
  given [A: ShowProperty, B: ShowProperty, C: ShowProperty, D: ShowProperty]
        : ShowProperty[(A, B, C, D)] = tuple =>
    List(summon[ShowProperty[A]].show(tuple(0)), summon[ShowProperty[B]].show(tuple(1)),
        summon[ShowProperty[C]].show(tuple(2)), summon[ShowProperty[D]].show(tuple(3)))
      .join(" ")
  
  given ShowProperty[Font] = _.names.map { f =>
    if f.contains(" ") then str"'$f'" else f
  }.join(", ")

  given ShowProperty[String] = str => str""""$str""""
  given ShowProperty[Int] = _.toString
  given ShowProperty[Srgb] = _.css
  given ShowProperty[Hsl] = _.css
  given ShowProperty[Color] = _.standardSrgb.css
  given ShowProperty[PropertyValue] = c => Macro.dashed(c.toString)
  given ShowProperty[Inherit.type] = c => "inherit"
  given ShowProperty[Transparent.type] = c => "transparent"
  given ShowProperty[Initial.type] = c => "initial"

trait ShowProperty[-T]:
  def show(value: T): String

trait PropertyValue  

enum Duration:
  case S(value: Double)
  case Ms(value: Double)

  override def toString(): String = this match
    case S(value)  => str"${value.toString}s"
    case Ms(value) => str"${value.toString}ms"

def max(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function("max", _))
def min(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function("min", _))

enum Length:
  case Px(value: Double)
  case Pt(value: Double)
  case In(value: Double)
  case Auto
  case Pc(value: Double)
  case Cm(value: Double)
  case Mm(value: Double)
  case Em(value: Double)
  case Ex(value: Double)
  case Ch(value: Double)
  case Rem(value: Double)
  case Vw(value: Double)
  case Vh(value: Double)
  case Vmin(value: Double)
  case Vmax(value: Double)
  case Calc(value: String)

  override def toString(): String = this match
    case Px(value)   => str"${value.toString}px"
    case Pt(value)   => str"${value.toString}pt"
    case In(value)   => str"${value.toString}in"
    case Auto        => str"auto"
    case Pc(value)   => str"${value.toString}%"
    case Cm(value)   => str"${value.toString}cm"
    case Mm(value)   => str"${value.toString}mm"
    case Em(value)   => str"${value.toString}em"
    case Ex(value)   => str"${value.toString}ex"
    case Ch(value)   => str"${value.toString}ch"
    case Rem(value)  => str"${value.toString}rem"
    case Vw(value)   => str"${value.toString}vw"
    case Vh(value)   => str"${value.toString}vh"
    case Vmin(value) => str"${value.toString}vmin"
    case Vmax(value) => str"${value.toString}vmax"
    case Calc(calc)  => str"calc($calc)"

  def +(dim: Length): Length = infixOp(" + ", dim)
  def -(dim: Length): Length = infixOp(" - ", dim)
  def *(double: Double): Length = infixOp(" * ", double)
  def /(double: Double): Length = infixOp(" / ", double)

  private def infixOp(operator: String, dim: Length | Double): Length.Calc = this match
    case Calc(calc) => dim match
      case double: Double => Calc(str"($calc)$operator${double.toString}")
      case Calc(calc2)    => Calc(str"($calc)$operator($calc2)")
      case other          => Calc(str"($calc)$operator${other.toString}")
    case other => dim match
      case double: Double => Calc(str"$toString$operator${double.toString}")
      case Calc(calc2)    => Calc(str"$toString$operator($calc2)")
      case other          => Calc(str"$toString$operator${other.toString}")
    
  def function(name: String, right: Length | Double): Length =
    Calc(str"$name(${infixOp(", ", right).value})")

extension (value: Double)
  def s = Duration.S(value)
  def ms = Duration.Ms(value)
  def px = Length.Px(value)
  def pt = Length.Pt(value)
  def in = Length.In(value)
  def pc = Length.Pc(value)
  def cm = Length.Cm(value)
  def mm = Length.Mm(value)
  def em = Length.Em(value)
  def ex = Length.Ex(value)
  def ch = Length.Ch(value)
  def rem = Length.Rem(value)
  def vw = Length.Vw(value)
  def vh = Length.Vh(value)
  def vmin = Length.Vmin(value)
  def vmax = Length.Vmax(value)

enum Position extends PropertyValue:
  case Static, Absolute, Fixed, Relative, Sticky

enum FontWeight extends PropertyValue:
  case Normal, Bold, Bolder, Lighter

enum Display extends PropertyValue:
  case Inline, Block, Contents, Flex, Grid, InlineBlock, InlineFlex, InlineGrid, InlineTable,
      ListItem, RunIn, Table, TableCaption, TableColumnGroup, TableHeaderGroup, TableFooterGroup,
      TableRowGroup, TableCell, TableColumn, TableRow, None

enum Cursor extends PropertyValue:
  case Auto, Default, None, ContextMenu, Help, Pointer, Progress, Wait, Cell, Crosshair, Text,
      VerticalText, Alias, Copy, Move, NoDrop, NotAllowed, Grab, Grabbing, AllScroll, ColResize,
      RowResize, NResize, EResize, SResize, WResize, NeResize, NwResize, SeResize, SwResize, 
      EwResize, NsResize, NeswResize, NwseResize, ZoomIn, ZoomOut

enum UserSelect extends PropertyValue:
  case Auto, None, Text, All

enum TextDecorationLine extends PropertyValue:
  case None, Underline, Overline, LineThrough

enum TextDecorationStyle extends PropertyValue:
  case Solid, Double, Dotted, Dashed, Wavy

enum BorderStyle extends PropertyValue:
  case Dotted, Dashed, Solid, Double, Groove, Ridge, Inset, Outset, None, Hidden

enum Float extends PropertyValue:
  case None, Left, Right

enum TextAlign extends PropertyValue:
  case Left, Right, Center, Justify

enum MixBlendMode extends PropertyValue:
  case Normal, Multiply, Screen, Overlay, Darken, Lighten, ColorDodge, ColorDurn, Difference,
      Exclusion, Hue, Saturation, Color, Luminos

enum VerticalAlign extends PropertyValue:
  case Baseline, Sub, Super, TextTop, TextBottom, Middle, Top, Bottom, Revert, Unset

enum Overflow extends PropertyValue:
  case Visible, Hidden, Clip, Scroll, Auto

enum AnimationFillMode extends PropertyValue:
  case None, Forwards, Backwards, Both

enum PointerEvents extends PropertyValue:
  case Auto, None, VisiblePainted, VisibleFill, VisibleStroke, Visible, Painted, Fill, Stroke, All

enum FontStyle extends PropertyValue:
  case Normal, Italic, Oblique

case class Font(names: String*)

enum Dir:
  case Rtl, Ltr

package pseudo:
  def dir(direction: Dir) = Selector.PseudoClass(str"dir(${direction.toString.lower})")
  def lang(language: String) = Selector.PseudoClass(str"lang($language)")
  val after = Selector.PseudoClass(":after")
  val before = Selector.PseudoClass(":before")
  val selection = Selector.PseudoClass(":selection")
  val firstLetter = Selector.PseudoClass(":first-letter")
  val firstLine = Selector.PseudoClass(":first-line")
  val marker = Selector.PseudoClass(":marker")
  val placeholder = Selector.PseudoClass(":placeholder")
  val anyLink = Selector.PseudoClass("any-link")
  val link = Selector.PseudoClass("link")
  val visited = Selector.PseudoClass("visited")
  val localLink = Selector.PseudoClass("local-link")
  val target = Selector.PseudoClass("target")
  val targetWithin = Selector.PseudoClass("target-within")
  val scope = Selector.PseudoClass("scope")
  val hover = Selector.PseudoClass("hover")
  val active = Selector.PseudoClass("active")
  val focus = Selector.PseudoClass("focus")
  val focusVisible = Selector.PseudoClass("focus-visible")
  val focusWithin = Selector.PseudoClass("focus-within")
  val current = Selector.PseudoClass("current")
  val past = Selector.PseudoClass("past")
  val future = Selector.PseudoClass("future")
  val playing = Selector.PseudoClass("playing")
  val paused = Selector.PseudoClass("paused")
  val autofill = Selector.PseudoClass("autofill")
  val enabled = Selector.PseudoClass("enabled")
  val disabled = Selector.PseudoClass("disabled")
  val readOnly = Selector.PseudoClass("read-only")
  val readWrite = Selector.PseudoClass("read-write")
  val placeholderShown = Selector.PseudoClass("placeholder-shown")
  val default = Selector.PseudoClass("default")
  val checked = Selector.PseudoClass("checked")
  val indeterminate = Selector.PseudoClass("indeterminate")
  val blank = Selector.PseudoClass("blank")
  val valid = Selector.PseudoClass("valid")
  val invalid = Selector.PseudoClass("invalid")
  val inRange = Selector.PseudoClass("in-range")
  val outOfRange = Selector.PseudoClass("out-of-range")
  val required = Selector.PseudoClass("required")
  val optional = Selector.PseudoClass("option")
  val userInvalid = Selector.PseudoClass("user-invalid")
  val root = Selector.PseudoClass("root")
  val empty = Selector.PseudoClass("empty")
  
  def nthChild(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-child(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  def nthLastChild(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-last-child(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  val firstChild = Selector.PseudoClass("first-child")
  val lastChild = Selector.PseudoClass("last-child")
  val onlyChild = Selector.PseudoClass("only-child")
  
  def nthOfType(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-of-type(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  def nthLastOfType(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-last-of-type(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  val firstOfType = Selector.PseudoClass("first-of-type")
  val lastOfType = Selector.PseudoClass("last-of-type")
  val onlyOfType = Selector.PseudoClass("only-of-type")
