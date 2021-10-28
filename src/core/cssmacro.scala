/*
    Cataract, version 0.6.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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

import annotation.targetName
import language.dynamics

private[cataract] type Label = String & Singleton

object Macro:
  def rule(selector: Expr[Selector], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Rule] =
    '{Rule($selector, ${read(props)})}

  def keyframe(name: Expr[String], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Keyframe] =
    '{Keyframe(Txt($name), ${read(props)})}

  def read(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Style] =
    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[CssProperty]] =
      exprs match
        case '{($key: k & Label, $value: v)} +: tail =>
          val att = key.valueOrAbort
          val exp: Expr[PropertyDef[k & Label, v]] =
            Expr.summon[PropertyDef[k & Label, v]].getOrElse {
              val typeName = TypeRepr.of[v].show
              report.errorAndAbort(
                  s"cataract: no valid CSS element $att taking values of type $typeName exists")
            }
          
          '{CssProperty(dashed(Txt($key)), $exp.show($value))} :: recur(tail)
        case _ =>
          Nil
    
    properties match
      case Varargs(exprs) =>
        '{Style(${Expr.ofSeq(recur(exprs))}*)}
      case _ =>
        report.errorAndAbort("cataract: expected varargs")

  private def words(string: Txt): List[Txt] =
    try
      val i = string.indexWhere(_.isUpper, 1)
      string.take(i).lower :: words(string.drop(i))
    catch case OutOfRangeError(_, _, _) => List(string.lower)
    

  private[cataract] def dashed(string: Txt): Txt = words(string).join(str"-")

case class PropertyDef[Name <: Label, -T: ShowProperty]():
  def show(value: T): Txt = summon[ShowProperty[T]].show(value)

object Selectable:
  given ident: Selectable[Selector] = identity(_)

  given [T](using sel: clairvoyant.CssSelection[T]): Selectable[T] = sel.selection(_) match
    case s".$cls" => Selector.Class(Txt(cls))
    case s"#$id"  => Selector.Id(Txt(id))
    case elem     => Selector.Element(Txt(elem))

trait Selectable[-T]:
  def selector(value: T): Selector

extension [T: Selectable](left: T)

  @targetName("definedAs")
  infix def :=(css: Style): Rule = Rule(summon[Selectable[T]].selector(left), css)

  @targetName("descendant")
  infix def >>[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) >> summon[Selectable[S]].selector(right)
  
  @targetName("child")
  infix def >[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) > summon[Selectable[S]].selector(right)
  
  @targetName("after")
  infix def +[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) + summon[Selectable[S]].selector(right)
  
  @targetName("or")
  infix def |[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) | summon[Selectable[S]].selector(right)
  
  @targetName("and")
  infix def &[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) & summon[Selectable[S]].selector(right)
  
  @targetName("before")
  infix def ~[S: Selectable](right: S): Selector =
    summon[Selectable[T]].selector(left) ~ summon[Selectable[S]].selector(right)
  
object PropertyDef:
  given alignContent: PropertyDef["alignContent", Txt] = PropertyDef()
  given alignItems: PropertyDef["alignItems", Txt] = PropertyDef()
  given alignSelf: PropertyDef["alignSelf", Txt] = PropertyDef()
  given all: PropertyDef["all", Txt] = PropertyDef()
  given animation: PropertyDef["animation", Txt] = PropertyDef()
  given animationDelay: PropertyDef["animationDelay", Duration] = PropertyDef()
  given animationDirection: PropertyDef["animationDirection", Txt] = PropertyDef()
  given animationDuration: PropertyDef["animationDuration", Duration] = PropertyDef()
  given animationFillMode: PropertyDef["animationFillMode", AnimationFillMode] = PropertyDef()
  given animationIterationCount: PropertyDef["animationIterationCount", Txt] = PropertyDef()
  given animationName: PropertyDef["animationName", Txt] = PropertyDef()
  given animationPlayState: PropertyDef["animationPlayState", Txt] = PropertyDef()
  given animationTimingFunction: PropertyDef["animationTimingFunction", Txt] = PropertyDef()
  given backfaceVisibility: PropertyDef["backfaceVisibility", Txt] = PropertyDef()
  given background: PropertyDef["background", Txt] = PropertyDef()
  given backgroundAttachment: PropertyDef["backgroundAttachment", Txt] = PropertyDef()
  given backgroundBlendMode: PropertyDef["backgroundBlendMode", Txt] = PropertyDef()
  given backgroundClip: PropertyDef["backgroundClip", Txt] = PropertyDef()
  given backgroundColor1: PropertyDef["backgroundColor", Color] = PropertyDef()
  given backgroundColor2: PropertyDef["backgroundColor", Transparent.type] = PropertyDef()
  given backgroundImage: PropertyDef["backgroundImage", Txt] = PropertyDef()
  given backgroundOrigin: PropertyDef["backgroundOrigin", Txt] = PropertyDef()
  given backgroundPosition: PropertyDef["backgroundPosition", Txt] = PropertyDef()
  given backgroundRepeat: PropertyDef["backgroundRepeat", Txt] = PropertyDef()
  given backgroundSize: PropertyDef["backgroundSize", Txt] = PropertyDef()
  given border: PropertyDef["border", (BorderStyle, Dimension, Color)] = PropertyDef()
  given borderBottom: PropertyDef["borderBottom", (BorderStyle, Dimension, Color)] = PropertyDef()
  given borderBottomColor1: PropertyDef["borderBottomColor", Color] = PropertyDef()
  given borderBottomColor2: PropertyDef["borderBottomColor", Transparent.type] = PropertyDef()
  given borderBottomLeftRadius: PropertyDef["borderBottomLeftRadius", Dimension] = PropertyDef()
  given borderBottomRightRadius: PropertyDef["borderBottomRightRadius", Dimension] = PropertyDef()
  given borderBottomStyle: PropertyDef["borderBottomStyle", BorderStyle] = PropertyDef()
  given borderBottomWidth: PropertyDef["borderBottomWidth", Dimension] = PropertyDef()
  given borderCollapse: PropertyDef["borderCollapse", Txt] = PropertyDef()
  given borderColor1: PropertyDef["borderColor", Color] = PropertyDef()
  given borderColor2: PropertyDef["borderColor", Transparent.type] = PropertyDef()
  given borderImage: PropertyDef["borderImage", Txt] = PropertyDef()
  given borderImageOutset: PropertyDef["borderImageOutset", Txt] = PropertyDef()
  given borderImageRepeat: PropertyDef["borderImageRepeat", Txt] = PropertyDef()
  given borderImageSlice: PropertyDef["borderImageSlice", Txt] = PropertyDef()
  given borderImageSource: PropertyDef["borderImageSource", Txt] = PropertyDef()
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
  given boxDecorationBreak: PropertyDef["boxDecorationBreak", Txt] = PropertyDef()
  given boxShadow: PropertyDef["boxShadow", (Dimension, Dimension, Dimension, Color)] = PropertyDef()
  given boxSizing: PropertyDef["boxSizing", Txt] = PropertyDef()
  given breakAfter: PropertyDef["breakAfter", Txt] = PropertyDef()
  given breakBefore: PropertyDef["breakBefore", Txt] = PropertyDef()
  given breakInside: PropertyDef["breakInside", Txt] = PropertyDef()
  given captionSide: PropertyDef["captionSide", Txt] = PropertyDef()
  given caretColor1: PropertyDef["caretColor", Color] = PropertyDef()
  given caretColor2: PropertyDef["caretColor", Transparent.type] = PropertyDef()
  given clear: PropertyDef["clear", Txt] = PropertyDef()
  given clip: PropertyDef["clip", Txt] = PropertyDef()
  given color1: PropertyDef["color", Color] = PropertyDef()
  given color2: PropertyDef["color", Transparent.type] = PropertyDef()
  given columnCount: PropertyDef["columnCount", Txt] = PropertyDef()
  given columnFill: PropertyDef["columnFill", Txt] = PropertyDef()
  given columnGap: PropertyDef["columnGap", Txt] = PropertyDef()
  given columnRule: PropertyDef["columnRule", Txt] = PropertyDef()
  given columnRuleColor1: PropertyDef["columnRuleColor", Color] = PropertyDef()
  given columnRuleColor2: PropertyDef["columnRuleColor", Transparent.type] = PropertyDef()
  given columnRuleStyle: PropertyDef["columnRuleStyle", Txt] = PropertyDef()
  given columnRuleWidth: PropertyDef["columnRuleWidth", Txt] = PropertyDef()
  given columnSpan: PropertyDef["columnSpan", Txt] = PropertyDef()
  given columnWidth: PropertyDef["columnWidth", Txt] = PropertyDef()
  given columns: PropertyDef["columns", Txt] = PropertyDef()
  given content: PropertyDef["content", Txt] = PropertyDef()
  given counterIncrement: PropertyDef["counterIncrement", Txt] = PropertyDef()
  given counterReset: PropertyDef["counterReset", Txt] = PropertyDef()
  given cursor: PropertyDef["cursor", Cursor] = PropertyDef()
  given direction: PropertyDef["direction", Txt] = PropertyDef()
  given display: PropertyDef["display", Display] = PropertyDef()
  given emptyCells: PropertyDef["emptyCells", Txt] = PropertyDef()
  given filter: PropertyDef["filter", Txt] = PropertyDef()
  given flex: PropertyDef["flex", Txt] = PropertyDef()
  given flexBasis: PropertyDef["flexBasis", Txt] = PropertyDef()
  given flexDirection: PropertyDef["flexDirection", Txt] = PropertyDef()
  given flexFlow: PropertyDef["flexFlow", Txt] = PropertyDef()
  given flexGrow: PropertyDef["flexGrow", Txt] = PropertyDef()
  given flexShrink: PropertyDef["flexShrink", Txt] = PropertyDef()
  given flexWrap: PropertyDef["flexWrap", Txt] = PropertyDef()
  given float: PropertyDef["float", Float] = PropertyDef()
  given font: PropertyDef["font", Txt] = PropertyDef()
  given fontFamily: PropertyDef["fontFamily", Font] = PropertyDef()
  given fontFeatureSettings: PropertyDef["fontFeatureSettings", Txt] = PropertyDef()
  given fontKerning: PropertyDef["fontKerning", Txt] = PropertyDef()
  given fontLanguageOverride: PropertyDef["fontLanguageOverride", Txt] = PropertyDef()
  given fontSize: PropertyDef["fontSize", Dimension] = PropertyDef()
  given fontSizeAdjust: PropertyDef["fontSizeAdjust", Txt] = PropertyDef()
  given fontStretch: PropertyDef["fontStretch", Txt] = PropertyDef()
  given fontStyle: PropertyDef["fontStyle", FontStyle] = PropertyDef()
  given fontSynthesis: PropertyDef["fontSynthesis", Txt] = PropertyDef()
  given fontVariant: PropertyDef["fontVariant", Txt] = PropertyDef()
  given fontVariantAlternates: PropertyDef["fontVariantAlternates", Txt] = PropertyDef()
  given fontVariantCaps: PropertyDef["fontVariantCaps", Txt] = PropertyDef()
  given fontVariantEastAsian: PropertyDef["fontVariantEastAsian", Txt] = PropertyDef()
  given fontVariantLigatures: PropertyDef["fontVariantLigatures", Txt] = PropertyDef()
  given fontVariantNumeric: PropertyDef["fontVariantNumeric", Txt] = PropertyDef()
  given fontVariantPosition: PropertyDef["fontVariantPosition", Txt] = PropertyDef()
  given fontWeight1: PropertyDef["fontWeight", Int] = PropertyDef()
  given fontWeight2: PropertyDef["fontWeight", FontWeight] = PropertyDef()
  given gap: PropertyDef["gap", Txt] = PropertyDef()
  given grid: PropertyDef["grid", Txt] = PropertyDef()
  given gridArea: PropertyDef["gridArea", Txt] = PropertyDef()
  given gridAutoColumns: PropertyDef["gridAutoColumns", Txt] = PropertyDef()
  given gridAutoFlow: PropertyDef["gridAutoFlow", Txt] = PropertyDef()
  given gridAutoRows: PropertyDef["gridAutoRows", Txt] = PropertyDef()
  given gridColumn: PropertyDef["gridColumn", Txt] = PropertyDef()
  given gridColumnEnd: PropertyDef["gridColumnEnd", Txt] = PropertyDef()
  given gridColumnGap: PropertyDef["gridColumnGap", Txt] = PropertyDef()
  given gridColumnStart: PropertyDef["gridColumnStart", Txt] = PropertyDef()
  given gridGap: PropertyDef["gridGap", Txt] = PropertyDef()
  given gridRow: PropertyDef["gridRow", Txt] = PropertyDef()
  given gridRowEnd: PropertyDef["gridRowEnd", Txt] = PropertyDef()
  given gridRowGap: PropertyDef["gridRowGap", Txt] = PropertyDef()
  given gridRowStart: PropertyDef["gridRowStart", Txt] = PropertyDef()
  given gridTemplate: PropertyDef["gridTemplate", Txt] = PropertyDef()
  given gridTemplateAreas: PropertyDef["gridTemplateAreas", Txt] = PropertyDef()
  given gridTemplateColumns: PropertyDef["gridTemplateColumns", Txt] = PropertyDef()
  given gridTemplateRows: PropertyDef["gridTemplateRows", Txt] = PropertyDef()
  given hangingPunctuation: PropertyDef["hangingPunctuation", Txt] = PropertyDef()
  given height: PropertyDef["height", Dimension] = PropertyDef()
  given hyphens: PropertyDef["hyphens", Txt] = PropertyDef()
  given imageRendering: PropertyDef["imageRendering", Txt] = PropertyDef()
  given isolation: PropertyDef["isolation", Txt] = PropertyDef()
  given justifyContent: PropertyDef["justifyContent", Txt] = PropertyDef()
  given left: PropertyDef["left", Dimension] = PropertyDef()
  given letterSpacing: PropertyDef["letterSpacing", Txt] = PropertyDef()
  given lineBreak: PropertyDef["lineBreak", Txt] = PropertyDef()
  given lineHeight: PropertyDef["lineHeight", Dimension] = PropertyDef()
  given listStyle: PropertyDef["listStyle", Txt] = PropertyDef()
  given listStyleImage: PropertyDef["listStyleImage", Txt] = PropertyDef()
  given listStylePosition: PropertyDef["listStylePosition", Txt] = PropertyDef()
  given listStyleType: PropertyDef["listStyleType", Txt] = PropertyDef()
  given margin1: PropertyDef["margin", Dimension] = PropertyDef()
  given margin2: PropertyDef["margin", (Dimension, Dimension)] = PropertyDef()
  given margin3: PropertyDef["margin", (Dimension, Dimension, Dimension)] = PropertyDef()
  given margin4: PropertyDef["margin", (Dimension, Dimension, Dimension, Dimension)] = PropertyDef()
  given marginBottom: PropertyDef["marginBottom", Dimension] = PropertyDef()
  given marginLeft: PropertyDef["marginLeft", Dimension] = PropertyDef()
  given marginRight: PropertyDef["marginRight", Dimension] = PropertyDef()
  given marginTop: PropertyDef["marginTop", Dimension] = PropertyDef()
  given mask: PropertyDef["mask", Txt] = PropertyDef()
  given maskType: PropertyDef["maskType", Txt] = PropertyDef()
  given maxHeight: PropertyDef["maxHeight", Dimension] = PropertyDef()
  given maxWidth: PropertyDef["maxWidth", Dimension] = PropertyDef()
  given minHeight: PropertyDef["minHeight", Dimension] = PropertyDef()
  given minWidth: PropertyDef["minWidth", Dimension] = PropertyDef()
  given mixBlendMode: PropertyDef["mixBlendMode", MixBlendMode] = PropertyDef()
  given objectFit: PropertyDef["objectFit", Txt] = PropertyDef()
  given objectPosition: PropertyDef["objectPosition", Txt] = PropertyDef()
  given opacity: PropertyDef["opacity", Txt] = PropertyDef()
  given order: PropertyDef["order", Txt] = PropertyDef()
  given orphans: PropertyDef["orphans", Txt] = PropertyDef()
  given outline: PropertyDef["outline", Txt] = PropertyDef()
  given outlineColor1: PropertyDef["outlineColor", Color] = PropertyDef()
  given outlineColor2: PropertyDef["outlineColor", Transparent.type] = PropertyDef()
  given outlineOffset: PropertyDef["outlineOffset", Txt] = PropertyDef()
  given outlineStyle: PropertyDef["outlineStyle", Txt] = PropertyDef()
  given outlineWidth: PropertyDef["outlineWidth", Txt] = PropertyDef()
  given over: PropertyDef["over", Txt] = PropertyDef()
  given overflowWrap: PropertyDef["overflowWrap", Txt] = PropertyDef()
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
  given pageBreakAfter: PropertyDef["pageBreakAfter", Txt] = PropertyDef()
  given pageBreakBefore: PropertyDef["pageBreakBefore", Txt] = PropertyDef()
  given pageBreakInside: PropertyDef["pageBreakInside", Txt] = PropertyDef()
  given perspective: PropertyDef["perspective", Txt] = PropertyDef()
  given perspectiveOrigin: PropertyDef["perspectiveOrigin", Txt] = PropertyDef()
  given pointerEvents: PropertyDef["pointerEvents", PointerEvents] = PropertyDef()
  given position: PropertyDef["position", Position] = PropertyDef()
  given quotes: PropertyDef["quotes", Txt] = PropertyDef()
  given resize: PropertyDef["resize", Txt] = PropertyDef()
  given right: PropertyDef["right", Dimension] = PropertyDef()
  given rowGap: PropertyDef["rowGap", Txt] = PropertyDef()
  given scrollBehavior: PropertyDef["scrollBehavior", Txt] = PropertyDef()
  given tabSize: PropertyDef["tabSize", Txt] = PropertyDef()
  given tableLayout: PropertyDef["tableLayout", Txt] = PropertyDef()
  given textAlign: PropertyDef["textAlign", TextAlign] = PropertyDef()
  given textAlignLast: PropertyDef["textAlignLast", TextAlign] = PropertyDef()
  given textCombineUpright: PropertyDef["textCombineUpright", Txt] = PropertyDef()
  given textDecoration1: PropertyDef["textDecoration", TextDecorationLine] = PropertyDef()
  
  given textDecoration2: PropertyDef["textDecoration", (TextDecorationLine, Txt,
      TextDecorationStyle)] = PropertyDef()

  given textDecorationColor1: PropertyDef["textDecorationColor", Color] = PropertyDef()
  given textDecorationColor2: PropertyDef["textDecorationColor", Transparent.type] = PropertyDef()
  given textDecorationLine: PropertyDef["textDecorationLine", TextDecorationLine] = PropertyDef()
  given textDecorationStyle: PropertyDef["textDecorationStyle", TextDecorationStyle] = PropertyDef()
  given textIndent: PropertyDef["textIndent", Dimension] = PropertyDef()
  given textJustify: PropertyDef["textJustify", Txt] = PropertyDef()
  given textOrientation: PropertyDef["textOrientation", Txt] = PropertyDef()
  given textOverflow: PropertyDef["textOverflow", Txt] = PropertyDef()
  given textShadow: PropertyDef["textShadow", Txt] = PropertyDef()
  given textTransform: PropertyDef["textTransform", Txt] = PropertyDef()
  given textUnderlinePosition: PropertyDef["textUnderlinePosition", Txt] = PropertyDef()
  given top: PropertyDef["top", Dimension] = PropertyDef()
  given transform: PropertyDef["transform", Txt] = PropertyDef()
  given transformOrigin: PropertyDef["transformOrigin", Txt] = PropertyDef()
  given transformStyle: PropertyDef["transformStyle", Txt] = PropertyDef()
  given transition: PropertyDef["transition", Txt] = PropertyDef()
  given transitionDelay: PropertyDef["transitionDelay", Txt] = PropertyDef()
  given transitionDuration: PropertyDef["transitionDuration", Txt] = PropertyDef()
  given transitionProperty: PropertyDef["transitionProperty", Txt] = PropertyDef()
  given transitionTimingFunction: PropertyDef["transitionTimingFunction", Txt] = PropertyDef()
  given unicodeBidi: PropertyDef["unicodeBidi", Txt] = PropertyDef()
  given userSelect: PropertyDef["userSelect", UserSelect] = PropertyDef()
  given verticalAlign1: PropertyDef["verticalAlign", VerticalAlign] = PropertyDef()
  given verticalAlign2: PropertyDef["verticalAlign", Dimension] = PropertyDef()
  given visibility: PropertyDef["visibility", Txt] = PropertyDef()
  given whiteSpace: PropertyDef["whiteSpace", Txt] = PropertyDef()
  given widows: PropertyDef["widows", Txt] = PropertyDef()
  given width: PropertyDef["width", Dimension] = PropertyDef()
  given wordBreak: PropertyDef["wordBreak", Txt] = PropertyDef()
  given wordSpacing: PropertyDef["wordSpacing", Txt] = PropertyDef()
  given wordWrap: PropertyDef["wordWrap", Txt] = PropertyDef()
  given writingMode: PropertyDef["writingMode", Txt] = PropertyDef()
  given zIndex: PropertyDef["zIndex", Int] = PropertyDef()

  given inherit[L <: Label]: PropertyDef[L, Inherit.type] = PropertyDef()
  given initial[L <: Label]: PropertyDef[L, Initial.type] = PropertyDef()
  given transparent[L <: Label]: PropertyDef[L, Transparent.type] = PropertyDef()

object Inherit
object Transparent
object Initial

type Dimension = Length | Int

object ShowProperty:
  given ShowProperty[Length] = _.text
  given ShowProperty[Duration] = _.text
  given ShowProperty[Dimension] =
    case length: Length => length.text
    case int: Int       => Txt(int.toString)

  given [A: ShowProperty, B: ShowProperty]: ShowProperty[(A, B)] = tuple =>
    str"${summon[ShowProperty[A]].show(tuple(0))} ${summon[ShowProperty[B]].show(tuple(1))}"
  
  given [A: ShowProperty, B: ShowProperty, C: ShowProperty]: ShowProperty[(A, B, C)] = tuple =>
    List(summon[ShowProperty[A]].show(tuple(0)), summon[ShowProperty[B]].show(tuple(1)),
        summon[ShowProperty[C]].show(tuple(2))).join(str" ")
  
  given [A: ShowProperty, B: ShowProperty, C: ShowProperty, D: ShowProperty]
        : ShowProperty[(A, B, C, D)] = tuple =>
    List(summon[ShowProperty[A]].show(tuple(0)), summon[ShowProperty[B]].show(tuple(1)),
        summon[ShowProperty[C]].show(tuple(2)), summon[ShowProperty[D]].show(tuple(3)))
      .join(str" ")
  
  given ShowProperty[Font] = _.names.map { f =>
    if f.contains(str" ") then str"'$f'" else f
  }.join(str", ")

  given ShowProperty[Txt] = identity(_)
  given ShowProperty[Int] = int => Txt(int.toString)
  given ShowProperty[Srgb] = _.css
  given ShowProperty[Hsl] = _.css
  given ShowProperty[Color] = _.standardSrgb.css
  given ShowProperty[PropertyValue] = c => Macro.dashed(c.text)
  given ShowProperty[Inherit.type] = c => str"inherit"
  given ShowProperty[Transparent.type] = c => str"transparent"
  given ShowProperty[Initial.type] = c => str"initial"

trait ShowProperty[-T]:
  def show(value: T): Txt

trait PropertyValue:
  def text: Txt = Txt(this.toString).lower

enum Duration:
  case S(value: Double)
  case Ms(value: Double)

  def text: Txt = this match
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
  case Calc(value: Txt)

  def text: Txt = this match
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

  @targetName("add")
  infix def +(dim: Length): Length = infixOp(" + ", dim)
  
  @targetName("sub")
  infix def -(dim: Length): Length = infixOp(" - ", dim)
  
  @targetName("mul")
  infix def *(double: Double): Length = infixOp(" * ", double)
  
  @targetName("div")
  infix def /(double: Double): Length = infixOp(" / ", double)

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
  def sec = Duration.S(value)
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

case class Font(names: Txt*)

enum Dir:
  case Rtl, Ltr

package pseudo:
  def dir(direction: Dir) = Selector.PseudoClass(str"dir(${direction.toString.lower})")
  def lang(language: String) = Selector.PseudoClass(str"lang($language)")
  val after = Selector.PseudoClass(str":after")
  val before = Selector.PseudoClass(str":before")
  val selection = Selector.PseudoClass(str":selection")
  val firstLetter = Selector.PseudoClass(str":first-letter")
  val firstLine = Selector.PseudoClass(str":first-line")
  val marker = Selector.PseudoClass(str":marker")
  val placeholder = Selector.PseudoClass(str":placeholder")
  val anyLink = Selector.PseudoClass(str"any-link")
  val link = Selector.PseudoClass(str"link")
  val visited = Selector.PseudoClass(str"visited")
  val localLink = Selector.PseudoClass(str"local-link")
  val target = Selector.PseudoClass(str"target")
  val targetWithin = Selector.PseudoClass(str"target-within")
  val scope = Selector.PseudoClass(str"scope")
  val hover = Selector.PseudoClass(str"hover")
  val active = Selector.PseudoClass(str"active")
  val focus = Selector.PseudoClass(str"focus")
  val focusVisible = Selector.PseudoClass(str"focus-visible")
  val focusWithin = Selector.PseudoClass(str"focus-within")
  val current = Selector.PseudoClass(str"current")
  val past = Selector.PseudoClass(str"past")
  val future = Selector.PseudoClass(str"future")
  val playing = Selector.PseudoClass(str"playing")
  val paused = Selector.PseudoClass(str"paused")
  val autofill = Selector.PseudoClass(str"autofill")
  val enabled = Selector.PseudoClass(str"enabled")
  val disabled = Selector.PseudoClass(str"disabled")
  val readOnly = Selector.PseudoClass(str"read-only")
  val readWrite = Selector.PseudoClass(str"read-write")
  val placeholderShown = Selector.PseudoClass(str"placeholder-shown")
  val default = Selector.PseudoClass(str"default")
  val checked = Selector.PseudoClass(str"checked")
  val indeterminate = Selector.PseudoClass(str"indeterminate")
  val blank = Selector.PseudoClass(str"blank")
  val valid = Selector.PseudoClass(str"valid")
  val invalid = Selector.PseudoClass(str"invalid")
  val inRange = Selector.PseudoClass(str"in-range")
  val outOfRange = Selector.PseudoClass(str"out-of-range")
  val required = Selector.PseudoClass(str"required")
  val optional = Selector.PseudoClass(str"option")
  val userInvalid = Selector.PseudoClass(str"user-invalid")
  val root = Selector.PseudoClass(str"root")
  val empty = Selector.PseudoClass(str"empty")
  
  def nthChild(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-child(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  def nthLastChild(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-last-child(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  val firstChild = Selector.PseudoClass(str"first-child")
  val lastChild = Selector.PseudoClass(str"last-child")
  val onlyChild = Selector.PseudoClass(str"only-child")
  
  def nthOfType(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-of-type(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  def nthLastOfType(a: Int, b: Int) =
    Selector.PseudoClass(str"nth-last-of-type(${if b == 0 then str"${a}n+$b" else str"${a}n"})")
  
  val firstOfType = Selector.PseudoClass(str"first-of-type")
  val lastOfType = Selector.PseudoClass(str"last-of-type")
  val onlyOfType = Selector.PseudoClass(str"only-of-type")
