/*
    Cataclysm, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cataclysm

import rudiments.*
import fulminate.*
import gossamer.*
import serpentine.*
import anticipation.*
import spectacular.*

import scala.quoted.*

import annotation.targetName
import language.dynamics

private[cataclysm] type Label = String & Singleton

// FIXME
given Show[Double] = _.toString.show

object Cataclysm:
  def rule(selector: Expr[Selector], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[CssRule] =
    '{CssRule($selector, ${read(props)})}

  def keyframe(name: Expr[String], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Keyframe] =
    '{Keyframe(Text($name), ${read(props)})}

  def read(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[CssStyle] =
    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[CssProperty]] = exprs match
      case '{type keyType <: Label; ($key: keyType, $value: valueType)} +: tail =>
        val exp: Expr[PropertyDef[keyType, valueType]] = Expr.summon[PropertyDef[keyType, valueType]].getOrElse:
          val typeName = TypeRepr.of[valueType].show
          fail(msg"no valid CSS element ${key.valueOrAbort} taking values of type $typeName exists")
        
        '{CssProperty(Text($key).uncamel.kebab, $exp.show($value))} :: recur(tail)
      
      case _ =>
        Nil
    
    (properties: @unchecked) match
      case Varargs(exprs) => '{CssStyle(${Expr.ofSeq(recur(exprs))}*)}

case class PropertyDef[Name <: Label, -PropertyType: ShowProperty]():
  def show(value: PropertyType): Text = summon[ShowProperty[PropertyType]].show(value)

object Selectable:
  given ident: Selectable[Selector] = identity(_)

  given [SelectableType](using sel: GenericCssSelection[SelectableType]): Selectable[SelectableType] =
    sel.selection(_).s match
      case s".$cls" => Selector.Class(cls.tt)
      case s"#$id"  => Selector.Id(id.tt)
      case elem     => Selector.Element(elem.tt)

trait Selectable[-SelectorType]:
  def selector(value: SelectorType): Selector

def select[SelectorType](sel: SelectorType)(using selectable: Selectable[SelectorType])(css: CssStyle) =
  CssRule(selectable.selector(sel), css)

extension [SelectorType: Selectable](left: SelectorType)(using selectable: Selectable[SelectorType])
  @targetName("descendant")
  infix def >>[SelectorType2](right: SelectorType2)(using selectable2: Selectable[SelectorType2]): Selector =
    selectable.selector(left) >> selectable2.selector(right)
  
  @targetName("after")
  infix def ~[SelectorType2](right: SelectorType2)(using selectable2: Selectable[SelectorType2]): Selector =
    selectable.selector(left) + selectable2.selector(right)
  
  @targetName("or")
  infix def ||[SelectorType2](right: SelectorType2)(using selectable2: Selectable[SelectorType2]): Selector =
    selectable.selector(left) | selectable2.selector(right)
  
  @targetName("and")
  infix def &&[SelectorType2](right: SelectorType2)(using selectable2: Selectable[SelectorType2]): Selector =
    selectable.selector(left) & selectable2.selector(right)
  
  @targetName("before")
  infix def ~~[SelectorType2](right: SelectorType2)(using selectable2: Selectable[SelectorType2]): Selector =
    selectable.selector(left) ~ selectable2.selector(right)
  
object PropertyDef:
  given alignContent: PropertyDef["alignContent", Text] = PropertyDef()
  given alignItems: PropertyDef["alignItems", Text] = PropertyDef()
  given alignSelf: PropertyDef["alignSelf", Text] = PropertyDef()
  given all: PropertyDef["all", Text] = PropertyDef()
  given animation: PropertyDef["animation", Text] = PropertyDef()
  given animationDelay: PropertyDef["animationDelay", Duration] = PropertyDef()
  given animationDirection: PropertyDef["animationDirection", Text] = PropertyDef()
  given animationDuration: PropertyDef["animationDuration", Duration] = PropertyDef()
  given animationFillMode: PropertyDef["animationFillMode", AnimationFillMode] = PropertyDef()
  given animationIterationCount: PropertyDef["animationIterationCount", Text] = PropertyDef()
  given animationName: PropertyDef["animationName", Text] = PropertyDef()
  given animationPlayState: PropertyDef["animationPlayState", Text] = PropertyDef()
  given animationTimingFunction: PropertyDef["animationTimingFunction", Text] = PropertyDef()
  given backfaceVisibility: PropertyDef["backfaceVisibility", Text] = PropertyDef()
  given background: PropertyDef["background", Text] = PropertyDef()
  given backgroundAttachment: PropertyDef["backgroundAttachment", Text] = PropertyDef()
  given backgroundBlendMode: PropertyDef["backgroundBlendMode", Text] = PropertyDef()
  given backgroundClip: PropertyDef["backgroundClip", Text] = PropertyDef()
  given backgroundColor1[ColorType: RgbColor]: PropertyDef["backgroundColor", ColorType] = PropertyDef()
  given backgroundColor2: PropertyDef["backgroundColor", Transparent.type] = PropertyDef()
  given backgroundImage: PropertyDef["backgroundImage", Text] = PropertyDef()
  //given backgroundImage2: PropertyDef["backgroundImage", Relative] = PropertyDef()
  given backgroundImage3[PathType: GenericPath]: PropertyDef["backgroundImage", PathType] = PropertyDef()
  given backgroundImage4: PropertyDef["backgroundImage", SimplePath] = PropertyDef()
  given backgroundOrigin: PropertyDef["backgroundOrigin", Text] = PropertyDef()
  given backgroundPosition: PropertyDef["backgroundPosition", Text] = PropertyDef()
  given backgroundPosition2: PropertyDef["backgroundPosition", Dimension] = PropertyDef()
  given backgroundPosition3: PropertyDef["backgroundPosition", (Dimension, Dimension)] = PropertyDef()
  given backgroundRepeat: PropertyDef["backgroundRepeat", Text] = PropertyDef()
  given backgroundRepeat2: PropertyDef["backgroundRepeat", BackgroundRepeat] = PropertyDef()
  
  given backgroundRepeat3: PropertyDef["backgroundRepeat", (BackgroundRepeat, BackgroundRepeat)] =
      PropertyDef()
  
  given backgroundSize: PropertyDef["backgroundSize", Text] = PropertyDef()
  given backgroundSize2: PropertyDef["backgroundSize", Dimension] = PropertyDef()
  given border[ColorType: RgbColor]: PropertyDef["border", (BorderStyle, Dimension, ColorType)] = PropertyDef()
  given borderBottom[ColorType: RgbColor]: PropertyDef["borderBottom", (BorderStyle, Dimension, ColorType)] = PropertyDef()
  given borderBottomColor1[ColorType: RgbColor]: PropertyDef["borderBottomColor", ColorType] = PropertyDef()
  given borderBottomColor2: PropertyDef["borderBottomColor", Transparent.type] = PropertyDef()
  given borderBottomLeftRadius: PropertyDef["borderBottomLeftRadius", Dimension] = PropertyDef()
  given borderBottomRightRadius: PropertyDef["borderBottomRightRadius", Dimension] = PropertyDef()
  given borderBottomStyle: PropertyDef["borderBottomStyle", BorderStyle] = PropertyDef()
  given borderBottomWidth: PropertyDef["borderBottomWidth", Dimension] = PropertyDef()
  given borderCollapse: PropertyDef["borderCollapse", Text] = PropertyDef()
  given borderColor1[ColorType: RgbColor]: PropertyDef["borderColor", ColorType] = PropertyDef()
  given borderColor2: PropertyDef["borderColor", Transparent.type] = PropertyDef()
  given borderImage: PropertyDef["borderImage", Text] = PropertyDef()
  given borderImageOutset: PropertyDef["borderImageOutset", Text] = PropertyDef()
  given borderImageRepeat: PropertyDef["borderImageRepeat", Text] = PropertyDef()
  given borderImageSlice: PropertyDef["borderImageSlice", Text] = PropertyDef()
  given borderImageSource: PropertyDef["borderImageSource", Text] = PropertyDef()
  given borderImageWidth: PropertyDef["borderImageWidth", Dimension] = PropertyDef()
  given borderLeft[ColorType: RgbColor]: PropertyDef["borderLeft", (BorderStyle, Dimension, ColorType)] = PropertyDef()
  given borderLeftColor1[ColorType: RgbColor]: PropertyDef["borderLeftColor", ColorType] = PropertyDef()
  given borderLeftColor2: PropertyDef["borderLeftColor", Transparent.type] = PropertyDef()
  given borderLeftStyle: PropertyDef["borderLeftStyle", BorderStyle] = PropertyDef()
  given borderLeftWidth: PropertyDef["borderLeftWidth", Dimension] = PropertyDef()
  given borderRadius: PropertyDef["borderRadius", Dimension] = PropertyDef()
  given borderRight[ColorType: RgbColor]: PropertyDef["borderRight", (BorderStyle, Dimension, ColorType)] = PropertyDef()
  given borderRightColor1[ColorType: RgbColor]: PropertyDef["borderRightColor", ColorType] = PropertyDef()
  given borderRightColor2: PropertyDef["borderRightColor", Transparent.type] = PropertyDef()
  given borderRightStyle: PropertyDef["borderRightStyle", BorderStyle] = PropertyDef()
  given borderRightWidth: PropertyDef["borderRightWidth", Dimension] = PropertyDef()
  given borderSpacing: PropertyDef["borderSpacing", Dimension] = PropertyDef()
  given borderStyle: PropertyDef["borderStyle", BorderStyle] = PropertyDef()
  given borderTop[ColorType: RgbColor]: PropertyDef["borderTop", (BorderStyle, Dimension, ColorType)] = PropertyDef()
  given borderTopColor1[ColorType: RgbColor]: PropertyDef["borderTopColor", ColorType] = PropertyDef()
  given borderTopColor2: PropertyDef["borderTopColor", Transparent.type] = PropertyDef()
  given borderTopLeftRadius: PropertyDef["borderTopLeftRadius", Dimension] = PropertyDef()
  given borderTopRightRadius: PropertyDef["borderTopRightRadius", Dimension] = PropertyDef()
  given borderTopStyle: PropertyDef["borderTopStyle", BorderStyle] = PropertyDef()
  given borderTopWidth: PropertyDef["borderTopWidth", Dimension] = PropertyDef()
  given borderWidth: PropertyDef["borderWidth", Dimension] = PropertyDef()
  given bottom: PropertyDef["bottom", Dimension] = PropertyDef()
  given boxDecorationBreak: PropertyDef["boxDecorationBreak", Text] = PropertyDef()
  given boxShadow[ColorType: RgbColor]: PropertyDef["boxShadow", (Dimension, Dimension, Dimension, ColorType)] = PropertyDef()
  given boxSizing: PropertyDef["boxSizing", Text] = PropertyDef()
  given breakAfter: PropertyDef["breakAfter", Text] = PropertyDef()
  given breakBefore: PropertyDef["breakBefore", Text] = PropertyDef()
  given breakInside: PropertyDef["breakInside", Text] = PropertyDef()
  given captionSide: PropertyDef["captionSide", Text] = PropertyDef()
  given caretColor1[ColorType: RgbColor]: PropertyDef["caretColor", ColorType] = PropertyDef()
  given caretColor2: PropertyDef["caretColor", Transparent.type] = PropertyDef()
  given clear: PropertyDef["clear", Text] = PropertyDef()
  given clip: PropertyDef["clip", Text] = PropertyDef()
  given color1[ColorType: RgbColor]: PropertyDef["color", ColorType] = PropertyDef()
  given color2: PropertyDef["color", Transparent.type] = PropertyDef()
  given columnCount: PropertyDef["columnCount", Text] = PropertyDef()
  given columnFill: PropertyDef["columnFill", Text] = PropertyDef()
  given columnGap: PropertyDef["columnGap", Text] = PropertyDef()
  given columnRule: PropertyDef["columnRule", Text] = PropertyDef()
  given columnRuleColor1[ColorType: RgbColor]: PropertyDef["columnRuleColor", ColorType] = PropertyDef()
  given columnRuleColor2: PropertyDef["columnRuleColor", Transparent.type] = PropertyDef()
  given columnRuleStyle: PropertyDef["columnRuleStyle", Text] = PropertyDef()
  given columnRuleWidth: PropertyDef["columnRuleWidth", Text] = PropertyDef()
  given columnSpan: PropertyDef["columnSpan", Text] = PropertyDef()
  given columnWidth: PropertyDef["columnWidth", Text] = PropertyDef()
  given columns: PropertyDef["columns", Text] = PropertyDef()
  given content: PropertyDef["content", Text] = PropertyDef()
  given counterIncrement: PropertyDef["counterIncrement", Text] = PropertyDef()
  given counterReset: PropertyDef["counterReset", Text] = PropertyDef()
  given cursor: PropertyDef["cursor", Cursor] = PropertyDef()
  given direction: PropertyDef["direction", Text] = PropertyDef()
  given display: PropertyDef["display", Display] = PropertyDef()
  given emptyCells: PropertyDef["emptyCells", Text] = PropertyDef()
  given filter: PropertyDef["filter", Text] = PropertyDef()
  given flex: PropertyDef["flex", Text] = PropertyDef()
  given flexBasis: PropertyDef["flexBasis", Text] = PropertyDef()
  given flexDirection: PropertyDef["flexDirection", Text] = PropertyDef()
  given flexFlow: PropertyDef["flexFlow", Text] = PropertyDef()
  given flexGrow: PropertyDef["flexGrow", Text] = PropertyDef()
  given flexShrink: PropertyDef["flexShrink", Text] = PropertyDef()
  given flexWrap: PropertyDef["flexWrap", Text] = PropertyDef()
  given float: PropertyDef["float", Float] = PropertyDef()
  given font: PropertyDef["font", Text] = PropertyDef()
  given fontFamily: PropertyDef["fontFamily", Font] = PropertyDef()
  given fontFeatureSettings: PropertyDef["fontFeatureSettings", Text] = PropertyDef()
  given fontKerning: PropertyDef["fontKerning", Text] = PropertyDef()
  given fontLanguageOverride: PropertyDef["fontLanguageOverride", Text] = PropertyDef()
  given fontSize: PropertyDef["fontSize", Dimension] = PropertyDef()
  given fontSizeAdjust: PropertyDef["fontSizeAdjust", Text] = PropertyDef()
  given fontStretch: PropertyDef["fontStretch", Text] = PropertyDef()
  given fontStyle: PropertyDef["fontStyle", FontStyle] = PropertyDef()
  given fontSynthesis: PropertyDef["fontSynthesis", Text] = PropertyDef()
  given fontVariant: PropertyDef["fontVariant", Text] = PropertyDef()
  given fontVariantAlternates: PropertyDef["fontVariantAlternates", Text] = PropertyDef()
  given fontVariantCaps: PropertyDef["fontVariantCaps", Text] = PropertyDef()
  given fontVariantEastAsian: PropertyDef["fontVariantEastAsian", Text] = PropertyDef()
  given fontVariantLigatures: PropertyDef["fontVariantLigatures", Text] = PropertyDef()
  given fontVariantNumeric: PropertyDef["fontVariantNumeric", Text] = PropertyDef()
  given fontVariantPosition: PropertyDef["fontVariantPosition", Text] = PropertyDef()
  given fontWeight1: PropertyDef["fontWeight", Int] = PropertyDef()
  given fontWeight2: PropertyDef["fontWeight", FontWeight] = PropertyDef()
  given gap: PropertyDef["gap", Text] = PropertyDef()
  given grid: PropertyDef["grid", Text] = PropertyDef()
  given gridArea: PropertyDef["gridArea", Text] = PropertyDef()
  given gridAutoColumns: PropertyDef["gridAutoColumns", Text] = PropertyDef()
  given gridAutoFlow: PropertyDef["gridAutoFlow", Text] = PropertyDef()
  given gridAutoRows: PropertyDef["gridAutoRows", Text] = PropertyDef()
  given gridColumn: PropertyDef["gridColumn", Text] = PropertyDef()
  given gridColumnEnd: PropertyDef["gridColumnEnd", Text] = PropertyDef()
  given gridColumnGap: PropertyDef["gridColumnGap", Text] = PropertyDef()
  given gridColumnStart: PropertyDef["gridColumnStart", Text] = PropertyDef()
  given gridGap: PropertyDef["gridGap", Text] = PropertyDef()
  given gridRow: PropertyDef["gridRow", Text] = PropertyDef()
  given gridRowEnd: PropertyDef["gridRowEnd", Text] = PropertyDef()
  given gridRowGap: PropertyDef["gridRowGap", Text] = PropertyDef()
  given gridRowStart: PropertyDef["gridRowStart", Text] = PropertyDef()
  given gridTemplate: PropertyDef["gridTemplate", Text] = PropertyDef()
  given gridTemplateAreas: PropertyDef["gridTemplateAreas", Text] = PropertyDef()
  given gridTemplateColumns: PropertyDef["gridTemplateColumns", Text] = PropertyDef()
  given gridTemplateRows: PropertyDef["gridTemplateRows", Text] = PropertyDef()
  given hangingPunctuation: PropertyDef["hangingPunctuation", Text] = PropertyDef()
  given height: PropertyDef["height", Dimension] = PropertyDef()
  given hyphens: PropertyDef["hyphens", Text] = PropertyDef()
  given imageRendering: PropertyDef["imageRendering", Text] = PropertyDef()
  given isolation: PropertyDef["isolation", Text] = PropertyDef()
  given justifyContent: PropertyDef["justifyContent", Text] = PropertyDef()
  given left: PropertyDef["left", Dimension] = PropertyDef()
  given letterSpacing: PropertyDef["letterSpacing", Text] = PropertyDef()
  given lineBreak: PropertyDef["lineBreak", Text] = PropertyDef()
  given lineHeight: PropertyDef["lineHeight", Dimension] = PropertyDef()
  given listStyle: PropertyDef["listStyle", Text] = PropertyDef()
  given listStyleImage: PropertyDef["listStyleImage", Text] = PropertyDef()
  given listStylePosition: PropertyDef["listStylePosition", Text] = PropertyDef()
  given listStyleType: PropertyDef["listStyleType", Text] = PropertyDef()
  given margin1: PropertyDef["margin", Dimension] = PropertyDef()
  given margin2: PropertyDef["margin", (Dimension, Dimension)] = PropertyDef()
  given margin3: PropertyDef["margin", (Dimension, Dimension, Dimension)] = PropertyDef()
  given margin4: PropertyDef["margin", (Dimension, Dimension, Dimension, Dimension)] = PropertyDef()
  given marginBottom: PropertyDef["marginBottom", Dimension] = PropertyDef()
  given marginLeft: PropertyDef["marginLeft", Dimension] = PropertyDef()
  given marginRight: PropertyDef["marginRight", Dimension] = PropertyDef()
  given marginTop: PropertyDef["marginTop", Dimension] = PropertyDef()
  given mask: PropertyDef["mask", Text] = PropertyDef()
  given maskType: PropertyDef["maskType", Text] = PropertyDef()
  given maxHeight: PropertyDef["maxHeight", Dimension] = PropertyDef()
  given maxWidth: PropertyDef["maxWidth", Dimension] = PropertyDef()
  given minHeight: PropertyDef["minHeight", Dimension] = PropertyDef()
  given minWidth: PropertyDef["minWidth", Dimension] = PropertyDef()
  given mixBlendMode: PropertyDef["mixBlendMode", MixBlendMode] = PropertyDef()
  given objectFit: PropertyDef["objectFit", Text] = PropertyDef()
  given objectPosition: PropertyDef["objectPosition", Text] = PropertyDef()
  given opacity: PropertyDef["opacity", Text] = PropertyDef()
  given order: PropertyDef["order", Text] = PropertyDef()
  given orphans: PropertyDef["orphans", Text] = PropertyDef()
  given outline: PropertyDef["outline", Text] = PropertyDef()
  given outlineColor1[ColorType: RgbColor]: PropertyDef["outlineColor", ColorType] = PropertyDef()
  given outlineColor2: PropertyDef["outlineColor", Transparent.type] = PropertyDef()
  given outlineOffset: PropertyDef["outlineOffset", Text] = PropertyDef()
  given outlineStyle: PropertyDef["outlineStyle", Text] = PropertyDef()
  given outlineWidth: PropertyDef["outlineWidth", Text] = PropertyDef()
  given over: PropertyDef["over", Text] = PropertyDef()
  given overflowWrap: PropertyDef["overflowWrap", Text] = PropertyDef()
  given overflow: PropertyDef["overflow", (Overflow, Overflow)] = PropertyDef()
  given overflowX: PropertyDef["overflowX", Overflow] = PropertyDef()
  given overflowY: PropertyDef["overflowY", Overflow] = PropertyDef()
  given padding1: PropertyDef["padding", Dimension] = PropertyDef()
  given padding2: PropertyDef["padding", (Dimension, Dimension)] = PropertyDef()
  given padding3: PropertyDef["padding", (Dimension, Dimension, Dimension)] = PropertyDef()
  given padding4: PropertyDef["padding", (Dimension, Dimension, Dimension, Dimension)] = PropertyDef()
  given paddingBottom: PropertyDef["paddingBottom", Dimension] = PropertyDef()
  given paddingLeft: PropertyDef["paddingLeft", Dimension] = PropertyDef()
  given paddingRight: PropertyDef["paddingRight", Dimension] = PropertyDef()
  given paddingTop: PropertyDef["paddingTop", Dimension] = PropertyDef()
  given pageBreakAfter: PropertyDef["pageBreakAfter", Text] = PropertyDef()
  given pageBreakBefore: PropertyDef["pageBreakBefore", Text] = PropertyDef()
  given pageBreakInside: PropertyDef["pageBreakInside", Text] = PropertyDef()
  given perspective: PropertyDef["perspective", Text] = PropertyDef()
  given perspectiveOrigin: PropertyDef["perspectiveOrigin", Text] = PropertyDef()
  given pointerEvents: PropertyDef["pointerEvents", PointerEvents] = PropertyDef()
  given position: PropertyDef["position", Position] = PropertyDef()
  given quotes: PropertyDef["quotes", Text] = PropertyDef()
  given resize: PropertyDef["resize", Text] = PropertyDef()
  given right: PropertyDef["right", Dimension] = PropertyDef()
  given rowGap: PropertyDef["rowGap", Text] = PropertyDef()
  given scrollBehavior: PropertyDef["scrollBehavior", Text] = PropertyDef()
  given scrollbarWidth: PropertyDef["scrollbarWidth", Text] = PropertyDef()
  given tabSize: PropertyDef["tabSize", Text] = PropertyDef()
  given tableLayout: PropertyDef["tableLayout", Text] = PropertyDef()
  given textAlign: PropertyDef["textAlign", TextAlign] = PropertyDef()
  given textAlignLast: PropertyDef["textAlignLast", TextAlign] = PropertyDef()
  given textCombineUpright: PropertyDef["textCombineUpright", Text] = PropertyDef()
  given textDecoration1: PropertyDef["textDecoration", TextDecorationLine] = PropertyDef()
  
  given textDecoration2: PropertyDef["textDecoration", (TextDecorationLine, Text, TextDecorationStyle)] =
    PropertyDef()

  given textDecorationColor1[ColorType: RgbColor]: PropertyDef["textDecorationColor", ColorType] = PropertyDef()
  given textDecorationColor2: PropertyDef["textDecorationColor", Transparent.type] = PropertyDef()
  given textDecorationLine: PropertyDef["textDecorationLine", TextDecorationLine] = PropertyDef()
  given textDecorationStyle: PropertyDef["textDecorationStyle", TextDecorationStyle] = PropertyDef()
  given textIndent: PropertyDef["textIndent", Dimension] = PropertyDef()
  given textJustify: PropertyDef["textJustify", Text] = PropertyDef()
  given textOrientation: PropertyDef["textOrientation", Text] = PropertyDef()
  given textOverflow: PropertyDef["textOverflow", Text] = PropertyDef()
  given textShadow: PropertyDef["textShadow", Text] = PropertyDef()
  given textTransform: PropertyDef["textTransform", Text] = PropertyDef()
  given textUnderlinePosition: PropertyDef["textUnderlinePosition", Text] = PropertyDef()
  given top: PropertyDef["top", Dimension] = PropertyDef()
  given transform: PropertyDef["transform", Text] = PropertyDef()
  given transformOrigin: PropertyDef["transformOrigin", Text] = PropertyDef()
  given transformStyle: PropertyDef["transformStyle", Text] = PropertyDef()
  given transition: PropertyDef["transition", Text] = PropertyDef()
  given transitionDelay: PropertyDef["transitionDelay", Text] = PropertyDef()
  given transitionDuration: PropertyDef["transitionDuration", Text] = PropertyDef()
  given transitionProperty: PropertyDef["transitionProperty", Text] = PropertyDef()
  given transitionTimingFunction: PropertyDef["transitionTimingFunction", Text] = PropertyDef()
  given unicodeBidi: PropertyDef["unicodeBidi", Text] = PropertyDef()
  given userSelect: PropertyDef["userSelect", UserSelect] = PropertyDef()
  given verticalAlign1: PropertyDef["verticalAlign", VerticalAlign] = PropertyDef()
  given verticalAlign2: PropertyDef["verticalAlign", Dimension] = PropertyDef()
  given visibility: PropertyDef["visibility", Text] = PropertyDef()
  given whiteSpace: PropertyDef["whiteSpace", Text] = PropertyDef()
  given widows: PropertyDef["widows", Text] = PropertyDef()
  given width: PropertyDef["width", Dimension] = PropertyDef()
  given wordBreak: PropertyDef["wordBreak", Text] = PropertyDef()
  given wordSpacing: PropertyDef["wordSpacing", Text] = PropertyDef()
  given wordWrap: PropertyDef["wordWrap", Text] = PropertyDef()
  given writingMode: PropertyDef["writingMode", Text] = PropertyDef()
  given zIndex: PropertyDef["zIndex", Int] = PropertyDef()

  given inherit[LabelType <: Label]: PropertyDef[LabelType, Inherit.type] = PropertyDef()
  given initial[LabelType <: Label]: PropertyDef[LabelType, Initial.type] = PropertyDef()
  given transparent[LabelType <: Label]: PropertyDef[LabelType, Transparent.type] = PropertyDef()

object Inherit
object Transparent
object Initial

type Dimension = Length | Int

object ShowProperty:
  given ShowProperty[Length] = _.show
  given ShowProperty[Duration] = _.show
  
  given ShowProperty[Dimension] =
    case length: Length => length.show
    case int: Int       => int.show

  given [PropertyType, PropertyType2]
      (using show: ShowProperty[PropertyType], show2: ShowProperty[PropertyType2])
      : ShowProperty[(PropertyType, PropertyType2)] = tuple =>
    t"${show.show(tuple(0))} ${show2.show(tuple(1))}"
  
  given [PropertyType, PropertyType2, PropertyType3]
      (using show: ShowProperty[PropertyType], show2: ShowProperty[PropertyType2],
          show3: ShowProperty[PropertyType3])
      : ShowProperty[(PropertyType, PropertyType2, PropertyType3)] = tuple =>
    List(show.show(tuple(0)), show2.show(tuple(1)), show3.show(tuple(2))).join(t" ")
  
  given [PropertyType, PropertyType2, PropertyType3, PropertyType4]
      (using show: ShowProperty[PropertyType], show2: ShowProperty[PropertyType2],
          show3: ShowProperty[PropertyType3], show4: ShowProperty[PropertyType4])
        : ShowProperty[(PropertyType, PropertyType2, PropertyType3, PropertyType4)] = tuple =>
    List(show.show(tuple(0)), show2.show(tuple(1)), show3.show(tuple(2)), show4.show(tuple(3))).join(t" ")
  
  given ShowProperty[Font] = _.names.map: f =>
    if f.contains(t" ") then t"'$f'" else f
  .join(t", ")

  given ShowProperty[SimplePath] = path => t"url('${path}')"

  given [PathType](using generic: GenericPath[PathType]): ShowProperty[PathType] =
    path => t"url('${path.pathText}')"

  given ShowProperty[Text] = identity(_)
  given ShowProperty[Int] = _.show
  
  given [ColorType](using rgbColor: RgbColor[ColorType]): ShowProperty[ColorType] = color =>
    t"rgb(${rgbColor.red(color)},${rgbColor.green(color)},${rgbColor.blue(color)})"
  
  //given ShowProperty[Relative] = rel => t"url('$rel')"
  //given ShowProperty[GenericPath] = rel => t"url('$rel')"
  given ShowProperty[PropertyValue] = _.show
  given ShowProperty[Inherit.type] = c => t"inherit"
  given ShowProperty[Transparent.type] = c => t"transparent"
  given ShowProperty[Initial.type] = c => t"initial"

trait ShowProperty[-PropertyType]:
  def show(value: PropertyType): Text

object PropertyValue:
  given Show[PropertyValue] = _.toString.show.uncamel.kebab

trait PropertyValue

object Duration:
  given Show[Duration] =
    case S(value)  => t"${value}s"
    case Ms(value) => t"${value}ms"

enum Duration:
  case S(value: Double)
  case Ms(value: Double)

def max(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function(t"max", _))
def min(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function(t"min", _))

object Length:
  given Show[Length] =
    case Auto        => t"auto"
    case Px(value)   => t"${value}px"
    case Pt(value)   => t"${value}pt"
    case In(value)   => t"${value}in"
    case Pc(value)   => t"${value}%"
    case Cm(value)   => t"${value}cm"
    case Mm(value)   => t"${value}mm"
    case Em(value)   => t"${value}em"
    case Ex(value)   => t"${value}ex"
    case Ch(value)   => t"${value}ch"
    case Rem(value)  => t"${value}rem"
    case Vw(value)   => t"${value}vw"
    case Vh(value)   => t"${value}vh"
    case Vmin(value) => t"${value}vmin"
    case Vmax(value) => t"${value}vmax"
    case Calc(calc)  => t"calc($calc)"

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
  case Calc(value: Text)

  @targetName("add")
  infix def +(dim: Length): Length = infixOp(t" + ", dim)
  
  @targetName("sub")
  infix def -(dim: Length): Length = infixOp(t" - ", dim)
  
  @targetName("mul")
  infix def *(double: Double): Length = infixOp(t" * ", double)
  
  @targetName("div")
  infix def /(double: Double): Length = infixOp(t" / ", double)

  private def infixOp(operator: Text, dim: Length | Double): Length.Calc = this match
    case Calc(calc) => dim match
      case double: Double => Calc(t"($calc)$operator${double}")
      case Calc(calc2)    => Calc(t"($calc)$operator($calc2)")
      case length: Length => Calc(t"($calc)$operator$length")
    
    case other => dim match
      case double: Double => Calc(t"${this.show}$operator$double")
      case Calc(calc2)    => Calc(t"${this.show}$operator($calc2)")
      case length: Length => Calc(t"${this.show}$operator$length")
    
  def function(name: Text, right: Length | Double): Length =
    Calc(t"$name(${infixOp(t", ", right).value})")

extension (value: Double)
  def sec: Duration = Duration.S(value)
  def ms: Duration = Duration.Ms(value)
  def px: Length = Length.Px(value)
  def pt: Length = Length.Pt(value)
  def in: Length = Length.In(value)
  def pc: Length = Length.Pc(value)
  def cm: Length = Length.Cm(value)
  def mm: Length = Length.Mm(value)
  def em: Length = Length.Em(value)
  def ex: Length = Length.Ex(value)
  def ch: Length = Length.Ch(value)
  def rem: Length = Length.Rem(value)
  def vw: Length = Length.Vw(value)
  def vh: Length = Length.Vh(value)
  def vmin: Length = Length.Vmin(value)
  def vmax: Length = Length.Vmax(value)

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

enum BackgroundRepeat extends PropertyValue:
  case NoRepeat, RepeatX, RepeatY, Repeat, Space, Round

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

case class Font(names: Text*)

enum Dir:
  case Rtl, Ltr

package pseudo:
  def dir(direction: Dir) = Selector.PseudoClass(t"dir(${direction.show.lower})")
  def has[T](sel: T)(using selectable: Selectable[T]) = Selector.PseudoClass(t"has(${selectable.selector(sel).value})")
  def webkitScrollbar = Selector.PseudoClass(t":-webkit-scrollbar")
  def lang(language: Text) = Selector.PseudoClass(t"lang($language)")
  val after = Selector.PseudoClass(t":after")
  val before = Selector.PseudoClass(t":before")
  val selection = Selector.PseudoClass(t":selection")
  val firstLetter = Selector.PseudoClass(t":first-letter")
  val firstLine = Selector.PseudoClass(t":first-line")
  val marker = Selector.PseudoClass(t":marker")
  val placeholder = Selector.PseudoClass(t":placeholder")
  val anyLink = Selector.PseudoClass(t"any-link")
  val link = Selector.PseudoClass(t"link")
  val visited = Selector.PseudoClass(t"visited")
  val localLink = Selector.PseudoClass(t"local-link")
  val target = Selector.PseudoClass(t"target")
  val targetWithin = Selector.PseudoClass(t"target-within")
  val scope = Selector.PseudoClass(t"scope")
  val hover = Selector.PseudoClass(t"hover")
  val active = Selector.PseudoClass(t"active")
  val focus = Selector.PseudoClass(t"focus")
  val focusVisible = Selector.PseudoClass(t"focus-visible")
  val focusWithin = Selector.PseudoClass(t"focus-within")
  val current = Selector.PseudoClass(t"current")
  val past = Selector.PseudoClass(t"past")
  val future = Selector.PseudoClass(t"future")
  val playing = Selector.PseudoClass(t"playing")
  val paused = Selector.PseudoClass(t"paused")
  val autofill = Selector.PseudoClass(t"autofill")
  val enabled = Selector.PseudoClass(t"enabled")
  val disabled = Selector.PseudoClass(t"disabled")
  val readOnly = Selector.PseudoClass(t"read-only")
  val readWrite = Selector.PseudoClass(t"read-write")
  val placeholderShown = Selector.PseudoClass(t"placeholder-shown")
  val default = Selector.PseudoClass(t"default")
  val checked = Selector.PseudoClass(t"checked")
  val indeterminate = Selector.PseudoClass(t"indeterminate")
  val blank = Selector.PseudoClass(t"blank")
  val valid = Selector.PseudoClass(t"valid")
  val invalid = Selector.PseudoClass(t"invalid")
  val inRange = Selector.PseudoClass(t"in-range")
  val outOfRange = Selector.PseudoClass(t"out-of-range")
  val required = Selector.PseudoClass(t"required")
  val optional = Selector.PseudoClass(t"option")
  val userInvalid = Selector.PseudoClass(t"user-invalid")
  val root = Selector.PseudoClass(t"root")
  val empty = Selector.PseudoClass(t"empty")
  
  private def expr(a: Int, b: Int): Text = if a == 0 then t"$b" else if b != 0 then t"${a}n+$b" else t"${a}n"
  def nthChild(a: Int, b: Int) = Selector.PseudoClass(t"nth-child(${expr(a, b)})")
  def nthLastChild(a: Int, b: Int) = Selector.PseudoClass(t"nth-last-child(${expr(a, b)})")
  def nthOfType(a: Int, b: Int) = Selector.PseudoClass(t"nth-of-type(${expr(a, b)})")
  def nthLastOfType(a: Int, b: Int) = Selector.PseudoClass(t"nth-last-of-type(${expr(a, b)})")
  
  val firstChild = Selector.PseudoClass(t"first-child")
  val lastChild = Selector.PseudoClass(t"last-child")
  val onlyChild = Selector.PseudoClass(t"only-child")
  val firstOfType = Selector.PseudoClass(t"first-of-type")
  val lastOfType = Selector.PseudoClass(t"last-of-type")
  val onlyOfType = Selector.PseudoClass(t"only-of-type")

