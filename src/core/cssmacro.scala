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

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*

import scala.quoted.*
import scala.annotation.targetName

import language.dynamics

given Realm = realm"cataclysm"

private[cataclysm] type Label = String & Singleton

given Decimalizer as decimalizer = Decimalizer(6)

object Cataclysm:
  def rule(selector: Expr[Selector], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[CssRule] =
    '{CssRule($selector, ${read(props)})}

  def keyframe(name: Expr[String], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Keyframe] =
    '{Keyframe(Text($name), ${read(props)})}

  def read(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[CssStyle] =
    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[CssProperty]] = exprs match
      case '{type keyType <: Label; ($key: keyType, $value: valueType)} +: tail =>
        val exp: Expr[keyType is PropertyDef[valueType]] = Expr.summon[keyType is PropertyDef[valueType]].getOrElse:
          val typeName = TypeRepr.of[valueType].show
          abandon(m"no valid CSS element ${key.valueOrAbort} taking values of type $typeName exists")

        '{CssProperty(Text($key).uncamel.kebab, compiletime.summonInline[ShowProperty[valueType]].show($value))} :: recur(tail)

      case _ =>
        Nil

    (properties: @unchecked) match
      case Varargs(exprs) => '{CssStyle(${Expr.ofSeq(recur(exprs))}*)}



erased trait PropertyDef[-PropertyType]:
  type Self <: Label

object Selectable:
  given Selector is Selectable as ident = identity(_)

  given [SelectableType: GenericCssSelection] => SelectableType is Selectable =
    SelectableType.selection(_).s match
      case s".$cls" => Selector.Class(cls.tt)
      case s"#$id"  => Selector.Id(id.tt)
      case elem     => Selector.Element(elem.tt)

trait Selectable:
  type Self
  def selector(value: Self): Selector

def select[SelectorType: Selectable](sel: SelectorType)(css: CssStyle) =
  CssRule(SelectorType.selector(sel), css)

extension [SelectorType: Selectable](left: SelectorType)
  @targetName("descendant")
  infix def >> [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) >> SelectorType2.selector(right)

  @targetName("after")
  infix def ~ [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) + SelectorType2.selector(right)

  @targetName("or")
  infix def || [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) | SelectorType2.selector(right)

  @targetName("and")
  infix def && [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) & SelectorType2.selector(right)

  @targetName("before")
  infix def ~~ [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) ~ SelectorType2.selector(right)

object PropertyDef:
  erased given ("alignContent" is PropertyDef[Text]) as alignContent = ###
  erased given ("alignItems" is PropertyDef[Text]) as alignItems = ###
  erased given ("alignSelf" is PropertyDef[Text]) as alignSelf = ###
  erased given ("all" is PropertyDef[Text]) as all = ###
  erased given ("animation" is PropertyDef[Text]) as animation = ###
  erased given ("animationDelay" is PropertyDef[Duration]) as animationDelay = ###
  erased given ("animationDirection" is PropertyDef[Text]) as animationDirection = ###
  erased given ("animationDuration" is PropertyDef[Duration]) as animationDuration = ###
  erased given ("animationFillMode" is PropertyDef[AnimationFillMode]) as animationFillMode = ###
  erased given ("animationIterationCount" is PropertyDef[Text]) as animationIterationCount = ###
  erased given ("animationName" is PropertyDef[Text]) as animationName = ###
  erased given ("animationPlayState" is PropertyDef[Text]) as animationPlayState = ###
  erased given ("animationTimingFunction" is PropertyDef[Text]) as animationTimingFunction = ###
  erased given ("backfaceVisibility" is PropertyDef[Text]) as backfaceVisibility = ###
  erased given ("background" is PropertyDef[Text]) as background = ###
  erased given ("backgroundAttachment" is PropertyDef[Text]) as backgroundAttachment = ###
  erased given ("backgroundBlendMode" is PropertyDef[Text]) as backgroundBlendMode = ###
  erased given ("backgroundClip" is PropertyDef[Text]) as backgroundClip = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("backgroundColor" is PropertyDef[ColorType]) as backgroundColor1 = ###

  erased given ("backgroundColor" is PropertyDef[Transparent.type]) as backgroundColor2 = ###
  erased given ("backgroundImage" is PropertyDef[Text]) as backgroundImage = ###
  //given ("backgroundImage" is PropertyDef[Relative]) as backgroundImage2 = ###

  erased given [PathType](using erased PathType is GenericPath)
      => ("backgroundImage" is PropertyDef[PathType]) as backgroundImage3 = ###

  //erased given ("backgroundImage" is PropertyDef[SimplePath]) as backgroundImage4 = ###
  erased given ("backgroundOrigin" is PropertyDef[Text]) as backgroundOrigin = ###
  erased given ("backgroundPosition" is PropertyDef[Text]) as backgroundPosition = ###
  erased given ("backgroundPosition" is PropertyDef[Dimension]) as backgroundPosition2 = ###
  erased given ("backgroundPosition" is PropertyDef[(Dimension, Dimension)]) as backgroundPosition3 = ###
  erased given ("backgroundRepeat" is PropertyDef[Text]) as backgroundRepeat = ###
  erased given ("backgroundRepeat" is PropertyDef[BackgroundRepeat]) as backgroundRepeat2 = ###

  erased given ("backgroundRepeat" is PropertyDef[(BackgroundRepeat, BackgroundRepeat)]) as backgroundRepeat3 =
      ###

  erased given ("backgroundSize" is PropertyDef[Text]) as backgroundSize = ###
  erased given ("backgroundSize" is PropertyDef[Dimension]) as backgroundSize2 = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("border" is PropertyDef[(BorderStyle, Dimension, ColorType)]) as border = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderBottom" is PropertyDef[(BorderStyle, Dimension, ColorType)]) as borderBottom = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderBottomColor" is PropertyDef[ColorType]) as borderBottomColor1 = ###

  erased given ("borderBottomColor" is PropertyDef[Transparent.type]) as borderBottomColor2 = ###
  erased given ("borderBottomLeftRadius" is PropertyDef[Dimension]) as borderBottomLeftRadius = ###
  erased given ("borderBottomRightRadius" is PropertyDef[Dimension]) as borderBottomRightRadius = ###
  erased given ("borderBottomStyle" is PropertyDef[BorderStyle]) as borderBottomStyle = ###
  erased given ("borderBottomWidth" is PropertyDef[Dimension]) as borderBottomWidth = ###
  erased given ("borderCollapse" is PropertyDef[Text]) as borderCollapse = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderColor" is PropertyDef[ColorType]) as borderColor1 = ###

  erased given ("borderColor" is PropertyDef[Transparent.type]) as borderColor2 = ###
  erased given ("borderImage" is PropertyDef[Text]) as borderImage = ###
  erased given ("borderImageOutset" is PropertyDef[Text]) as borderImageOutset = ###
  erased given ("borderImageRepeat" is PropertyDef[Text]) as borderImageRepeat = ###
  erased given ("borderImageSlice" is PropertyDef[Text]) as borderImageSlice = ###
  erased given ("borderImageSource" is PropertyDef[Text]) as borderImageSource = ###
  erased given ("borderImageWidth" is PropertyDef[Dimension]) as borderImageWidth = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderLeft" is PropertyDef[(BorderStyle, Dimension, ColorType)]) as borderLeft = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderLeftColor" is PropertyDef[ColorType]) as borderLeftColor1 = ###

  erased given ("borderLeftColor" is PropertyDef[Transparent.type]) as borderLeftColor2 = ###
  erased given ("borderLeftStyle" is PropertyDef[BorderStyle]) as borderLeftStyle = ###
  erased given ("borderLeftWidth" is PropertyDef[Dimension]) as borderLeftWidth = ###
  erased given ("borderRadius" is PropertyDef[Dimension]) as borderRadius = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderRight" is PropertyDef[(BorderStyle, Dimension, ColorType)]) as borderRight = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderRightColor" is PropertyDef[ColorType]) as borderRightColor1 = ###

  erased given ("borderRightColor" is PropertyDef[Transparent.type]) as borderRightColor2 = ###
  erased given ("borderRightStyle" is PropertyDef[BorderStyle]) as borderRightStyle = ###
  erased given ("borderRightWidth" is PropertyDef[Dimension]) as borderRightWidth = ###
  erased given ("borderSpacing" is PropertyDef[Dimension]) as borderSpacing = ###
  erased given ("borderStyle" is PropertyDef[BorderStyle]) as borderStyle = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderTop" is PropertyDef[(BorderStyle, Dimension, ColorType)]) as borderTop = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("borderTopColor" is PropertyDef[ColorType]) as borderTopColor1 = ###

  erased given ("borderTopColor" is PropertyDef[Transparent.type]) as borderTopColor2 = ###
  erased given ("borderTopLeftRadius" is PropertyDef[Dimension]) as borderTopLeftRadius = ###
  erased given ("borderTopRightRadius" is PropertyDef[Dimension]) as borderTopRightRadius = ###
  erased given ("borderTopStyle" is PropertyDef[BorderStyle]) as borderTopStyle = ###
  erased given ("borderTopWidth" is PropertyDef[Dimension]) as borderTopWidth = ###
  erased given ("borderWidth" is PropertyDef[Dimension]) as borderWidth = ###
  erased given ("bottom" is PropertyDef[Dimension]) as bottom = ###
  erased given ("boxDecorationBreak" is PropertyDef[Text]) as boxDecorationBreak = ###

  erased given [ColorType](using erased ColorType is Chromatic)
       => ("boxShadow" is PropertyDef[(Dimension, Dimension, Dimension, ColorType)]) as boxShadow = ###

  erased given ("boxSizing" is PropertyDef[Text]) as boxSizing = ###
  erased given ("breakAfter" is PropertyDef[Text]) as breakAfter = ###
  erased given ("breakBefore" is PropertyDef[Text]) as breakBefore = ###
  erased given ("breakInside" is PropertyDef[Text]) as breakInside = ###
  erased given ("captionSide" is PropertyDef[Text]) as captionSide = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("caretColor" is PropertyDef[ColorType]) as caretColor1 = ###

  erased given ("caretColor" is PropertyDef[Transparent.type]) as caretColor2 = ###
  erased given ("clear" is PropertyDef[Text]) as clear = ###
  erased given ("clip" is PropertyDef[Text]) as clip = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("color" is PropertyDef[ColorType]) as color1 = ###

  erased given ("color" is PropertyDef[Transparent.type]) as color2 = ###
  erased given ("columnCount" is PropertyDef[Text]) as columnCount = ###
  erased given ("columnFill" is PropertyDef[Text]) as columnFill = ###
  erased given ("columnGap" is PropertyDef[Text]) as columnGap = ###
  erased given ("columnRule" is PropertyDef[Text]) as columnRule = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("columnRuleColor" is PropertyDef[ColorType]) as columnRuleColor1 = ###

  erased given ("columnRuleColor" is PropertyDef[Transparent.type]) as columnRuleColor2 = ###
  erased given ("columnRuleStyle" is PropertyDef[Text]) as columnRuleStyle = ###
  erased given ("columnRuleWidth" is PropertyDef[Text]) as columnRuleWidth = ###
  erased given ("columnSpan" is PropertyDef[Text]) as columnSpan = ###
  erased given ("columnWidth" is PropertyDef[Text]) as columnWidth = ###
  erased given ("columns" is PropertyDef[Text]) as columns = ###
  erased given ("content" is PropertyDef[Text]) as content = ###
  erased given ("counterIncrement" is PropertyDef[Text]) as counterIncrement = ###
  erased given ("counterReset" is PropertyDef[Text]) as counterReset = ###
  erased given ("cursor" is PropertyDef[Cursor]) as cursor = ###
  erased given ("direction" is PropertyDef[Text]) as direction = ###
  erased given ("display" is PropertyDef[Display]) as display = ###
  erased given ("emptyCells" is PropertyDef[Text]) as emptyCells = ###
  erased given ("filter" is PropertyDef[Text]) as filter = ###
  erased given ("flex" is PropertyDef[Text]) as flex = ###
  erased given ("flexBasis" is PropertyDef[Text]) as flexBasis = ###
  erased given ("flexDirection" is PropertyDef[Text]) as flexDirection = ###
  erased given ("flexFlow" is PropertyDef[Text]) as flexFlow = ###
  erased given ("flexGrow" is PropertyDef[Text]) as flexGrow = ###
  erased given ("flexShrink" is PropertyDef[Text]) as flexShrink = ###
  erased given ("flexWrap" is PropertyDef[Text]) as flexWrap = ###
  erased given ("float" is PropertyDef[Float]) as float = ###
  erased given ("font" is PropertyDef[Text]) as font = ###
  erased given ("fontFamily" is PropertyDef[Font]) as fontFamily = ###
  erased given ("fontFeatureSettings" is PropertyDef[Text]) as fontFeatureSettings = ###
  erased given ("fontKerning" is PropertyDef[Text]) as fontKerning = ###
  erased given ("fontLanguageOverride" is PropertyDef[Text]) as fontLanguageOverride = ###
  erased given ("fontSize" is PropertyDef[Dimension]) as fontSize = ###
  erased given ("fontSizeAdjust" is PropertyDef[Text]) as fontSizeAdjust = ###
  erased given ("fontStretch" is PropertyDef[Text]) as fontStretch = ###
  erased given ("fontStyle" is PropertyDef[FontStyle]) as fontStyle = ###
  erased given ("fontSynthesis" is PropertyDef[Text]) as fontSynthesis = ###
  erased given ("fontVariant" is PropertyDef[Text]) as fontVariant = ###
  erased given ("fontVariantAlternates" is PropertyDef[Text]) as fontVariantAlternates = ###
  erased given ("fontVariantCaps" is PropertyDef[Text]) as fontVariantCaps = ###
  erased given ("fontVariantEastAsian" is PropertyDef[Text]) as fontVariantEastAsian = ###
  erased given ("fontVariantLigatures" is PropertyDef[Text]) as fontVariantLigatures = ###
  erased given ("fontVariantNumeric" is PropertyDef[Text]) as fontVariantNumeric = ###
  erased given ("fontVariantPosition" is PropertyDef[Text]) as fontVariantPosition = ###
  erased given ("fontWeight" is PropertyDef[Int]) as fontWeight1 = ###
  erased given ("fontWeight" is PropertyDef[FontWeight]) as fontWeight2 = ###
  erased given ("gap" is PropertyDef[Text]) as gap = ###
  erased given ("grid" is PropertyDef[Text]) as grid = ###
  erased given ("gridArea" is PropertyDef[Text]) as gridArea = ###
  erased given ("gridAutoColumns" is PropertyDef[Text]) as gridAutoColumns = ###
  erased given ("gridAutoFlow" is PropertyDef[Text]) as gridAutoFlow = ###
  erased given ("gridAutoRows" is PropertyDef[Text]) as gridAutoRows = ###
  erased given ("gridColumn" is PropertyDef[Text]) as gridColumn = ###
  erased given ("gridColumnEnd" is PropertyDef[Text]) as gridColumnEnd = ###
  erased given ("gridColumnGap" is PropertyDef[Text]) as gridColumnGap = ###
  erased given ("gridColumnStart" is PropertyDef[Text]) as gridColumnStart = ###
  erased given ("gridGap" is PropertyDef[Text]) as gridGap = ###
  erased given ("gridRow" is PropertyDef[Text]) as gridRow = ###
  erased given ("gridRowEnd" is PropertyDef[Text]) as gridRowEnd = ###
  erased given ("gridRowGap" is PropertyDef[Text]) as gridRowGap = ###
  erased given ("gridRowStart" is PropertyDef[Text]) as gridRowStart = ###
  erased given ("gridTemplate" is PropertyDef[Text]) as gridTemplate = ###
  erased given ("gridTemplateAreas" is PropertyDef[Text]) as gridTemplateAreas = ###
  erased given ("gridTemplateColumns" is PropertyDef[Text]) as gridTemplateColumns = ###
  erased given ("gridTemplateRows" is PropertyDef[Text]) as gridTemplateRows = ###
  erased given ("hangingPunctuation" is PropertyDef[Text]) as hangingPunctuation = ###
  erased given ("height" is PropertyDef[Dimension]) as height = ###
  erased given ("hyphens" is PropertyDef[Text]) as hyphens = ###
  erased given ("imageRendering" is PropertyDef[Text]) as imageRendering = ###
  erased given ("isolation" is PropertyDef[Text]) as isolation = ###
  erased given ("justifyContent" is PropertyDef[Text]) as justifyContent = ###
  erased given ("left" is PropertyDef[Dimension]) as left = ###
  erased given ("letterSpacing" is PropertyDef[Text]) as letterSpacing = ###
  erased given ("lineBreak" is PropertyDef[Text]) as lineBreak = ###
  erased given ("lineHeight" is PropertyDef[Dimension]) as lineHeight = ###
  erased given ("listStyle" is PropertyDef[Text]) as listStyle = ###
  erased given ("listStyleImage" is PropertyDef[Text]) as listStyleImage = ###
  erased given ("listStylePosition" is PropertyDef[Text]) as listStylePosition = ###
  erased given ("listStyleType" is PropertyDef[Text]) as listStyleType = ###
  erased given ("margin" is PropertyDef[Dimension]) as margin1 = ###
  erased given ("margin" is PropertyDef[(Dimension, Dimension)]) as margin2 = ###
  erased given ("margin" is PropertyDef[(Dimension, Dimension, Dimension)]) as margin3 = ###
  erased given ("margin" is PropertyDef[(Dimension, Dimension, Dimension, Dimension)]) as margin4 = ###
  erased given ("marginBottom" is PropertyDef[Dimension]) as marginBottom = ###
  erased given ("marginLeft" is PropertyDef[Dimension]) as marginLeft = ###
  erased given ("marginRight" is PropertyDef[Dimension]) as marginRight = ###
  erased given ("marginTop" is PropertyDef[Dimension]) as marginTop = ###
  erased given ("mask" is PropertyDef[Text]) as mask = ###
  erased given ("maskType" is PropertyDef[Text]) as maskType = ###
  erased given ("maxHeight" is PropertyDef[Dimension]) as maxHeight = ###
  erased given ("maxWidth" is PropertyDef[Dimension]) as maxWidth = ###
  erased given ("minHeight" is PropertyDef[Dimension]) as minHeight = ###
  erased given ("minWidth" is PropertyDef[Dimension]) as minWidth = ###
  erased given ("mixBlendMode" is PropertyDef[MixBlendMode]) as mixBlendMode = ###
  erased given ("objectFit" is PropertyDef[Text]) as objectFit = ###
  erased given ("objectPosition" is PropertyDef[Text]) as objectPosition = ###
  erased given ("opacity" is PropertyDef[Text]) as opacity = ###
  erased given ("order" is PropertyDef[Text]) as order = ###
  erased given ("orphans" is PropertyDef[Text]) as orphans = ###
  erased given ("outline" is PropertyDef[Text]) as outline = ###

  erased given [ColorType](using erased ColorType is Chromatic)
      => ("outlineColor" is PropertyDef[ColorType]) as outlineColor1 = ###

  erased given ("outlineColor" is PropertyDef[Transparent.type]) as outlineColor2 = ###
  erased given ("outlineOffset" is PropertyDef[Text]) as outlineOffset = ###
  erased given ("outlineStyle" is PropertyDef[Text]) as outlineStyle = ###
  erased given ("outlineWidth" is PropertyDef[Text]) as outlineWidth = ###
  erased given ("over" is PropertyDef[Text]) as over = ###
  erased given ("overflowWrap" is PropertyDef[Text]) as overflowWrap = ###
  erased given ("overflow" is PropertyDef[(Overflow, Overflow)]) as overflow = ###
  erased given ("overflowX" is PropertyDef[Overflow]) as overflowX = ###
  erased given ("overflowY" is PropertyDef[Overflow]) as overflowY = ###
  erased given ("padding" is PropertyDef[Dimension]) as padding1 = ###
  erased given ("padding" is PropertyDef[(Dimension, Dimension)]) as padding2 = ###
  erased given ("padding" is PropertyDef[(Dimension, Dimension, Dimension)]) as padding3 = ###
  erased given ("padding" is PropertyDef[(Dimension, Dimension, Dimension, Dimension)]) as padding4 = ###
  erased given ("paddingBottom" is PropertyDef[Dimension]) as paddingBottom = ###
  erased given ("paddingLeft" is PropertyDef[Dimension]) as paddingLeft = ###
  erased given ("paddingRight" is PropertyDef[Dimension]) as paddingRight = ###
  erased given ("paddingTop" is PropertyDef[Dimension]) as paddingTop = ###
  erased given ("pageBreakAfter" is PropertyDef[Text]) as pageBreakAfter = ###
  erased given ("pageBreakBefore" is PropertyDef[Text]) as pageBreakBefore = ###
  erased given ("pageBreakInside" is PropertyDef[Text]) as pageBreakInside = ###
  erased given ("perspective" is PropertyDef[Text]) as perspective = ###
  erased given ("perspectiveOrigin" is PropertyDef[Text]) as perspectiveOrigin = ###
  erased given ("pointerEvents" is PropertyDef[PointerEvents]) as pointerEvents = ###
  erased given ("position" is PropertyDef[Position]) as position = ###
  erased given ("quotes" is PropertyDef[Text]) as quotes = ###
  erased given ("resize" is PropertyDef[Text]) as resize = ###
  erased given ("right" is PropertyDef[Dimension]) as right = ###
  erased given ("rowGap" is PropertyDef[Text]) as rowGap = ###
  erased given ("scrollBehavior" is PropertyDef[Text]) as scrollBehavior = ###
  erased given ("scrollbarWidth" is PropertyDef[Text]) as scrollbarWidth = ###
  erased given ("tabSize" is PropertyDef[Text]) as tabSize = ###
  erased given ("tableLayout" is PropertyDef[Text]) as tableLayout = ###
  erased given ("textAlign" is PropertyDef[TextAlign]) as textAlign = ###
  erased given ("textAlignLast" is PropertyDef[TextAlign]) as textAlignLast = ###
  erased given ("textCombineUpright" is PropertyDef[Text]) as textCombineUpright = ###
  erased given ("textDecoration" is PropertyDef[TextDecorationLine]) as textDecoration1 = ###

  erased given ("textDecoration" is PropertyDef[(TextDecorationLine, Text, TextDecorationStyle)]) as textDecoration2 =
    ###

  erased given [ColorType](using erased ColorType is Chromatic) => ("textDecorationColor" is PropertyDef[ColorType]) as textDecorationColor1 = ###
  erased given ("textDecorationColor" is PropertyDef[Transparent.type]) as textDecorationColor2 = ###
  erased given ("textDecorationLine" is PropertyDef[TextDecorationLine]) as textDecorationLine = ###
  erased given ("textDecorationStyle" is PropertyDef[TextDecorationStyle]) as textDecorationStyle = ###
  erased given ("textIndent" is PropertyDef[Dimension]) as textIndent = ###
  erased given ("textJustify" is PropertyDef[Text]) as textJustify = ###
  erased given ("textOrientation" is PropertyDef[Text]) as textOrientation = ###
  erased given ("textOverflow" is PropertyDef[Text]) as textOverflow = ###
  erased given ("textShadow" is PropertyDef[Text]) as textShadow = ###
  erased given ("textTransform" is PropertyDef[Text]) as textTransform = ###
  erased given ("textUnderlinePosition" is PropertyDef[Text]) as textUnderlinePosition = ###
  erased given ("top" is PropertyDef[Dimension]) as top = ###
  erased given ("transform" is PropertyDef[Text]) as transform = ###
  erased given ("transformOrigin" is PropertyDef[Text]) as transformOrigin = ###
  erased given ("transformStyle" is PropertyDef[Text]) as transformStyle = ###
  erased given ("transition" is PropertyDef[Text]) as transition = ###
  erased given ("transitionDelay" is PropertyDef[Text]) as transitionDelay = ###
  erased given ("transitionDuration" is PropertyDef[Text]) as transitionDuration = ###
  erased given ("transitionProperty" is PropertyDef[Text]) as transitionProperty = ###
  erased given ("transitionTimingFunction" is PropertyDef[Text]) as transitionTimingFunction = ###
  erased given ("unicodeBidi" is PropertyDef[Text]) as unicodeBidi = ###
  erased given ("userSelect" is PropertyDef[UserSelect]) as userSelect = ###
  erased given ("verticalAlign" is PropertyDef[VerticalAlign]) as verticalAlign1 = ###
  erased given ("verticalAlign" is PropertyDef[Dimension]) as verticalAlign2 = ###
  erased given ("visibility" is PropertyDef[Text]) as visibility = ###
  erased given ("whiteSpace" is PropertyDef[Text]) as whiteSpace = ###
  erased given ("widows" is PropertyDef[Text]) as widows = ###
  erased given ("width" is PropertyDef[Dimension]) as width = ###
  erased given ("wordBreak" is PropertyDef[Text]) as wordBreak = ###
  erased given ("wordSpacing" is PropertyDef[Text]) as wordSpacing = ###
  erased given ("wordWrap" is PropertyDef[Text]) as wordWrap = ###
  erased given ("writingMode" is PropertyDef[Text]) as writingMode = ###
  erased given ("zIndex" is PropertyDef[Int]) as zIndex = ###

  erased given [LabelType <: Label] => LabelType is PropertyDef[Inherit.type] as inherit = ###
  erased given [LabelType <: Label] => LabelType is PropertyDef[Initial.type] as initial = ###
  erased given [LabelType <: Label] => LabelType is PropertyDef[Transparent.type] as transparent = ###

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

  given [PropertyType: ShowProperty, PropertyType2: ShowProperty]
          : ShowProperty[(PropertyType, PropertyType2)] = tuple =>
    t"${PropertyType.show(tuple(0))} ${PropertyType2.show(tuple(1))}"

  given [PropertyType: ShowProperty, PropertyType2: ShowProperty, PropertyType3: ShowProperty]
          : ShowProperty[(PropertyType, PropertyType2, PropertyType3)] =

    tuple => List(PropertyType.show(tuple(0)), PropertyType2.show(tuple(1)), PropertyType3.show(tuple(2))).join(t" ")

  given [PropertyType: ShowProperty, PropertyType2: ShowProperty, PropertyType3: ShowProperty, PropertyType4: ShowProperty]
          : ShowProperty[(PropertyType, PropertyType2, PropertyType3, PropertyType4)] = tuple =>
    List
     (PropertyType.show(tuple(0)),
      PropertyType2.show(tuple(1)),
      PropertyType3.show(tuple(2)),
      PropertyType4.show(tuple(3))).join(t" ")

  given ShowProperty[Font] = _.names.map: f =>
    if f.contains(t" ") then t"'$f'" else f

  . join(t", ")

  //given ShowProperty[SimplePath] = path => t"url('${path}')"

  given [PathType: GenericPath]: ShowProperty[PathType] =
    path => t"url('${path.pathText}')"

  given ShowProperty[Text] = identity(_)
  given ShowProperty[Int] = _.show

  given [ColorType: Chromatic]: ShowProperty[ColorType] = color =>
    t"rgb(${ColorType.red(color)},${ColorType.green(color)},${ColorType.blue(color)})"

  //given ShowProperty[Relative] = rel => t"url('$rel')"
  //given ShowProperty[GenericPath] = rel => t"url('$rel')"
  given ShowProperty[PropertyValue] = _.show
  given ShowProperty[Inherit.type] = c => t"inherit"
  given ShowProperty[Transparent.type] = c => t"transparent"
  given ShowProperty[Initial.type] = c => t"initial"

trait ShowProperty[-PropertyType]:
  def show(value: PropertyType): Text

object PropertyValue:
  given PropertyValue is Showable = _.toString.show.uncamel.kebab

trait PropertyValue

object Duration:
  given Duration is Showable =
    case S(value)  => t"${value}s"
    case Ms(value) => t"${value}ms"

enum Duration:
  case S(value: Double)
  case Ms(value: Double)

def max(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function(t"max", _))
def min(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function(t"min", _))

object Length:
  given Length is Showable =
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
  infix def + (dim: Length): Length = infixOp(t" + ", dim)

  @targetName("sub")
  infix def - (dim: Length): Length = infixOp(t" - ", dim)

  @targetName("mul")
  infix def * (double: Double): Length = infixOp(t" * ", double)

  @targetName("div")
  infix def / (double: Double): Length = infixOp(t" / ", double)

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

  def has[SelectorType: Selectable](selector: SelectorType) =
     Selector.PseudoClass(t"has(${SelectorType.selector(selector).value})")

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
