/*
    Cataclysm, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import rudiments.*

erased trait PropertyDef[-PropertyType]:
  type Self <: Label

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

  erased given [LabelType <: Label] => LabelType is PropertyDef[Transparent.type] as transparent =
    ###
