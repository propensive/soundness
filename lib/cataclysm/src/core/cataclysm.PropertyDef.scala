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
┃    Soundness, version 0.49.0.                                                                    ┃
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
package cataclysm

import anticipation.*
import prepositional.*
import quantitative.*
import rudiments.*

erased trait PropertyDef[-property]:
  type Self <: Label

object PropertyDef:
  erased given alignContent: ("alignContent" is PropertyDef[Text]) = !!
  erased given alignItems: ("alignItems" is PropertyDef[Text]) = !!
  erased given alignSelf: ("alignSelf" is PropertyDef[Text]) = !!
  erased given all: ("all" is PropertyDef[Text]) = !!
  erased given animation: ("animation" is PropertyDef[Text]) = !!

  erased given animationDelay: [duration <: Measure]
               => (erased duration is Normalizable to Seconds[1])
               => ("animationDelay" is PropertyDef[Quantity[duration]]) = !!

  erased given animationDirection: ("animationDirection" is PropertyDef[Text]) = !!

  given animationDuration: [duration <: Measure]
        => (erased duration is Normalizable to Seconds[1])
        => ("animationDuration" is PropertyDef[Quantity[duration]]) = !!

  erased given animationFillMode: ("animationFillMode" is PropertyDef[AnimationFillMode]) = !!
  erased given animationIterationCount: ("animationIterationCount" is PropertyDef[Text]) = !!
  erased given animationName: ("animationName" is PropertyDef[Text]) = !!
  erased given animationPlayState: ("animationPlayState" is PropertyDef[Text]) = !!
  erased given animationTimingFunction: ("animationTimingFunction" is PropertyDef[Text]) = !!
  erased given backfaceVisibility: ("backfaceVisibility" is PropertyDef[Text]) = !!
  erased given background: ("background" is PropertyDef[Text]) = !!
  erased given backgroundAttachment: ("backgroundAttachment" is PropertyDef[Text]) = !!
  erased given backgroundBlendMode: ("backgroundBlendMode" is PropertyDef[Text]) = !!
  erased given backgroundClip: ("backgroundClip" is PropertyDef[Text]) = !!

  erased given backgroundColor1: [color]
        => (erased color is Chromatic)
        => ("backgroundColor" is PropertyDef[color]) = !!

  erased given backgroundColor2: ("backgroundColor" is PropertyDef[Transparent.type]) = !!
  erased given backgroundImage: ("backgroundImage" is PropertyDef[Text]) = !!
  //given backgroundImage2: ("backgroundImage" is PropertyDef[Relative]) = !!

  erased given backgroundImage3: [path]
        => (erased path is Abstractable across Paths to Text)
        => ("backgroundImage" is PropertyDef[path]) = !!

  //erased given backgroundImage4: ("backgroundImage" is PropertyDef[SimplePath]) = !!
  erased given backgroundOrigin: ("backgroundOrigin" is PropertyDef[Text]) = !!
  erased given backgroundPosition: ("backgroundPosition" is PropertyDef[Text]) = !!
  erased given backgroundPosition2: ("backgroundPosition" is PropertyDef[Length | Int]) = !!

  erased given backgroundPosition3: ("backgroundPosition" is PropertyDef[(Length | Int,
                                                                          Length | Int)]) =
    !!

  erased given backgroundRepeat: ("backgroundRepeat" is PropertyDef[Text]) = !!
  erased given backgroundRepeat2: ("backgroundRepeat" is PropertyDef[BackgroundRepeat]) = !!

  erased given backgroundRepeat3: ("backgroundRepeat" is PropertyDef
                                                          [(BackgroundRepeat, BackgroundRepeat)]) =
      !!

  erased given backgroundSize: ("backgroundSize" is PropertyDef[Text]) = !!
  erased given backgroundSize2: ("backgroundSize" is PropertyDef[Length | Int]) = !!

  erased given border: [color]
               => (erased color is Chromatic)
               => ("border" is PropertyDef[(BorderStyle, Length | Int, color)]) = !!

  erased given borderBottom: [color] => (erased color is Chromatic)
               => ("borderBottom" is PropertyDef[(BorderStyle, Length | Int, color)]) = !!

  erased given borderBottomColor: [color] => (erased color is Chromatic)
               => ("borderBottomColor" is PropertyDef[color]) = !!

  erased given borderBottomColor2: ("borderBottomColor" is PropertyDef[Transparent.type]) = !!
  erased given borderBottomLeftRadius: ("borderBottomLeftRadius" is PropertyDef[Length | Int]) = !!

  erased given borderBottomRightRadius: ("borderBottomRightRadius" is PropertyDef[Length | Int]) =
    !!

  erased given borderBottomStyle: ("borderBottomStyle" is PropertyDef[BorderStyle]) = !!
  erased given borderBottomWidth: ("borderBottomWidth" is PropertyDef[Length | Int]) = !!
  erased given borderCollapse: ("borderCollapse" is PropertyDef[Text]) = !!

  erased given borderColor1: [color] => (erased color is Chromatic)
               => ("borderColor" is PropertyDef[color]) = !!

  erased given borderColor2: ("borderColor" is PropertyDef[Transparent.type]) = !!
  erased given borderImage: ("borderImage" is PropertyDef[Text]) = !!
  erased given borderImageOutset: ("borderImageOutset" is PropertyDef[Text]) = !!
  erased given borderImageRepeat: ("borderImageRepeat" is PropertyDef[Text]) = !!
  erased given borderImageSlice: ("borderImageSlice" is PropertyDef[Text]) = !!
  erased given borderImageSource: ("borderImageSource" is PropertyDef[Text]) = !!
  erased given borderImageWidth: ("borderImageWidth" is PropertyDef[Length | Int]) = !!

  erased given borderLeft: [color] => (erased color is Chromatic)
               => ("borderLeft" is PropertyDef[(BorderStyle, Length | Int, color)]) = !!

  erased given borderLeftColor1: [color] => (erased color is Chromatic)
               => ("borderLeftColor" is PropertyDef[color]) = !!

  erased given borderLeftColor2: ("borderLeftColor" is PropertyDef[Transparent.type]) = !!
  erased given borderLeftStyle: ("borderLeftStyle" is PropertyDef[BorderStyle]) = !!
  erased given borderLeftWidth: ("borderLeftWidth" is PropertyDef[Length | Int]) = !!
  erased given borderRadius: ("borderRadius" is PropertyDef[Length | Int]) = !!

  erased given borderRight: [color] => (erased color is Chromatic)
               => ("borderRight" is PropertyDef[(BorderStyle, Length | Int, color)]) = !!

  erased given borderRightColor1: [color] => (erased color is Chromatic)
               => ("borderRightColor" is PropertyDef[color]) = !!

  erased given borderRightColor2: ("borderRightColor" is PropertyDef[Transparent.type]) = !!
  erased given borderRightStyle: ("borderRightStyle" is PropertyDef[BorderStyle]) = !!
  erased given borderRightWidth: ("borderRightWidth" is PropertyDef[Length | Int]) = !!
  erased given borderSpacing: ("borderSpacing" is PropertyDef[Length | Int]) = !!
  erased given borderStyle: ("borderStyle" is PropertyDef[BorderStyle]) = !!

  erased given borderTop: [color] => (erased color is Chromatic)
               => ("borderTop" is PropertyDef[(BorderStyle, Length | Int, color)]) = !!

  erased given borderTopColor1: [color] => (erased color is Chromatic)
               => ("borderTopColor" is PropertyDef[color]) = !!

  erased given borderTopColor2: ("borderTopColor" is PropertyDef[Transparent.type]) = !!
  erased given borderTopLeftRadius: ("borderTopLeftRadius" is PropertyDef[Length | Int]) = !!
  erased given borderTopRightRadius: ("borderTopRightRadius" is PropertyDef[Length | Int]) = !!
  erased given borderTopStyle: ("borderTopStyle" is PropertyDef[BorderStyle]) = !!
  erased given borderTopWidth: ("borderTopWidth" is PropertyDef[Length | Int]) = !!
  erased given borderWidth: ("borderWidth" is PropertyDef[Length | Int]) = !!
  erased given bottom: ("bottom" is PropertyDef[Length | Int]) = !!
  erased given boxDecorationBreak: ("boxDecorationBreak" is PropertyDef[Text]) = !!

  erased given boxShadow: [color] => (erased color is Chromatic)
               => ("boxShadow" is PropertyDef[(Length | Int, Length | Int, Length | Int, color)]) =
    !!

  erased given boxSizing: ("boxSizing" is PropertyDef[Text]) = !!
  erased given breakAfter: ("breakAfter" is PropertyDef[Text]) = !!
  erased given breakBefore: ("breakBefore" is PropertyDef[Text]) = !!
  erased given breakInside: ("breakInside" is PropertyDef[Text]) = !!
  erased given captionSide: ("captionSide" is PropertyDef[Text]) = !!

  erased given caretColor1: [color] => (erased color is Chromatic)
               => ("caretColor" is PropertyDef[color]) = !!

  erased given caretColor2: ("caretColor" is PropertyDef[Transparent.type]) = !!
  erased given clear: ("clear" is PropertyDef[Text]) = !!
  erased given clip: ("clip" is PropertyDef[Text]) = !!

  erased given color1: [color] => (erased color is Chromatic)
               => ("color" is PropertyDef[color]) = !!

  erased given color2: ("color" is PropertyDef[Transparent.type]) = !!
  erased given columnCount: ("columnCount" is PropertyDef[Text]) = !!
  erased given columnFill: ("columnFill" is PropertyDef[Text]) = !!
  erased given columnGap: ("columnGap" is PropertyDef[Text]) = !!
  erased given columnRule: ("columnRule" is PropertyDef[Text]) = !!

  erased given columnRuleColor1: [color] => (erased color is Chromatic)
               => ("columnRuleColor" is PropertyDef[color]) = !!

  erased given columnRuleColor2: ("columnRuleColor" is PropertyDef[Transparent.type]) = !!
  erased given columnRuleStyle: ("columnRuleStyle" is PropertyDef[Text]) = !!
  erased given columnRuleWidth: ("columnRuleWidth" is PropertyDef[Text]) = !!
  erased given columnSpan: ("columnSpan" is PropertyDef[Text]) = !!
  erased given columnWidth: ("columnWidth" is PropertyDef[Text]) = !!
  erased given columns: ("columns" is PropertyDef[Text]) = !!
  erased given content: ("content" is PropertyDef[Text]) = !!
  erased given counterIncrement: ("counterIncrement" is PropertyDef[Text]) = !!
  erased given counterReset: ("counterReset" is PropertyDef[Text]) = !!
  erased given cursor: ("cursor" is PropertyDef[Cursor]) = !!
  erased given direction: ("direction" is PropertyDef[Text]) = !!
  erased given display: ("display" is PropertyDef[Display]) = !!
  erased given emptyCells: ("emptyCells" is PropertyDef[Text]) = !!
  erased given filter: ("filter" is PropertyDef[Text]) = !!
  erased given flex: ("flex" is PropertyDef[Text]) = !!
  erased given flexBasis: ("flexBasis" is PropertyDef[Text]) = !!
  erased given flexDirection: ("flexDirection" is PropertyDef[Text]) = !!
  erased given flexFlow: ("flexFlow" is PropertyDef[Text]) = !!
  erased given flexGrow: ("flexGrow" is PropertyDef[Text]) = !!
  erased given flexShrink: ("flexShrink" is PropertyDef[Text]) = !!
  erased given flexWrap: ("flexWrap" is PropertyDef[Text]) = !!
  erased given float: ("float" is PropertyDef[Float]) = !!
  erased given font: ("font" is PropertyDef[Text]) = !!
  erased given fontFamily: ("fontFamily" is PropertyDef[Font]) = !!
  erased given fontFeatureSettings: ("fontFeatureSettings" is PropertyDef[Text]) = !!
  erased given fontKerning: ("fontKerning" is PropertyDef[Text]) = !!
  erased given fontLanguageOverride: ("fontLanguageOverride" is PropertyDef[Text]) = !!
  erased given fontSize: ("fontSize" is PropertyDef[Length | Int]) = !!
  erased given fontSizeAdjust: ("fontSizeAdjust" is PropertyDef[Text]) = !!
  erased given fontStretch: ("fontStretch" is PropertyDef[Text]) = !!
  erased given fontStyle: ("fontStyle" is PropertyDef[FontStyle]) = !!
  erased given fontSynthesis: ("fontSynthesis" is PropertyDef[Text]) = !!
  erased given fontVariant: ("fontVariant" is PropertyDef[Text]) = !!
  erased given fontVariantAlternates: ("fontVariantAlternates" is PropertyDef[Text]) = !!
  erased given fontVariantCaps: ("fontVariantCaps" is PropertyDef[Text]) = !!
  erased given fontVariantEastAsian: ("fontVariantEastAsian" is PropertyDef[Text]) = !!
  erased given fontVariantLigatures: ("fontVariantLigatures" is PropertyDef[Text]) = !!
  erased given fontVariantNumeric: ("fontVariantNumeric" is PropertyDef[Text]) = !!
  erased given fontVariantPosition: ("fontVariantPosition" is PropertyDef[Text]) = !!
  erased given fontWeight1: ("fontWeight" is PropertyDef[Int]) = !!
  erased given fontWeight2: ("fontWeight" is PropertyDef[FontWeight]) = !!
  erased given gap: ("gap" is PropertyDef[Text]) = !!
  erased given grid: ("grid" is PropertyDef[Text]) = !!
  erased given gridArea: ("gridArea" is PropertyDef[Text]) = !!
  erased given gridAutoColumns: ("gridAutoColumns" is PropertyDef[Text]) = !!
  erased given gridAutoFlow: ("gridAutoFlow" is PropertyDef[Text]) = !!
  erased given gridAutoRows: ("gridAutoRows" is PropertyDef[Text]) = !!
  erased given gridColumn: ("gridColumn" is PropertyDef[Text]) = !!
  erased given gridColumnEnd: ("gridColumnEnd" is PropertyDef[Text]) = !!
  erased given gridColumnGap: ("gridColumnGap" is PropertyDef[Text]) = !!
  erased given gridColumnStart: ("gridColumnStart" is PropertyDef[Text]) = !!
  erased given gridGap: ("gridGap" is PropertyDef[Text]) = !!
  erased given gridRow: ("gridRow" is PropertyDef[Text]) = !!
  erased given gridRowEnd: ("gridRowEnd" is PropertyDef[Text]) = !!
  erased given gridRowGap: ("gridRowGap" is PropertyDef[Text]) = !!
  erased given gridRowStart: ("gridRowStart" is PropertyDef[Text]) = !!
  erased given gridTemplate: ("gridTemplate" is PropertyDef[Text]) = !!
  erased given gridTemplateAreas: ("gridTemplateAreas" is PropertyDef[Text]) = !!
  erased given gridTemplateColumns: ("gridTemplateColumns" is PropertyDef[Text]) = !!
  erased given gridTemplateRows: ("gridTemplateRows" is PropertyDef[Text]) = !!
  erased given hangingPunctuation: ("hangingPunctuation" is PropertyDef[Text]) = !!
  erased given height: ("height" is PropertyDef[Length | Int]) = !!
  erased given hyphens: ("hyphens" is PropertyDef[Text]) = !!
  erased given imageRendering: ("imageRendering" is PropertyDef[Text]) = !!
  erased given isolation: ("isolation" is PropertyDef[Text]) = !!
  erased given justifyContent: ("justifyContent" is PropertyDef[Text]) = !!
  erased given left: ("left" is PropertyDef[Length | Int]) = !!
  erased given letterSpacing: ("letterSpacing" is PropertyDef[Text]) = !!
  erased given lineBreak: ("lineBreak" is PropertyDef[Text]) = !!
  erased given lineHeight: ("lineHeight" is PropertyDef[Length | Int]) = !!
  erased given listStyle: ("listStyle" is PropertyDef[Text]) = !!
  erased given listStyleImage: ("listStyleImage" is PropertyDef[Text]) = !!
  erased given listStylePosition: ("listStylePosition" is PropertyDef[Text]) = !!
  erased given listStyleType: ("listStyleType" is PropertyDef[Text]) = !!
  erased given margin1: ("margin" is PropertyDef[Length | Int]) = !!
  erased given margin2: ("margin" is PropertyDef[(Length | Int, Length | Int)]) = !!
  erased given margin3: ("margin" is PropertyDef[(Length | Int, Length | Int, Length | Int)]) = !!

  erased given margin4: ("margin" is PropertyDef[(Length | Int,
                                                  Length | Int,
                                                  Length | Int,
                                                  Length | Int)]) =
    !!

  erased given marginBottom: ("marginBottom" is PropertyDef[Length | Int]) = !!
  erased given marginLeft: ("marginLeft" is PropertyDef[Length | Int]) = !!
  erased given marginRight: ("marginRight" is PropertyDef[Length | Int]) = !!
  erased given marginTop: ("marginTop" is PropertyDef[Length | Int]) = !!
  erased given mask: ("mask" is PropertyDef[Text]) = !!
  erased given maskType: ("maskType" is PropertyDef[Text]) = !!
  erased given maxHeight: ("maxHeight" is PropertyDef[Length | Int]) = !!
  erased given maxWidth: ("maxWidth" is PropertyDef[Length | Int]) = !!
  erased given minHeight: ("minHeight" is PropertyDef[Length | Int]) = !!
  erased given minWidth: ("minWidth" is PropertyDef[Length | Int]) = !!
  erased given mixBlendMode: ("mixBlendMode" is PropertyDef[MixBlendMode]) = !!
  erased given objectFit: ("objectFit" is PropertyDef[Text]) = !!
  erased given objectPosition: ("objectPosition" is PropertyDef[Text]) = !!
  erased given opacity: ("opacity" is PropertyDef[Text]) = !!
  erased given order: ("order" is PropertyDef[Text]) = !!
  erased given orphans: ("orphans" is PropertyDef[Text]) = !!
  erased given outline: ("outline" is PropertyDef[Text]) = !!

  erased given outlineColor1: [color] => (erased color is Chromatic)
               => ("outlineColor" is PropertyDef[color]) = !!

  erased given outlineColor2: ("outlineColor" is PropertyDef[Transparent.type]) = !!
  erased given outlineOffset: ("outlineOffset" is PropertyDef[Text]) = !!
  erased given outlineStyle: ("outlineStyle" is PropertyDef[Text]) = !!
  erased given outlineWidth: ("outlineWidth" is PropertyDef[Text]) = !!
  erased given over: ("over" is PropertyDef[Text]) = !!
  erased given overflowWrap: ("overflowWrap" is PropertyDef[Text]) = !!
  erased given overflow: ("overflow" is PropertyDef[(Overflow, Overflow)]) = !!
  erased given overflowX: ("overflowX" is PropertyDef[Overflow]) = !!
  erased given overflowY: ("overflowY" is PropertyDef[Overflow]) = !!
  erased given padding1: ("padding" is PropertyDef[Length | Int]) = !!
  erased given padding2: ("padding" is PropertyDef[(Length | Int, Length | Int)]) = !!
  erased given padding3: ("padding" is PropertyDef[(Length | Int, Length | Int, Length | Int)]) = !!

  erased given padding4: ("padding" is PropertyDef[(Length | Int,
                                                    Length | Int,
                                                    Length | Int,
                                                    Length | Int)]) =
    !!

  erased given paddingBottom: ("paddingBottom" is PropertyDef[Length | Int]) = !!
  erased given paddingLeft: ("paddingLeft" is PropertyDef[Length | Int]) = !!
  erased given paddingRight: ("paddingRight" is PropertyDef[Length | Int]) = !!
  erased given paddingTop: ("paddingTop" is PropertyDef[Length | Int]) = !!
  erased given pageBreakAfter: ("pageBreakAfter" is PropertyDef[Text]) = !!
  erased given pageBreakBefore: ("pageBreakBefore" is PropertyDef[Text]) = !!
  erased given pageBreakInside: ("pageBreakInside" is PropertyDef[Text]) = !!
  erased given perspective: ("perspective" is PropertyDef[Text]) = !!
  erased given perspectiveOrigin: ("perspectiveOrigin" is PropertyDef[Text]) = !!
  erased given pointerEvents: ("pointerEvents" is PropertyDef[PointerEvents]) = !!
  erased given position: ("position" is PropertyDef[Position]) = !!
  erased given quotes: ("quotes" is PropertyDef[Text]) = !!
  erased given resize: ("resize" is PropertyDef[Text]) = !!
  erased given right: ("right" is PropertyDef[Length | Int]) = !!
  erased given rowGap: ("rowGap" is PropertyDef[Text]) = !!
  erased given scrollBehavior: ("scrollBehavior" is PropertyDef[Text]) = !!
  erased given scrollbarWidth: ("scrollbarWidth" is PropertyDef[Text]) = !!
  erased given tabSize: ("tabSize" is PropertyDef[Text]) = !!
  erased given tableLayout: ("tableLayout" is PropertyDef[Text]) = !!
  erased given textAlign: ("textAlign" is PropertyDef[TextAlign]) = !!
  erased given textAlignLast: ("textAlignLast" is PropertyDef[TextAlign]) = !!
  erased given textCombineUpright: ("textCombineUpright" is PropertyDef[Text]) = !!
  erased given textDecoration1: ("textDecoration" is PropertyDef[TextDecorationLine]) = !!

  erased given textDecoration2: ("textDecoration" is PropertyDef
                                                      [(TextDecorationLine,
                                                        Text,
                                                        TextDecorationStyle)]) =
    !!

  erased given textDecorationColor1: [color] => (erased color is Chromatic)
               => ("textDecorationColor" is PropertyDef[color]) = !!

  erased given textDecorationColor2: ("textDecorationColor" is PropertyDef[Transparent.type]) = !!
  erased given textDecorationLine: ("textDecorationLine" is PropertyDef[TextDecorationLine]) = !!

  erased given textDecorationStyle: ("textDecorationStyle" is PropertyDef[TextDecorationStyle]) =
    !!

  erased given textIndent: ("textIndent" is PropertyDef[Length | Int]) = !!
  erased given textJustify: ("textJustify" is PropertyDef[Text]) = !!
  erased given textOrientation: ("textOrientation" is PropertyDef[Text]) = !!
  erased given textOverflow: ("textOverflow" is PropertyDef[Text]) = !!
  erased given textShadow: ("textShadow" is PropertyDef[Text]) = !!
  erased given textTransform: ("textTransform" is PropertyDef[Text]) = !!
  erased given textUnderlinePosition: ("textUnderlinePosition" is PropertyDef[Text]) = !!
  erased given top: ("top" is PropertyDef[Length | Int]) = !!
  erased given transform: ("transform" is PropertyDef[Text]) = !!
  erased given transformOrigin: ("transformOrigin" is PropertyDef[Text]) = !!
  erased given transformStyle: ("transformStyle" is PropertyDef[Text]) = !!
  erased given transition: ("transition" is PropertyDef[Text]) = !!
  erased given transitionDelay: ("transitionDelay" is PropertyDef[Text]) = !!
  erased given transitionDuration: ("transitionDuration" is PropertyDef[Text]) = !!
  erased given transitionProperty: ("transitionProperty" is PropertyDef[Text]) = !!
  erased given transitionTimingFunction: ("transitionTimingFunction" is PropertyDef[Text]) = !!
  erased given unicodeBidi: ("unicodeBidi" is PropertyDef[Text]) = !!
  erased given userSelect: ("userSelect" is PropertyDef[UserSelect]) = !!
  erased given verticalAlign1: ("verticalAlign" is PropertyDef[VerticalAlign]) = !!
  erased given verticalAlign2: ("verticalAlign" is PropertyDef[Length | Int]) = !!
  erased given visibility: ("visibility" is PropertyDef[Text]) = !!
  erased given whiteSpace: ("whiteSpace" is PropertyDef[Text]) = !!
  erased given widows: ("widows" is PropertyDef[Text]) = !!
  erased given width: ("width" is PropertyDef[Length | Int]) = !!
  erased given wordBreak: ("wordBreak" is PropertyDef[Text]) = !!
  erased given wordSpacing: ("wordSpacing" is PropertyDef[Text]) = !!
  erased given wordWrap: ("wordWrap" is PropertyDef[Text]) = !!
  erased given writingMode: ("writingMode" is PropertyDef[Text]) = !!
  erased given zIndex: ("zIndex" is PropertyDef[Int]) = !!

  erased given inherit: [label <: Label] => label is PropertyDef[Inherit.type] = !!
  erased given initial: [label <: Label] => label is PropertyDef[Initial.type] = !!

  erased given transparent: [label <: Label] => label is PropertyDef[Transparent.type] =
    !!
