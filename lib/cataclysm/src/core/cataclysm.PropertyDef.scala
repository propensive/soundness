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
package cataclysm

import anticipation.*
import prepositional.*
import quantitative.*
import rudiments.*

sealed trait PropertyDef[-property]:
  type Self <: Label

object PropertyDef:
  inline given alignContent: ("alignContent" is PropertyDef[Text]) = !!
  inline given alignItems: ("alignItems" is PropertyDef[Text]) = !!
  inline given alignSelf: ("alignSelf" is PropertyDef[Text]) = !!
  inline given all: ("all" is PropertyDef[Text]) = !!
  inline given animation: ("animation" is PropertyDef[Text]) = !!


  inline given animationDelay: [duration <: Measure]
  =>  ( erased duration is Normalizable to Seconds[1] )
  =>  ( "animationDelay" is PropertyDef[Quantity[duration]] ) = !!


  inline given animationDirection: ("animationDirection" is PropertyDef[Text]) = !!


  inline given animationDuration: [duration <: Measure]
  =>  ( erased duration is Normalizable to Seconds[1] )
  =>  ( "animationDuration" is PropertyDef[Quantity[duration]] ) = !!


  inline given animationFillMode: ("animationFillMode" is PropertyDef[AnimationFillMode]) = !!
  inline given animationIterationCount: ("animationIterationCount" is PropertyDef[Text]) = !!
  inline given animationName: ("animationName" is PropertyDef[Text]) = !!
  inline given animationPlayState: ("animationPlayState" is PropertyDef[Text]) = !!
  inline given animationTimingFunction: ("animationTimingFunction" is PropertyDef[Text]) = !!
  inline given backfaceVisibility: ("backfaceVisibility" is PropertyDef[Text]) = !!
  inline given background: ("background" is PropertyDef[Text]) = !!
  inline given backgroundAttachment: ("backgroundAttachment" is PropertyDef[Text]) = !!
  inline given backgroundBlendMode: ("backgroundBlendMode" is PropertyDef[Text]) = !!
  inline given backgroundClip: ("backgroundClip" is PropertyDef[Text]) = !!


  inline given backgroundColor1: [color]
  =>  ( erased color is Chromatic )
  =>  ( "backgroundColor" is PropertyDef[color] ) = !!


  inline given backgroundColor2: ("backgroundColor" is PropertyDef[Transparent.type]) = !!
  inline given backgroundImage: ("backgroundImage" is PropertyDef[Text]) = !!
  //given backgroundImage2: ("backgroundImage" is PropertyDef[Relative]) = !!


  inline given backgroundImage3: [path]
  =>  ( erased path is Abstractable across Paths to Text )
  =>  ( "backgroundImage" is PropertyDef[path] ) = !!


  //inline given backgroundImage4: ("backgroundImage" is PropertyDef[SimplePath]) = !!
  inline given backgroundOrigin: ("backgroundOrigin" is PropertyDef[Text]) = !!
  inline given backgroundPosition: ("backgroundPosition" is PropertyDef[Text]) = !!
  inline given backgroundPosition2: ("backgroundPosition" is PropertyDef[Length | Int]) = !!


  inline given backgroundPosition3: ("backgroundPosition" is PropertyDef[(Length | Int,
                                                                          Length | Int)]) =

    !!


  inline given backgroundRepeat: ("backgroundRepeat" is PropertyDef[Text]) = !!
  inline given backgroundRepeat2: ("backgroundRepeat" is PropertyDef[BackgroundRepeat]) = !!


  inline given backgroundRepeat3: ("backgroundRepeat" is PropertyDef
                                                          [(BackgroundRepeat, BackgroundRepeat)]) =

      !!


  inline given backgroundSize: ("backgroundSize" is PropertyDef[Text]) = !!
  inline given backgroundSize2: ("backgroundSize" is PropertyDef[Length | Int]) = !!


  inline given border: [color] => (erased color is Chromatic)
  =>  ( "border" is PropertyDef[(BorderStyle, Length | Int, color)] ) = !!


  inline given borderBottom: [color] => (erased color is Chromatic)
  =>  ( "borderBottom" is PropertyDef[(BorderStyle, Length | Int, color)] ) = !!


  inline given borderBottomColor: [color] => (erased color is Chromatic)
  =>  ( "borderBottomColor" is PropertyDef[color] ) = !!


  inline given borderBottomColor2: ("borderBottomColor" is PropertyDef[Transparent.type]) = !!
  inline given borderBottomLeftRadius: ("borderBottomLeftRadius" is PropertyDef[Length | Int]) = !!

  inline given borderBottomRightRadius: ("borderBottomRightRadius" is PropertyDef[Length | Int]) =
    !!

  inline given borderBottomStyle: ("borderBottomStyle" is PropertyDef[BorderStyle]) = !!
  inline given borderBottomWidth: ("borderBottomWidth" is PropertyDef[Length | Int]) = !!
  inline given borderCollapse: ("borderCollapse" is PropertyDef[Text]) = !!


  inline given borderColor1: [color] => (erased color is Chromatic)
  =>  ( "borderColor" is PropertyDef[color] ) = !!


  inline given borderColor2: ("borderColor" is PropertyDef[Transparent.type]) = !!
  inline given borderImage: ("borderImage" is PropertyDef[Text]) = !!
  inline given borderImageOutset: ("borderImageOutset" is PropertyDef[Text]) = !!
  inline given borderImageRepeat: ("borderImageRepeat" is PropertyDef[Text]) = !!
  inline given borderImageSlice: ("borderImageSlice" is PropertyDef[Text]) = !!
  inline given borderImageSource: ("borderImageSource" is PropertyDef[Text]) = !!
  inline given borderImageWidth: ("borderImageWidth" is PropertyDef[Length | Int]) = !!


  inline given borderLeft: [color] => (erased color is Chromatic)
  =>  ( "borderLeft" is PropertyDef[(BorderStyle, Length | Int, color)] ) = !!


  inline given borderLeftColor1: [color] => (erased color is Chromatic)
  =>  ( "borderLeftColor" is PropertyDef[color] ) = !!


  inline given borderLeftColor2: ("borderLeftColor" is PropertyDef[Transparent.type]) = !!
  inline given borderLeftStyle: ("borderLeftStyle" is PropertyDef[BorderStyle]) = !!
  inline given borderLeftWidth: ("borderLeftWidth" is PropertyDef[Length | Int]) = !!
  inline given borderRadius: ("borderRadius" is PropertyDef[Length | Int]) = !!


  inline given borderRight: [color] => (erased color is Chromatic)
  =>  ( "borderRight" is PropertyDef[(BorderStyle, Length | Int, color)] ) = !!


  inline given borderRightColor1: [color] => (erased color is Chromatic)
  =>  ( "borderRightColor" is PropertyDef[color] ) = !!


  inline given borderRightColor2: ("borderRightColor" is PropertyDef[Transparent.type]) = !!
  inline given borderRightStyle: ("borderRightStyle" is PropertyDef[BorderStyle]) = !!
  inline given borderRightWidth: ("borderRightWidth" is PropertyDef[Length | Int]) = !!
  inline given borderSpacing: ("borderSpacing" is PropertyDef[Length | Int]) = !!
  inline given borderStyle: ("borderStyle" is PropertyDef[BorderStyle]) = !!


  inline given borderTop: [color] => (erased color is Chromatic)
  =>  ( "borderTop" is PropertyDef[(BorderStyle, Length | Int, color)] ) = !!


  inline given borderTopColor1: [color] => (erased color is Chromatic)
  =>  ( "borderTopColor" is PropertyDef[color] ) = !!


  inline given borderTopColor2: ("borderTopColor" is PropertyDef[Transparent.type]) = !!
  inline given borderTopLeftRadius: ("borderTopLeftRadius" is PropertyDef[Length | Int]) = !!
  inline given borderTopRightRadius: ("borderTopRightRadius" is PropertyDef[Length | Int]) = !!
  inline given borderTopStyle: ("borderTopStyle" is PropertyDef[BorderStyle]) = !!
  inline given borderTopWidth: ("borderTopWidth" is PropertyDef[Length | Int]) = !!
  inline given borderWidth: ("borderWidth" is PropertyDef[Length | Int]) = !!
  inline given bottom: ("bottom" is PropertyDef[Length | Int]) = !!
  inline given boxDecorationBreak: ("boxDecorationBreak" is PropertyDef[Text]) = !!


  inline given boxShadow: [color] => (erased color is Chromatic)
  =>  ( "boxShadow" is PropertyDef[(Length | Int, Length | Int, Length | Int, color)] ) =

    !!


  inline given boxSizing: ("boxSizing" is PropertyDef[Text]) = !!
  inline given breakAfter: ("breakAfter" is PropertyDef[Text]) = !!
  inline given breakBefore: ("breakBefore" is PropertyDef[Text]) = !!
  inline given breakInside: ("breakInside" is PropertyDef[Text]) = !!
  inline given captionSide: ("captionSide" is PropertyDef[Text]) = !!


  inline given caretColor1: [color] => (erased color is Chromatic)
  =>  ( "caretColor" is PropertyDef[color] ) = !!


  inline given caretColor2: ("caretColor" is PropertyDef[Transparent.type]) = !!
  inline given clear: ("clear" is PropertyDef[Text]) = !!
  inline given clip: ("clip" is PropertyDef[Text]) = !!


  inline given color1: [color] => (erased color is Chromatic)
  =>  ( "color" is PropertyDef[color] ) = !!


  inline given color2: ("color" is PropertyDef[Transparent.type]) = !!
  inline given columnCount: ("columnCount" is PropertyDef[Text]) = !!
  inline given columnFill: ("columnFill" is PropertyDef[Text]) = !!
  inline given columnGap: ("columnGap" is PropertyDef[Text]) = !!
  inline given columnRule: ("columnRule" is PropertyDef[Text]) = !!


  inline given columnRuleColor1: [color] => (erased color is Chromatic)
  =>  ( "columnRuleColor" is PropertyDef[color] ) = !!


  inline given columnRuleColor2: ("columnRuleColor" is PropertyDef[Transparent.type]) = !!
  inline given columnRuleStyle: ("columnRuleStyle" is PropertyDef[Text]) = !!
  inline given columnRuleWidth: ("columnRuleWidth" is PropertyDef[Text]) = !!
  inline given columnSpan: ("columnSpan" is PropertyDef[Text]) = !!
  inline given columnWidth: ("columnWidth" is PropertyDef[Text]) = !!
  inline given columns: ("columns" is PropertyDef[Text]) = !!
  inline given content: ("content" is PropertyDef[Text]) = !!
  inline given counterIncrement: ("counterIncrement" is PropertyDef[Text]) = !!
  inline given counterReset: ("counterReset" is PropertyDef[Text]) = !!
  inline given cursor: ("cursor" is PropertyDef[Cursor]) = !!
  inline given direction: ("direction" is PropertyDef[Text]) = !!
  inline given display: ("display" is PropertyDef[Display]) = !!
  inline given emptyCells: ("emptyCells" is PropertyDef[Text]) = !!
  inline given filter: ("filter" is PropertyDef[Text]) = !!
  inline given flex: ("flex" is PropertyDef[Text]) = !!
  inline given flexBasis: ("flexBasis" is PropertyDef[Text]) = !!
  inline given flexDirection: ("flexDirection" is PropertyDef[Text]) = !!
  inline given flexFlow: ("flexFlow" is PropertyDef[Text]) = !!
  inline given flexGrow: ("flexGrow" is PropertyDef[Text]) = !!
  inline given flexShrink: ("flexShrink" is PropertyDef[Text]) = !!
  inline given flexWrap: ("flexWrap" is PropertyDef[Text]) = !!
  inline given float: ("float" is PropertyDef[Float]) = !!
  inline given font: ("font" is PropertyDef[Text]) = !!
  inline given fontFamily: ("fontFamily" is PropertyDef[Font]) = !!
  inline given fontFeatureSettings: ("fontFeatureSettings" is PropertyDef[Text]) = !!
  inline given fontKerning: ("fontKerning" is PropertyDef[Text]) = !!
  inline given fontLanguageOverride: ("fontLanguageOverride" is PropertyDef[Text]) = !!
  inline given fontSize: ("fontSize" is PropertyDef[Length | Int]) = !!
  inline given fontSizeAdjust: ("fontSizeAdjust" is PropertyDef[Text]) = !!
  inline given fontStretch: ("fontStretch" is PropertyDef[Text]) = !!
  inline given fontStyle: ("fontStyle" is PropertyDef[FontStyle]) = !!
  inline given fontSynthesis: ("fontSynthesis" is PropertyDef[Text]) = !!
  inline given fontVariant: ("fontVariant" is PropertyDef[Text]) = !!
  inline given fontVariantAlternates: ("fontVariantAlternates" is PropertyDef[Text]) = !!
  inline given fontVariantCaps: ("fontVariantCaps" is PropertyDef[Text]) = !!
  inline given fontVariantEastAsian: ("fontVariantEastAsian" is PropertyDef[Text]) = !!
  inline given fontVariantLigatures: ("fontVariantLigatures" is PropertyDef[Text]) = !!
  inline given fontVariantNumeric: ("fontVariantNumeric" is PropertyDef[Text]) = !!
  inline given fontVariantPosition: ("fontVariantPosition" is PropertyDef[Text]) = !!
  inline given fontWeight1: ("fontWeight" is PropertyDef[Int]) = !!
  inline given fontWeight2: ("fontWeight" is PropertyDef[FontWeight]) = !!
  inline given gap: ("gap" is PropertyDef[Text]) = !!
  inline given grid: ("grid" is PropertyDef[Text]) = !!
  inline given gridArea: ("gridArea" is PropertyDef[Text]) = !!
  inline given gridAutoColumns: ("gridAutoColumns" is PropertyDef[Text]) = !!
  inline given gridAutoFlow: ("gridAutoFlow" is PropertyDef[Text]) = !!
  inline given gridAutoRows: ("gridAutoRows" is PropertyDef[Text]) = !!
  inline given gridColumn: ("gridColumn" is PropertyDef[Text]) = !!
  inline given gridColumnEnd: ("gridColumnEnd" is PropertyDef[Text]) = !!
  inline given gridColumnGap: ("gridColumnGap" is PropertyDef[Text]) = !!
  inline given gridColumnStart: ("gridColumnStart" is PropertyDef[Text]) = !!
  inline given gridGap: ("gridGap" is PropertyDef[Text]) = !!
  inline given gridRow: ("gridRow" is PropertyDef[Text]) = !!
  inline given gridRowEnd: ("gridRowEnd" is PropertyDef[Text]) = !!
  inline given gridRowGap: ("gridRowGap" is PropertyDef[Text]) = !!
  inline given gridRowStart: ("gridRowStart" is PropertyDef[Text]) = !!
  inline given gridTemplate: ("gridTemplate" is PropertyDef[Text]) = !!
  inline given gridTemplateAreas: ("gridTemplateAreas" is PropertyDef[Text]) = !!
  inline given gridTemplateColumns: ("gridTemplateColumns" is PropertyDef[Text]) = !!
  inline given gridTemplateRows: ("gridTemplateRows" is PropertyDef[Text]) = !!
  inline given hangingPunctuation: ("hangingPunctuation" is PropertyDef[Text]) = !!
  inline given height: ("height" is PropertyDef[Length | Int]) = !!
  inline given hyphens: ("hyphens" is PropertyDef[Text]) = !!
  inline given imageRendering: ("imageRendering" is PropertyDef[Text]) = !!
  inline given isolation: ("isolation" is PropertyDef[Text]) = !!
  inline given justifyContent: ("justifyContent" is PropertyDef[Text]) = !!
  inline given left: ("left" is PropertyDef[Length | Int]) = !!
  inline given letterSpacing: ("letterSpacing" is PropertyDef[Text]) = !!
  inline given lineBreak: ("lineBreak" is PropertyDef[Text]) = !!
  inline given lineHeight: ("lineHeight" is PropertyDef[Length | Int]) = !!
  inline given listStyle: ("listStyle" is PropertyDef[Text]) = !!
  inline given listStyleImage: ("listStyleImage" is PropertyDef[Text]) = !!
  inline given listStylePosition: ("listStylePosition" is PropertyDef[Text]) = !!
  inline given listStyleType: ("listStyleType" is PropertyDef[Text]) = !!
  inline given margin1: ("margin" is PropertyDef[Length | Int]) = !!
  inline given margin2: ("margin" is PropertyDef[(Length | Int, Length | Int)]) = !!
  inline given margin3: ("margin" is PropertyDef[(Length | Int, Length | Int, Length | Int)]) = !!


  inline given margin4: ("margin" is PropertyDef[(Length | Int,
                                                  Length | Int,
                                                  Length | Int,
                                                  Length | Int)]) =

    !!


  inline given marginBottom: ("marginBottom" is PropertyDef[Length | Int]) = !!
  inline given marginLeft: ("marginLeft" is PropertyDef[Length | Int]) = !!
  inline given marginRight: ("marginRight" is PropertyDef[Length | Int]) = !!
  inline given marginTop: ("marginTop" is PropertyDef[Length | Int]) = !!
  inline given mask: ("mask" is PropertyDef[Text]) = !!
  inline given maskType: ("maskType" is PropertyDef[Text]) = !!
  inline given maxHeight: ("maxHeight" is PropertyDef[Length | Int]) = !!
  inline given maxWidth: ("maxWidth" is PropertyDef[Length | Int]) = !!
  inline given minHeight: ("minHeight" is PropertyDef[Length | Int]) = !!
  inline given minWidth: ("minWidth" is PropertyDef[Length | Int]) = !!
  inline given mixBlendMode: ("mixBlendMode" is PropertyDef[MixBlendMode]) = !!
  inline given objectFit: ("objectFit" is PropertyDef[Text]) = !!
  inline given objectPosition: ("objectPosition" is PropertyDef[Text]) = !!
  inline given opacity: ("opacity" is PropertyDef[Text]) = !!
  inline given order: ("order" is PropertyDef[Text]) = !!
  inline given orphans: ("orphans" is PropertyDef[Text]) = !!
  inline given outline: ("outline" is PropertyDef[Text]) = !!


  inline given outlineColor1: [color] => (erased color is Chromatic)
  =>  ( "outlineColor" is PropertyDef[color] ) = !!


  inline given outlineColor2: ("outlineColor" is PropertyDef[Transparent.type]) = !!
  inline given outlineOffset: ("outlineOffset" is PropertyDef[Text]) = !!
  inline given outlineStyle: ("outlineStyle" is PropertyDef[Text]) = !!
  inline given outlineWidth: ("outlineWidth" is PropertyDef[Text]) = !!
  inline given over: ("over" is PropertyDef[Text]) = !!
  inline given overflowWrap: ("overflowWrap" is PropertyDef[Text]) = !!
  inline given overflow: ("overflow" is PropertyDef[(Overflow, Overflow)]) = !!
  inline given overflowX: ("overflowX" is PropertyDef[Overflow]) = !!
  inline given overflowY: ("overflowY" is PropertyDef[Overflow]) = !!
  inline given padding1: ("padding" is PropertyDef[Length | Int]) = !!
  inline given padding2: ("padding" is PropertyDef[(Length | Int, Length | Int)]) = !!
  inline given padding3: ("padding" is PropertyDef[(Length | Int, Length | Int, Length | Int)]) = !!


  inline given padding4: ("padding" is PropertyDef[(Length | Int,
                                                    Length | Int,
                                                    Length | Int,
                                                    Length | Int)]) =

    !!


  inline given paddingBottom: ("paddingBottom" is PropertyDef[Length | Int]) = !!
  inline given paddingLeft: ("paddingLeft" is PropertyDef[Length | Int]) = !!
  inline given paddingRight: ("paddingRight" is PropertyDef[Length | Int]) = !!
  inline given paddingTop: ("paddingTop" is PropertyDef[Length | Int]) = !!
  inline given pageBreakAfter: ("pageBreakAfter" is PropertyDef[Text]) = !!
  inline given pageBreakBefore: ("pageBreakBefore" is PropertyDef[Text]) = !!
  inline given pageBreakInside: ("pageBreakInside" is PropertyDef[Text]) = !!
  inline given perspective: ("perspective" is PropertyDef[Text]) = !!
  inline given perspectiveOrigin: ("perspectiveOrigin" is PropertyDef[Text]) = !!
  inline given pointerEvents: ("pointerEvents" is PropertyDef[PointerEvents]) = !!
  inline given position: ("position" is PropertyDef[Position]) = !!
  inline given quotes: ("quotes" is PropertyDef[Text]) = !!
  inline given resize: ("resize" is PropertyDef[Text]) = !!
  inline given right: ("right" is PropertyDef[Length | Int]) = !!
  inline given rowGap: ("rowGap" is PropertyDef[Text]) = !!
  inline given scrollBehavior: ("scrollBehavior" is PropertyDef[Text]) = !!
  inline given scrollbarWidth: ("scrollbarWidth" is PropertyDef[Text]) = !!
  inline given tabSize: ("tabSize" is PropertyDef[Text]) = !!
  inline given tableLayout: ("tableLayout" is PropertyDef[Text]) = !!
  inline given textAlign: ("textAlign" is PropertyDef[TextAlign]) = !!
  inline given textAlignLast: ("textAlignLast" is PropertyDef[TextAlign]) = !!
  inline given textCombineUpright: ("textCombineUpright" is PropertyDef[Text]) = !!
  inline given textDecoration1: ("textDecoration" is PropertyDef[TextDecorationLine]) = !!


  inline given textDecoration2: ("textDecoration" is PropertyDef
                                                      [(TextDecorationLine,
                                                        Text,
                                                        TextDecorationStyle)]) =

    !!


  inline given textDecorationColor1: [color] => (erased color is Chromatic)
  =>  ( "textDecorationColor" is PropertyDef[color] ) = !!


  inline given textDecorationColor2: ("textDecorationColor" is PropertyDef[Transparent.type]) = !!
  inline given textDecorationLine: ("textDecorationLine" is PropertyDef[TextDecorationLine]) = !!

  inline given textDecorationStyle: ("textDecorationStyle" is PropertyDef[TextDecorationStyle]) =
    !!

  inline given textIndent: ("textIndent" is PropertyDef[Length | Int]) = !!
  inline given textJustify: ("textJustify" is PropertyDef[Text]) = !!
  inline given textOrientation: ("textOrientation" is PropertyDef[Text]) = !!
  inline given textOverflow: ("textOverflow" is PropertyDef[Text]) = !!
  inline given textShadow: ("textShadow" is PropertyDef[Text]) = !!
  inline given textTransform: ("textTransform" is PropertyDef[Text]) = !!
  inline given textUnderlinePosition: ("textUnderlinePosition" is PropertyDef[Text]) = !!
  inline given top: ("top" is PropertyDef[Length | Int]) = !!
  inline given transform: ("transform" is PropertyDef[Text]) = !!
  inline given transformOrigin: ("transformOrigin" is PropertyDef[Text]) = !!
  inline given transformStyle: ("transformStyle" is PropertyDef[Text]) = !!
  inline given transition: ("transition" is PropertyDef[Text]) = !!
  inline given transitionDelay: ("transitionDelay" is PropertyDef[Text]) = !!
  inline given transitionDuration: ("transitionDuration" is PropertyDef[Text]) = !!
  inline given transitionProperty: ("transitionProperty" is PropertyDef[Text]) = !!
  inline given transitionTimingFunction: ("transitionTimingFunction" is PropertyDef[Text]) = !!
  inline given unicodeBidi: ("unicodeBidi" is PropertyDef[Text]) = !!
  inline given userSelect: ("userSelect" is PropertyDef[UserSelect]) = !!
  inline given verticalAlign1: ("verticalAlign" is PropertyDef[VerticalAlign]) = !!
  inline given verticalAlign2: ("verticalAlign" is PropertyDef[Length | Int]) = !!
  inline given visibility: ("visibility" is PropertyDef[Text]) = !!
  inline given whiteSpace: ("whiteSpace" is PropertyDef[Text]) = !!
  inline given widows: ("widows" is PropertyDef[Text]) = !!
  inline given width: ("width" is PropertyDef[Length | Int]) = !!
  inline given wordBreak: ("wordBreak" is PropertyDef[Text]) = !!
  inline given wordSpacing: ("wordSpacing" is PropertyDef[Text]) = !!
  inline given wordWrap: ("wordWrap" is PropertyDef[Text]) = !!
  inline given writingMode: ("writingMode" is PropertyDef[Text]) = !!
  inline given zIndex: ("zIndex" is PropertyDef[Int]) = !!
  inline given inherit: [label <: Label] => label is PropertyDef[Inherit.type] = !!
  inline given initial: [label <: Label] => label is PropertyDef[Initial.type] = !!

  inline given transparent: [label <: Label] => label is PropertyDef[Transparent.type] =
    !!
