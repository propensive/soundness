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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package honeycomb

import anticipation.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*
import urticose.*
import vacuous.*

object Attributive:
  erased trait AccessKeys
  erased trait Affirmation
  erased trait Autocapitalization
  erased trait ButtonType
  erased trait Closedby
  erased trait Color
  erased trait Colorspace
  erased trait Command
  erased trait ContentEditable
  erased trait Coords
  erased trait Crossorigin
  erased trait CustomElementName
  erased trait Css
  erased trait CssClassList
  erased trait Datetime
  erased trait Decimal
  erased trait Decoding
  erased trait Dir
  erased trait Enctype
  erased trait EnterKeyHint
  erased trait FetchPriority
  erased trait HashName
  erased trait Hidden
  erased trait HttpEquiv
  erased trait Id
  erased trait Ids
  erased trait ImageSizes
  erased trait ImageSrcSet
  erased trait InputMode
  erased trait InputType
  erased trait InputValue
  erased trait Integral
  erased trait ItemProp
  erased trait Kind
  erased trait Language
  erased trait Laziness
  erased trait MediaQueryList
  erased trait Method
  erased trait Minmax
  erased trait MimeList
  erased trait Mime
  erased trait Name
  erased trait OlType
  erased trait Openness
  erased trait PermissionsPolicy
  erased trait Popover
  erased trait PopoverAction
  erased trait PositiveInt
  erased trait Presence
  erased trait Preload
  erased trait ReferrerPolicy
  erased trait Regex
  erased trait Sandbox
  erased trait ScriptType
  erased trait Shape
  erased trait Sizes
  erased trait Softness
  erased trait SourceSizeList
  erased trait Srcdoc
  erased trait SrcSet
  erased trait Switch
  erased trait Target
  erased trait Temporal
  erased trait Textual
  erased trait ThScope
  erased trait Tokens
  erased trait Truth
  erased trait Upto8
  erased trait Url
  erased trait Urls
  erased trait Utf8

  given textTextual: Text is Attributive to Textual = (key, value) => (key, value)
  given stringTextual: String is Attributive to Textual = (key, value) => (key, value.tt)

  given boolean: Boolean is Attributive to Presence = (key, value) =>
    if value then (key, Unset) else Unset

  given switch: Boolean is Attributive to Switch = (key, value) =>
    (key, if value then t"on" else t"off")

  given truth: Boolean is Attributive to Truth = (key, value) =>
    (key, if value then t"true" else t"false")

  given int: Int is Attributive to Integral = _ -> _.show
  given posInt: Int is Attributive to PositiveInt = _ -> _.show
  given double: Double is Attributive to Decimal = _ -> _.toString.tt
  given domId: DomId is Attributive to Id = _ -> _.toString.tt
  given stylesheet: Stylesheet is Attributive to CssClassList = _ -> _.classes.join(t" ")

  given url: [url: Abstractable across Urls to Text] => url is Attributive to Url =
    (key, value) => (key, value.generic)

  given url: HttpUrl is Attributive to Url = (key, value) => (key, value.show)

  given style: Text is Attributive to Css = (key, value) => (key, value)

trait Attributive extends Typeclass, Resultant:
  def attribute(key: Text, value: Self): Optional[(Text, Optional[Text])]
