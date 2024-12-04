/*
    Honeycomb, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

import rudiments.*
import vacuous.*
import gossamer.*
import anticipation.*
import hieroglyph.*
import prepositional.*
import spectacular.*

trait HtmlAttribute[-ValueType, -T]:
  type Self <: Label
  def convert(value: ValueType): Optional[Text]
  def rename: Option[Text] = None

object HtmlAttribute:
  given [LabelType <: Label: GenericHtmlAttribute[ValueType], ValueType, T]
      => LabelType is HtmlAttribute[ValueType, T]:

    def convert(value: ValueType): Optional[Text] = LabelType.serialize(value).show
    override def rename: Option[Text] = Some(LabelType.name.show)

  given [T, LabelType <: Label] => LabelType is HtmlAttribute[Text, T] as any = identity(_)

  given [T] => ("accept" is HtmlAttribute[List[Text], T]) as accept = _.join(t",")

  given [T] => ("acceptCharset" is HtmlAttribute[Encoding, T]) as acceptCharset:
    override def rename: Option[Text] = Some(t"accept-charset")
    def convert(value: Encoding): Text = value.name

  given [T] => ("accesskey" is HtmlAttribute[Char, T]) as accesskey = _.show
  given [T] => ("allowfullscreen" is HtmlAttribute[Boolean, T]) as allowfullscreen = _ => Unset

  given [T] => ("allowpaymentrequest" is HtmlAttribute[Boolean, T]) as allowpaymentrequest =
    _ => Unset

  given [T] => ("alt" is HtmlAttribute[Text, T]) as alt = identity(_)
  given [T] => ("async" is HtmlAttribute[Boolean, T]) as async = _ => Unset
  given [T] => ("autocomplete" is HtmlAttribute[Autocomplete, T]) as autocomplete = _.show
  given [T] => ("autocomplete" is HtmlAttribute[Boolean, T]) as autocomplete2 = t"off".unless(_)
  given [T] => ("autoplay" is HtmlAttribute[Boolean, T]) as autoplay = _ => Unset
  given [T] => ("autofocus" is HtmlAttribute[Boolean, T]) as autofocus = _ => Unset
  given [T] => ("border" is HtmlAttribute[Boolean, T]) as border = if _ then t"1" else t""
  given [T] => ("charset" is HtmlAttribute[Encoding, T]) as charset = _.name
  given [T] => ("checked" is HtmlAttribute[Boolean, T]) as checkedBoolean = _ => Unset
  given [T] => ("cite" is HtmlAttribute[Text, T]) as cite = identity(_)

  given [T] => ("class" is HtmlAttribute[List[CssClass], T]) as `class`:
    override def rename: Option[Text] = Some(t"class")
    def convert(value: List[CssClass]): Text = value.map(_.name).join(t" ")

  given [T] => HtmlAttribute[CssClass, T] as class2:
    override def rename: Option[Text] = Some(t"class")
    def convert(value: CssClass): Text = value.name

  given [T] => ("code" is HtmlAttribute[Text, T]) as code = identity(_) // MediaError
  given [T] => ("codebase" is HtmlAttribute[Text, T]) as codebase = identity(_)
  given [T] => ("cols" is HtmlAttribute[Int, T]) as cols = _.show
  given [T] => ("colspan" is HtmlAttribute[Int, T]) as colspan = _.show
  given [T] => ("content" is HtmlAttribute[Text, T]) as content = identity(_)
  given [T] => ("contenteditable" is HtmlAttribute[Boolean, T]) as contenteditable = if _ then t"true" else t"false"
  given [T] => ("controls" is HtmlAttribute[Boolean, T]) as controls = _ => Unset
  given [T] => ("coords" is HtmlAttribute[Seq[Double], T]) as coords = _.map(_.toString.show).join(t",")
  given [T] => ("crossorigin" is HtmlAttribute[Crossorigin, T]) as crossorigin = _.show
  given [T] => ("data" is HtmlAttribute[Text, T]) as data = identity(_)
  given [T] => ("datetime" is HtmlAttribute[Text, T]) as datetime = identity(_) // To be provided by Aviation
  given [T] => ("default" is HtmlAttribute[Boolean, T]) as default = _ => Unset
  given [T] => ("defer" is HtmlAttribute[Boolean, T]) as defer = _ => Unset
  given [T] => ("dir" is HtmlAttribute[HDir, T]) as dir = _.show

  // Should be the name of an input in a form, followed by `.dir`
  given [T] => ("dirname" is HtmlAttribute[Text, T]) as dirname = identity(_)

  given [T] => ("disabled" is HtmlAttribute[Boolean, T]) as disabled = _ => Unset

  // should be a filename, but probably best as `Text`
  given [T] => ("download" is HtmlAttribute[Text, T]) as download = identity(_)

  given [T] => ("draggable" is HtmlAttribute[Boolean, T]) as draggable = _.toString.tt

  given [T] => ("enctype" is HtmlAttribute[Text, T]) as enctype = identity(_) // provided by Gesticulate

  given [T] => ("hfor" is HtmlAttribute[DomId, T]) as hfor:
    override def rename: Option[Text] = Some(t"for")
    def convert(value: DomId): Text = value.name

  given [T] => ("hfor" is HtmlAttribute[Seq[DomId], T]) as hfors:
    override def rename: Option[Text] = Some(t"for")
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given [T] => ("for" is HtmlAttribute[DomId, T]) as `for`:
    def convert(value: DomId): Text = value.name

  given [T] => ("for" is HtmlAttribute[Seq[DomId], T]) as fors:
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given [T] => ("form" is HtmlAttribute[DomId, T]) as form = _.name
  given [T] => ("formaction" is HtmlAttribute[Text, T]) as formaction = identity(_) // Provided by Scintillate
  given [T] => ("formenctype" is HtmlAttribute[Text, T]) as formenctype = identity(_)
  given [T] => ("formmethod" is HtmlAttribute[Method, T]) as formmethod = _.show
  given [T] => ("formnovalidate" is HtmlAttribute[Boolean, T]) as formnovalidate = _ => Unset
  given [T] => ("formtarget" is HtmlAttribute[Target, T]) as formtarget = _.show
  given [T] => ("headers" is HtmlAttribute[DomId, T]) as headers = _.name
  given [T] => ("headers" is HtmlAttribute[Set[DomId], T]) as headers2 = _.map(_.name).join(t" ")
  given [T] => ("height" is HtmlAttribute[Int, T]) as height = _.show
  given [T] => ("hidden" is HtmlAttribute[Boolean, T]) as hidden = _ => Unset
  given [T] => ("high" is HtmlAttribute[Double, T]) as high = _.toString.show
  given ("href" is HtmlAttribute[Text, Text]) as href = identity(_)
  given [UrlType: GenericUrl, T] => ("href" is HtmlAttribute[UrlType, T]) as href2 = UrlType.text(_)
  given [T] => ("hreflang" is HtmlAttribute[Text, T]) as hreflang = identity(_) // Needs to be provided by Cosmopolite

  given [T] => ("httpEquiv" is HtmlAttribute[HttpEquiv, T]) as httpEquiv:
    override def rename: Option[Text] = Some(t"http-equiv")
    def convert(value: HttpEquiv): Text = value.show

  given [T] => ("id" is HtmlAttribute[DomId, T]) as id = _.name
  given [T] => ("ismap" is HtmlAttribute[Boolean, T]) as ismap = _ => Unset
  given [T] => ("kind" is HtmlAttribute[Kind, T]) as kind = _.show
  given [T] => ("label" is HtmlAttribute[Text, T]) as label = identity(_)
  given [T] => ("lang" is HtmlAttribute[Text, T]) as lang = identity(_) // Should be provided by Cosmopolite
  given [T] => ("list" is HtmlAttribute[Seq[DomId], T]) as list = _.map(_.name).join(t" ")
  given [T] => ("list" is HtmlAttribute[DomId, T]) as list2 = _.name
  given [T] => ("loop" is HtmlAttribute[Boolean, T]) as loop = _ => Unset
  given [T] => ("low" is HtmlAttribute[Double, T]) as low = _.toString.tt
  given [T] => ("manifest" is HtmlAttribute[Text, T]) as manifest = identity(_) // Provided by Scintillate
  given [T] => ("max" is HtmlAttribute[Double | Int, T]) as max = _.toString.show
  given [T] => ("maxlength" is HtmlAttribute[Int, T]) as maxlength = _.show
  given [T] => ("minlength" is HtmlAttribute[Int, T]) as minlength = _.show
  given [T] => ("media" is HtmlAttribute[Text, T]) as media = identity(_) // Should be provided by Cataclysm
  given [T] => ("method" is HtmlAttribute[Method, T]) as method = _.show
  given [T] => ("min" is HtmlAttribute[Double | Int, T]) as min = _.toString.show
  given [T] => ("multiple" is HtmlAttribute[Boolean, T]) as multiple = _ => Unset
  given [T] => ("muted" is HtmlAttribute[Boolean, T]) as muted = _ => Unset
  given [T] => ("name" is HtmlAttribute[Text, T]) as name = identity(_) // Should provide special `name` identifiers
  given ("name" is HtmlAttribute[Target, "iframe" | "object"]) as name2 = _.show
  given [T] => ("nonce" is HtmlAttribute[Text, T]) as nonce = identity(_) // Should be provided by Gastronomy
  given [T] => ("novalidate" is HtmlAttribute[Boolean, T]) as novalidate = _ => Unset
  given [T] => ("open" is HtmlAttribute[Boolean, T]) as open = _ => Unset
  given [T] => ("optimum" is HtmlAttribute[Double, T]) as optimum = _.toString.show
  given [T] => ("pattern" is HtmlAttribute[Text, T]) as pattern = identity(_) // Provide with Kaleidoscope
  given [T] => ("placeholder" is HtmlAttribute[Text, T]) as placeholder = identity(_)
  given [T] => ("playsinline" is HtmlAttribute[Boolean, T]) as playsinline = _ => Unset
  given [T] => ("poster" is HtmlAttribute[Text, T]) as poster = identity(_)
  given [T] => ("preload" is HtmlAttribute[Preload, T]) as preload = _.show
  given [T] => ("readonly" is HtmlAttribute[Boolean, T]) as readonly = _ => Unset
  given [T] => ("referrerpolicy" is HtmlAttribute[Text, T]) as referrerpolicy = identity(_)
  given [T] => ("rel" is HtmlAttribute[Rel, T]) as rel = _.show
  given [T] => ("rel" is HtmlAttribute[Seq[Rel], T]) as rel2 = _.map(_.show).join(t" ")
  given [T] => ("required" is HtmlAttribute[Boolean, T]) as required = _ => Unset
  given [T] => ("rev" is HtmlAttribute[Rev, T]) as rev = _.show
  given [T] => ("rows" is HtmlAttribute[Int, T]) as rows = _.show
  given [T] => ("rowspan" is HtmlAttribute[Int, T]) as rowspan = _.show
  given [T] => ("sandbox" is HtmlAttribute[Sandbox, T]) as sandbox = _.show
  given [T] => ("scope" is HtmlAttribute[Scope, T]) as scope = _.show
  given [T] => ("selected" is HtmlAttribute[Boolean, T]) as selected = _ => Unset
  given [T] => ("shape" is HtmlAttribute[Shape, T]) as shape = _.show
  given [T] => ("size" is HtmlAttribute[Int, T]) as size = _.show
  given [T] => ("sizes" is HtmlAttribute[Text, T]) as sizes = identity(_) // This should perhaps be a Map
  given [T] => ("slot" is HtmlAttribute[Text, T]) as slot = identity(_)
  given [T] => ("span" is HtmlAttribute[Int, T]) as span = _.show
  given [T] => ("spellcheck" is HtmlAttribute[Boolean, T]) as spellcheck = if _ then t"true" else t"false"
  given [T] => ("src" is HtmlAttribute[Text, T]) as src = identity(_)
  given [T, PathType: GenericPath] => ("src" is HtmlAttribute[PathType, T]) as src2 = _.pathText
  given [T] => ("srcdoc" is HtmlAttribute[Html[?], T]) as srcdoc = _.show
  given [T] => ("srclang" is HtmlAttribute[Text, T]) as srclang = identity(_)
  given [T] => ("srcset" is HtmlAttribute[Text, T]) as srcset = identity(_) // This should be provided by Cataclysm
  given [T] => ("start" is HtmlAttribute[Int, T]) as start = _.show
  given [T] => ("step" is HtmlAttribute[Double, T]) as step = _.toString.show
  given [T] => ("style" is HtmlAttribute[Text, T]) as style = identity(_) // This should be provided by Cataclysm
  given [T] => ("tabindex" is HtmlAttribute[Int, T]) as tabindex = _.show
  given [T] => ("target" is HtmlAttribute[Target, T]) as target = _.show
  given [T] => ("title" is HtmlAttribute[Text, T]) as title = identity(_)
  given [T] => ("translate" is HtmlAttribute[Boolean, T]) as translate = _ => Unset
  given [T] => ("capture" is HtmlAttribute[Capture, T]) as capture = _.show
  given [T] => ("usemap" is HtmlAttribute[Text, T]) as usemap = identity(_) // This needs a representation of HTML names
  given [T] => ("value" is HtmlAttribute[Double, T]) as value = _.toString.show
  given [T] => ("value" is HtmlAttribute[Int, T]) as valueInt = _.show
  given [T] => ("width" is HtmlAttribute[Int, T]) as width = _.show
  given [T] => ("wrap" is HtmlAttribute[Wrap, T]) as wrap = _.show
