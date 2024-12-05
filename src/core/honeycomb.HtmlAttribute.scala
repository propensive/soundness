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

trait HtmlAttribute[-ValueType]:
  type Self <: Label
  type Target
  def convert(value: ValueType): Optional[Text | HtmlAttribute.NotShown.type]
  def rename: Optional[Text] = Unset

object HtmlAttribute:
  object NotShown

  given [LabelType <: Label: GenericHtmlAttribute[ValueType], ValueType]
      => LabelType is HtmlAttribute[ValueType]:

    def convert(value: ValueType): Optional[Text] = LabelType.serialize(value).show
    override def rename: Optional[Text] = LabelType.name.show

  //given [LabelType <: Label] => LabelType is HtmlAttribute[Text] as any = identity(_)
  given ("accept" is HtmlAttribute[List[Text]]) as accept = _.join(t",")

  given ("acceptCharset" is HtmlAttribute[Encoding]) as acceptCharset:
    override def rename: Optional[Text] = t"accept-charset"
    def convert(value: Encoding): Text = value.name

  given ("accesskey" is HtmlAttribute[Char]) as accesskey = _.show
  given ("action" is HtmlAttribute[Text]) as action = identity(_)
  given ("allowfullscreen" is HtmlAttribute[Boolean]) as allowfullscreen = _ => Unset
  given ("allowpaymentrequest" is HtmlAttribute[Boolean]) as allowpaymentrequest = _ => Unset
  given ("alt" is HtmlAttribute[Text]) as alt = identity(_)
  given ("async" is HtmlAttribute[Boolean]) as async = _ => Unset
  given ("autocomplete" is HtmlAttribute[Autocomplete]) as autocomplete = _.show
  given ("autocomplete" is HtmlAttribute[Boolean]) as autocomplete2 = t"off".unless(_)
  given ("autoplay" is HtmlAttribute[Boolean]) as autoplay = _ => Unset
  given ("autofocus" is HtmlAttribute[Boolean]) as autofocus = _ => Unset
  given ("border" is HtmlAttribute[Boolean]) as border = if _ then t"1" else t""
  given ("charset" is HtmlAttribute[Encoding]) as charset = _.name
  given ("checked" is HtmlAttribute[Boolean]) as checkedBoolean = _ => Unset
  given ("cite" is HtmlAttribute[Text]) as cite = identity(_)

  given ("class" is HtmlAttribute[List[CssClass]]) as `class`:
    override def rename: Optional[Text] = t"class"
    def convert(value: List[CssClass]): Text = value.map(_.name).join(t" ")

  given HtmlAttribute[CssClass] as class2:
    override def rename: Optional[Text] = t"class"
    def convert(value: CssClass): Text = value.name

  given ("code" is HtmlAttribute[Text]) as code = identity(_) // MediaError
  given ("codebase" is HtmlAttribute[Text]) as codebase = identity(_)
  given ("cols" is HtmlAttribute[Int]) as cols = _.show
  given ("colspan" is HtmlAttribute[Int]) as colspan = _.show
  given ("content" is HtmlAttribute[Text]) as content = identity(_)
  given ("contenteditable" is HtmlAttribute[Boolean]) as contenteditable = if _ then t"true" else t"false"
  given ("controls" is HtmlAttribute[Boolean]) as controls = _ => Unset
  given ("coords" is HtmlAttribute[Seq[Double]]) as coords = _.map(_.toString.show).join(t",")
  given ("crossorigin" is HtmlAttribute[Crossorigin]) as crossorigin = _.show
  given ("data" is HtmlAttribute[Text]) as data = identity(_)
  given ("datetime" is HtmlAttribute[Text]) as datetime = identity(_) // To be provided by Aviation
  given ("default" is HtmlAttribute[Boolean]) as default = _ => Unset
  given ("defer" is HtmlAttribute[Boolean]) as defer = _ => Unset
  given ("dir" is HtmlAttribute[HDir]) as dir = _.show

  // Should be the name of an input in a form, followed by `.dir`
  given ("dirname" is HtmlAttribute[Text]) as dirname = identity(_)

  given ("disabled" is HtmlAttribute[Boolean]) as disabled = _ => Unset

  // should be a filename, but probably best as `Text`
  given ("download" is HtmlAttribute[Text]) as download = identity(_)

  given ("draggable" is HtmlAttribute[Boolean]) as draggable = _.toString.tt
  given ("enctype" is HtmlAttribute[Text]) as enctype = identity(_) // provided by Gesticulate

  given ("hfor" is HtmlAttribute[DomId]) as hfor:
    override def rename: Optional[Text] = t"for"
    def convert(value: DomId): Text = value.name

  given ("hfor" is HtmlAttribute[Seq[DomId]]) as hfors:
    override def rename: Optional[Text] = t"for"
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given ("for" is HtmlAttribute[DomId]) as `for`:
    def convert(value: DomId): Text = value.name

  given ("for" is HtmlAttribute[Seq[DomId]]) as fors:
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given ("form" is HtmlAttribute[DomId]) as form = _.name
  given ("formaction" is HtmlAttribute[Text]) as formaction = identity(_) // Provided by Scintillate
  given ("formenctype" is HtmlAttribute[Text]) as formenctype = identity(_)
  given ("formmethod" is HtmlAttribute[Method]) as formmethod = _.show
  given ("formnovalidate" is HtmlAttribute[Boolean]) as formnovalidate = _ => Unset
  given ("formtarget" is HtmlAttribute[Target]) as formtarget = _.show
  given ("headers" is HtmlAttribute[DomId]) as headers = _.name
  given ("headers" is HtmlAttribute[Set[DomId]]) as headers2 = _.map(_.name).join(t" ")
  given ("height" is HtmlAttribute[Int]) as height = _.show
  given ("hidden" is HtmlAttribute[Boolean]) as hidden = _ => Unset
  given ("high" is HtmlAttribute[Double]) as high = _.toString.show
  given ("href" is HtmlAttribute[Text]) as href = identity(_)
  given [UrlType: GenericUrl] => ("href" is HtmlAttribute[UrlType]) as href2 = UrlType.text(_)
  given ("hreflang" is HtmlAttribute[Text]) as hreflang = identity(_) // Needs to be provided by Cosmopolite

  given ("httpEquiv" is HtmlAttribute[HttpEquiv]) as httpEquiv:
    override def rename: Optional[Text] = t"http-equiv"
    def convert(value: HttpEquiv): Text = value.show

  given ("id" is HtmlAttribute[DomId]) as id = _.name
  given ("ismap" is HtmlAttribute[Boolean]) as ismap = _ => Unset
  given ("kind" is HtmlAttribute[Kind]) as kind = _.show
  given ("label" is HtmlAttribute[Text]) as label = identity(_)
  given ("lang" is HtmlAttribute[Text]) as lang = identity(_) // Should be provided by Cosmopolite
  given ("list" is HtmlAttribute[Seq[DomId]]) as list = _.map(_.name).join(t" ")
  given ("list" is HtmlAttribute[DomId]) as list2 = _.name
  given ("loop" is HtmlAttribute[Boolean]) as loop = _ => Unset
  given ("low" is HtmlAttribute[Double]) as low = _.toString.tt
  given ("manifest" is HtmlAttribute[Text]) as manifest = identity(_) // Provided by Scintillate
  given ("max" is HtmlAttribute[Double | Int]) as max = _.toString.show
  given ("maxlength" is HtmlAttribute[Int]) as maxlength = _.show
  given ("minlength" is HtmlAttribute[Int]) as minlength = _.show
  given ("media" is HtmlAttribute[Text]) as media = identity(_) // Should be provided by Cataclysm
  given ("method" is HtmlAttribute[Method]) as method = _.show
  given ("min" is HtmlAttribute[Double | Int]) as min = _.toString.show
  given ("multiple" is HtmlAttribute[Boolean]) as multiple = _ => Unset
  given ("muted" is HtmlAttribute[Boolean]) as muted = _ => Unset
  given ("name" is HtmlAttribute[Text]) as name = identity(_) // Should provide special `name` identifiers
  given ("name" is HtmlAttribute[Target] onto "object") as name2 = _.show
  given ("name" is HtmlAttribute[Target] onto "iframe") as name3 = _.show
  given ("nonce" is HtmlAttribute[Text]) as nonce = identity(_) // Should be provided by Gastronomy
  given ("novalidate" is HtmlAttribute[Boolean]) as novalidate = _ => Unset
  given ("open" is HtmlAttribute[Boolean]) as open = _ => Unset
  given ("optimum" is HtmlAttribute[Double]) as optimum = _.toString.show
  given ("pattern" is HtmlAttribute[Text]) as pattern = identity(_) // Provide with Kaleidoscope
  given ("placeholder" is HtmlAttribute[Text]) as placeholder = identity(_)
  given ("playsinline" is HtmlAttribute[Boolean]) as playsinline = _ => Unset
  given ("poster" is HtmlAttribute[Text]) as poster = identity(_)
  given ("preload" is HtmlAttribute[Preload]) as preload = _.show
  given ("readonly" is HtmlAttribute[Boolean]) as readonly = _ => Unset
  given ("referrerpolicy" is HtmlAttribute[Text]) as referrerpolicy = identity(_)
  given ("rel" is HtmlAttribute[Rel]) as rel = _.show
  given ("rel" is HtmlAttribute[Seq[Rel]]) as rel2 = _.map(_.show).join(t" ")
  given ("required" is HtmlAttribute[Boolean]) as required = _ => Unset
  given ("rev" is HtmlAttribute[Rev]) as rev = _.show
  given ("rows" is HtmlAttribute[Int]) as rows = _.show
  given ("rowspan" is HtmlAttribute[Int]) as rowspan = _.show
  given ("sandbox" is HtmlAttribute[Sandbox]) as sandbox = _.show
  given ("scope" is HtmlAttribute[Scope]) as scope = _.show
  given ("selected" is HtmlAttribute[Boolean]) as selected = _ => Unset
  given ("shape" is HtmlAttribute[Shape]) as shape = _.show
  given ("size" is HtmlAttribute[Int]) as size = _.show
  given ("sizes" is HtmlAttribute[Text]) as sizes = identity(_) // This should perhaps be a Map
  given ("slot" is HtmlAttribute[Text]) as slot = identity(_)
  given ("span" is HtmlAttribute[Int]) as span = _.show
  given ("spellcheck" is HtmlAttribute[Boolean]) as spellcheck = _.toString.tt
  given ("src" is HtmlAttribute[Text]) as src = identity(_)
  given [PathType: GenericPath] => ("src" is HtmlAttribute[PathType]) as src2 = _.pathText
  given ("srcdoc" is HtmlAttribute[Html[?]]) as srcdoc = _.show
  given ("srclang" is HtmlAttribute[Text]) as srclang = identity(_)
  given ("srcset" is HtmlAttribute[Text]) as srcset = identity(_) // This should be provided by Cataclysm
  given ("start" is HtmlAttribute[Int]) as start = _.show
  given ("step" is HtmlAttribute[Double]) as step = _.toString.show
  given ("style" is HtmlAttribute[Text]) as style = identity(_) // This should be provided by Cataclysm
  given ("tabindex" is HtmlAttribute[Int]) as tabindex = _.show
  given ("target" is HtmlAttribute[Target]) as target = _.show
  given ("title" is HtmlAttribute[Text]) as title = identity(_)
  given ("translate" is HtmlAttribute[Boolean]) as translate = _ => Unset
  given ("capture" is HtmlAttribute[Capture]) as capture = _.show
  given ("usemap" is HtmlAttribute[Text]) as usemap = identity(_) // This needs a representation of HTML names
  given ("value" is HtmlAttribute[Double]) as value = _.toString.show
  given ("value" is HtmlAttribute[Int]) as valueInt = _.show
  given ("value" is HtmlAttribute[Text]) as valueText = identity(_)
  given ("width" is HtmlAttribute[Int]) as width = _.show
  given ("wrap" is HtmlAttribute[Wrap]) as wrap = _.show
