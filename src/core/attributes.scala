/*
    Honeycomb, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import spectacular.*

trait HtmlAttribute[KeyType <: Label, -ValueType, -T]:
  def convert(value: ValueType): Optional[Text]
  def rename: Option[Text] = None

object Rel:
  given Show[Rel] = _.toString.show.uncamel.kebab

enum Rel:
  case Alternate, Author, Bookmark, Canonical, DnsPrefetch, External, Help, Icon, License, Manifest,
      Modulepreload, Next, Nofollow, Noopener, Noreferrer, Opener, Pingback, Preconnect, Prefetch,
      Preload, Prerender, Prev, Search, Stylesheet, Tag

object Autocomplete:
  given Show[Autocomplete] = _.toString.show.lower

enum Autocomplete:
  case On, Off

object Method:
  given Show[Method] = _.toString.show.lower

enum Method:
  case Post, Get, Dialog

object Target:
  given Show[Target] =
    case Browse(value) => value
    case Self          => t"_self"
    case Blank         => t"_blank"
    case Parent        => t"_parent"
    case Top           => t"_top"

enum Target:
  case Browse(value: Text)
  case Self, Blank, Parent, Top

object Cls:
  given GenericCssSelection[Cls] = cls => t".${cls.name}"

case class Cls(name: Text)

object DomId:
  given GenericCssSelection[DomId] = id => t"#${id.name}"

case class DomId(name: Text)

object Crossorigin:
  given Show[Crossorigin] = _.toString.show.uncamel.kebab

enum Crossorigin:
  case UseCredentials, Anonymous

object HDir:
  given Show[HDir] = _.toString.show.lower

enum HDir:
  case Ltr, Rtl, Auto

object HttpEquiv:
  given Show[HttpEquiv] = _.toString.show.uncamel.kebab

enum HttpEquiv:
  case ContentSecurityPolicy, ContentType, DefaultStyle, Refresh

object HType:
  given Show[HType] =
    case DatetimeLocal => t"datetime-local"
    case other         => other.toString.show.lower

enum HType:
  case Button, Checkbox, Color, Date, DatetimeLocal, Email, File, Hidden, Image, Month, Number,
      Password, Radio, Range, Reset, Search, Submit, Tel, Text, Time, Url, Week

object Kind:
  given Show[Kind] = _.toString.show.lower

enum Kind:
  case Subtitles, Captions, Descriptions, Chapters, Metadata

object Preload:
  given Show[Preload] = _.toString.show.lower

enum Preload:
  case None, Metadata, Auto

object Rev:
  given Show[Rev] = _.toString.show.lower

enum Rev:
  case Alternate, Stylesheet, Start, Next, Prev, Contents, Index, Glossary, Copyright, Chapter,
      Section, Subsection, Appendix, Help, Bookmark

object Sandbox:
  given Show[Sandbox] = _.toString.show.uncamel.kebab

enum Sandbox:
  case AllowForms, AllowPointerLock, AllowPopups, AllowPresentation, AllowSameOrigin, AllowScripts,
      AllowTopNavigation

object Scope:
  given Show[Scope] = _.toString.show.lower

enum Scope:
  case Row, Col, Rowgroup, Colgroup

object Shape:
  given Show[Shape] = _.toString.show.lower

enum Shape:
  case Circle, Default, Poly, Rect

object Wrap:
  given Show[Wrap] = _.toString.show.lower

enum Wrap:
  case Soft, Hard

object HtmlAttribute:
  given [L <: Label, V, T](using att: GenericHtmlAttribute[L, V]): HtmlAttribute[L, V, T] with
    def convert(value: V): Optional[Text] = att.serialize(value).show
    override def rename: Option[Text] = Some(att.name.show)

  given any[T, L <: Label]: HtmlAttribute[L, Text, T] = identity(_)

  given accept[T]: HtmlAttribute["accept", List[Text], T] = _.join(t",")
  
  given acceptCharset[T]: HtmlAttribute["acceptCharset", Encoding, T] with
    override def rename: Option[Text] = Some(t"accept-charset")
    def convert(value: Encoding): Text = value.name

  given accesskey[T]: HtmlAttribute["accesskey", Char, T] = _.show
  given allowfullscreen[T]: HtmlAttribute["allowfullscreen", Boolean, T] = _ => Unset
  given allowpaymentrequest[T]: HtmlAttribute["allowpaymentrequest", Boolean, T] = _ => Unset
  given alt[T]: HtmlAttribute["alt", Text, T] = identity(_)
  given async[T]: HtmlAttribute["async", Boolean, T] = _ => Unset
  given autocomplete[T]: HtmlAttribute["autocomplete", Autocomplete, T] = _.show
  given autocomplete2[T]: HtmlAttribute["autocomplete", Boolean, T] = if _ then Unset else t"off"
  given autoplay[T]: HtmlAttribute["autoplay", Boolean, T] = _ => Unset
  given autofocus[T]: HtmlAttribute["autofocus", Boolean, T] = _ => Unset
  given border[T]: HtmlAttribute["border", Boolean, T] = if _ then t"1" else t""
  given charset[T]: HtmlAttribute["acceptCharset", Encoding, T] = _.name
  given checkedBoolean[T]: HtmlAttribute["checked", Boolean, T] = _ => Unset
  given cite[T]: HtmlAttribute["cite", Text, T] = identity(_)

  given hclass[T]: HtmlAttribute["hclass", List[Cls], T] with
    override def rename: Option[Text] = Some(t"class")
    def convert(value: List[Cls]): Text = value.map(_.name).join(t" ")
 
  given hclass2[T]: HtmlAttribute["hclass", Cls, T] with
    override def rename: Option[Text] = Some(t"class")
    def convert(value: Cls): Text = value.name
 
  given code[T]: HtmlAttribute["code", Text, T] = identity(_) // MediaError
  given codebase[T]: HtmlAttribute["codebase", Text, T] = identity(_)
  given cols[T]: HtmlAttribute["cols", Int, T] = _.show
  given colspan[T]: HtmlAttribute["colspan", Int, T] = _.show
  given content[T]: HtmlAttribute["content", Text, T] = identity(_)
  given contenteditable[T]: HtmlAttribute["contenteditable", Boolean, T] = if _ then t"true" else t"false"
  given controls[T]: HtmlAttribute["controls", Boolean, T] = _ => Unset
  given coords[T]: HtmlAttribute["coords", Seq[Double], T] = _.map(_.toString.show).join(t",")
  given crossorigin[T]: HtmlAttribute["crossorigin", Crossorigin, T] = _.show
  given data[T]: HtmlAttribute["data", Text, T] = identity(_)
  given datetime[T]: HtmlAttribute["datetime", Text, T] = identity(_) // To be provided by Aviation
  given default[T]: HtmlAttribute["default", Boolean, T] = _ => Unset
  given defer[T]: HtmlAttribute["defer", Boolean, T] = _ => Unset
  given dir[T]: HtmlAttribute["dir", HDir, T] = _.show
  given dirname[T]: HtmlAttribute["dirname", Text, T] = identity(_) // Should be the name of an input in a form, followed by `.dir`
  given disabled[T]: HtmlAttribute["disabled", Boolean, T] = _ => Unset
  given download[T]: HtmlAttribute["download", Text, T] = identity(_) // should be a filename, but probably best as `Text`
  given draggable[T]: HtmlAttribute["draggable", Boolean, T] = if _ then t"true" else t"false"
  given enctype[T]: HtmlAttribute["enctype", Text, T] = identity(_) // provided by Gesticulate
  
  given hfor[T]: HtmlAttribute["hfor", DomId, T] with
    override def rename: Option[Text] = Some(t"for")
    def convert(value: DomId): Text = value.name
  
  given hfors[T]: HtmlAttribute["hfor", Seq[DomId], T] with
    override def rename: Option[Text] = Some(t"for")
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")
  
  given `for`[T]: HtmlAttribute["for", DomId, T] with
    def convert(value: DomId): Text = value.name
  
  given fors[T]: HtmlAttribute["for", Seq[DomId], T] with
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")


  given form[T]: HtmlAttribute["form", DomId, T] = _.name
  given formaction[T]: HtmlAttribute["formaction", Text, T] = identity(_) // Provided by Scintillate
  given formenctype[T]: HtmlAttribute["formenctype", Text, T] = identity(_)
  given formmethod[T]: HtmlAttribute["formmethod", Method, T] = _.show
  given formnovalidate[T]: HtmlAttribute["formnovalidate", Boolean, T] = _ => Unset
  given formtarget[T]: HtmlAttribute["formtarget", Target, T] = _.show
  given headers[T]: HtmlAttribute["headers", DomId, T] = _.name
  given headers2[T]: HtmlAttribute["headers", Set[DomId], T] = _.map(_.name).join(t" ")
  given height[T]: HtmlAttribute["height", Int, T] = _.show
  given hidden[T]: HtmlAttribute["hidden", Boolean, T] = _ => Unset
  given high[T]: HtmlAttribute["high", Double, T] = _.toString.show
  given href: HtmlAttribute["href", Text, Text] = identity(_)
  given hreflang[T]: HtmlAttribute["hreflang", Text, T] = identity(_) // Needs to be provided by Cosmopolite
  
  given httpEquiv[T]: HtmlAttribute["httpEquiv", HttpEquiv, T] with
    override def rename: Option[Text] = Some(t"http-equiv")
    def convert(value: HttpEquiv): Text = value.show
  
  given id[T]: HtmlAttribute["id", DomId, T] = _.name
  given ismap[T]: HtmlAttribute["ismap", Boolean, T] = _ => Unset
  given kind[T]: HtmlAttribute["kind", Kind, T] = _.show
  given label[T]: HtmlAttribute["label", Text, T] = identity(_)
  given lang[T]: HtmlAttribute["lang", Text, T] = identity(_) // Should be provided by Cosmopolite
  given list[T]: HtmlAttribute["list", Seq[DomId], T] = _.map(_.name).join(t" ")
  given list2[T]: HtmlAttribute["list", DomId, T] = _.name
  given loop[T]: HtmlAttribute["loop", Boolean, T] = _ => Unset
  given low[T]: HtmlAttribute["low", Double, T] = _.toString.show
  given manifest[T]: HtmlAttribute["manifest", Text, T] = identity(_) // Provided by Scintillate
  given max[T]: HtmlAttribute["max", Double | Int, T] = _.toString.show
  given maxlength[T]: HtmlAttribute["maxlength", Int, T] = _.show
  given minlength[T]: HtmlAttribute["minlength", Int, T] = _.show
  given media[T]: HtmlAttribute["media", Text, T] = identity(_) // Should be provided by Cataclysm
  given method[T]: HtmlAttribute["method", Method, T] = _.show
  given min[T]: HtmlAttribute["min", Double | Int, T] = _.toString.show
  given multiple[T]: HtmlAttribute["multiple", Boolean, T] = _ => Unset
  given muted[T]: HtmlAttribute["muted", Boolean, T] = _ => Unset
  given name[T]: HtmlAttribute["name", Text, T] = identity(_) // Should provide special `name` identifiers
  given name2: HtmlAttribute["name", Target, "iframe" | "object"] = _.show
  given nonce[T]: HtmlAttribute["nonce", Text, T] = identity(_) // Should be provided by Gastronomy
  given novalidate[T]: HtmlAttribute["novalidate", Boolean, T] = _ => Unset
  given open[T]: HtmlAttribute["open", Boolean, T] = _ => Unset
  given optimum[T]: HtmlAttribute["optimum", Double, T] = _.toString.show
  given pattern[T]: HtmlAttribute["pattern", Text, T] = identity(_) // Provide with Kaleidoscope
  given placeholder[T]: HtmlAttribute["placeholder", Text, T] = identity(_)
  given poster[T]: HtmlAttribute["poster", Text, T] = identity(_)
  given preload[T]: HtmlAttribute["preload", Preload, T] = _.show
  given readonly[T]: HtmlAttribute["readonly", Boolean, T] = _ => Unset
  given referrerpolicy[T]: HtmlAttribute["referrerpolicy", Text, T] = identity(_)
  given rel[T]: HtmlAttribute["rel", Rel, T] = _.show
  given rel2[T]: HtmlAttribute["rel", Seq[Rel], T] = _.map(_.show).join(t" ")
  given required[T]: HtmlAttribute["required", Boolean, T] = _ => Unset
  given rev[T]: HtmlAttribute["rev", Rev, T] = _.show
  given rows[T]: HtmlAttribute["rows", Int, T] = _.show
  given rowspan[T]: HtmlAttribute["rowspan", Int, T] = _.show
  given sandbox[T]: HtmlAttribute["sandbox", Sandbox, T] = _.show
  given scope[T]: HtmlAttribute["scope", Scope, T] = _.show
  given selected[T]: HtmlAttribute["selected", Boolean, T] = _ => Unset
  given shape[T]: HtmlAttribute["shape", Shape, T] = _.show
  given size[T]: HtmlAttribute["size", Int, T] = _.show
  given sizes[T]: HtmlAttribute["sizes", Text, T] = identity(_) // This should perhaps be a Map
  given slot[T]: HtmlAttribute["slot", Text, T] = identity(_)
  given span[T]: HtmlAttribute["span", Int, T] = _.show
  given spellcheck[T]: HtmlAttribute["spellcheck", Boolean, T] = if _ then t"true" else t"false"
  given src[T]: HtmlAttribute["src", Text, T] = identity(_)
  given src2[T, PathType: GenericPath]: HtmlAttribute["src", PathType, T] = _.pathText
  given srcdoc[T]: HtmlAttribute["srcdoc", Html[?], T] = _.show
  given srclang[T]: HtmlAttribute["srclang", Text, T] = identity(_)
  given srcset[T]: HtmlAttribute["srcset", Text, T] = identity(_) // This should be provided by Cataclysm
  given start[T]: HtmlAttribute["start", Int, T] = _.show
  given step[T]: HtmlAttribute["step", Double, T] = _.toString.show
  given style[T]: HtmlAttribute["style", Text, T] = identity(_) // This should be provided by Cataclysm
  given tabindex[T]: HtmlAttribute["tabindex", Int, T] = _.show
  given target[T]: HtmlAttribute["target", Target, T] = _.show
  given title[T]: HtmlAttribute["title", Text, T] = identity(_)
  given translate[T]: HtmlAttribute["translate", Boolean, T] = _ => Unset
  
  given htype[T]: HtmlAttribute["htype", HType, T] with
    override def rename: Option[Text] = Some(t"type")
    def convert(value: HType): Text = value.show
  
  given usemap[T]: HtmlAttribute["usemap", Text, T] = identity(_) // This needs a representation of HTML names
  given value[T]: HtmlAttribute["value", Double, T] = _.toString.show
  given valueInt[T]: HtmlAttribute["value", Int, T] = _.show
  given width[T]: HtmlAttribute["width", Int, T] = _.show
  given wrap[T]: HtmlAttribute["wrap", Wrap, T] = _.show

extension (sc: StringContext)
  def cls(): Cls = Cls(Text(sc.parts.head))
  def id(): DomId = DomId(Text(sc.parts.head))
