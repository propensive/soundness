/*
    Honeycomb, version 0.9.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*

trait Attribute[Key <: Label, -Value, -T]:
  def convert(value: Value): Maybe[Text]
  def rename: Option[Text] = None


object Rel:
  given Show[Rel] = Showable(_).show.dashed

enum Rel:
  case Alternate, Author, Bookmark, Canonical, DnsPrefetch, External, Help, Icon, License, Manifest,
      Modulepreload, Next, Nofollow, Noopener, Noreferrer, Opener, Pingback, Preconnect, Prefetch,
      Preload, Prerender, Prev, Search, Stylesheet, Tag

object Autocomplete:
  given Show[Autocomplete] = Showable(_).show.lower

enum Autocomplete:
  case On, Off

object Method:
  given Show[Method] = Showable(_).show.lower

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
  given clairvoyant.CssSelection[Cls] = cls => t".${cls.name}".s

case class Cls(name: Text)

object DomId:
  given clairvoyant.CssSelection[DomId] = id => t"#${id.name}".s

case class DomId(name: Text)

object Crossorigin:
  given Show[Crossorigin] = Showable(_).show.dashed

enum Crossorigin:
  case UseCredentials, Anonymous

object Dir:
  given Show[Dir] = Showable(_).show.lower

enum Dir:
  case Ltr, Rtl, Auto

object HttpEquiv:
  given Show[HttpEquiv] = Showable(_).show.dashed

enum HttpEquiv:
  case ContentSecurityPolicy, ContentType, DefaultStyle, Refresh

object HType:
  given Show[HType] =
    case DatetimeLocal => t"datetime-local"
    case other         => Showable(other).show.lower

enum HType:
  case Button, Checkbox, Color, Date, DatetimeLocal, Email, File, Hidden, Image, Month, Number,
      Password, Radio, Range, Reset, Search, Submit, Tel, Text, Time, Url, Week

object Kind:
  given Show[Kind] = Showable(_).show.lower

enum Kind:
  case Subtitles, Captions, Descriptions, Chapters, Metadata

object Preload:
  given Show[Preload] = Showable(_).show.lower

enum Preload:
  case None, Metadata, Auto

object Rev:
  given Show[Rev] = Showable(_).show.lower

enum Rev:
  case Alternate, Stylesheet, Start, Next, Prev, Contents, Index, Glossary, Copyright, Chapter,
      Section, Subsection, Appendix, Help, Bookmark

object Sandbox:
  given Show[Sandbox] = Showable(_).show.dashed

enum Sandbox:
  case AllowForms, AllowPointerLock, AllowPopups, AllowPresentation, AllowSameOrigin, AllowScripts,
      AllowTopNavigation

object Scope:
  given Show[Scope] = Showable(_).show.lower

enum Scope:
  case Row, Col, Rowgroup, Colgroup

object Shape:
  given Show[Shape] = Showable(_).show.lower

enum Shape:
  case Circle, Default, Poly, Rect

object Wrap:
  given Show[Wrap] = Showable(_).show.lower

enum Wrap:
  case Soft, Hard

object Attribute:
  given [L <: Label, V, T](using att: clairvoyant.HtmlAttribute[L, V]): Attribute[L, V, T] with
    def convert(value: V): Maybe[Text] = att.serialize(value).show
    override def rename: Option[Text] = Some(att.name.show)

  given any[T, L <: Label]: Attribute[L, Text, T] = identity(_)

  given accept[T]: Attribute["accept", List[Text], T] = _.join(t",")
  
  given acceptCharset[T]: Attribute["acceptCharset", Encoding, T] with
    override def rename: Option[Text] = Some(t"accept-charset")
    def convert(value: Encoding): Text = value.name

  given accesskey[T]: Attribute["accesskey", Char, T] = _.show
  given allowfullscreen[T]: Attribute["allowfullscreen", Boolean, T] = _ => Unset
  given allowpaymentrequest[T]: Attribute["allowpaymentrequest", Boolean, T] = _ => Unset
  given alt[T]: Attribute["alt", Text, T] = identity(_)
  given async[T]: Attribute["async", Boolean, T] = _ => Unset
  given autocomplete[T]: Attribute["autocomplete", Autocomplete, T] = _.show
  given autocomplete2[T]: Attribute["autocomplete", Boolean, T] = if _ then Unset else t"off"
  given autoplay[T]: Attribute["autoplay", Boolean, T] = _ => Unset
  given autofocus[T]: Attribute["autofocus", Boolean, T] = _ => Unset
  given border[T]: Attribute["border", Boolean, T] = if _ then t"1" else t""
  given charset[T]: Attribute["acceptCharset", Encoding, T] = _.name
  given checkedBoolean[T]: Attribute["checked", Boolean, T] = _ => Unset
  given cite[T]: Attribute["cite", Text, T] = identity(_)

  given hclass[T]: Attribute["hclass", List[Cls], T] with
    override def rename: Option[Text] = Some(t"class")
    def convert(value: List[Cls]): Text = value.map(_.name).join(t" ")
 
  given hclass2[T]: Attribute["hclass", Cls, T] with
    override def rename: Option[Text] = Some(t"class")
    def convert(value: Cls): Text = value.name
 
  given code[T]: Attribute["code", Text, T] = identity(_) // MediaError
  given codebase[T]: Attribute["codebase", Text, T] = identity(_)
  given cols[T]: Attribute["cols", Int, T] = _.show
  given colspan[T]: Attribute["colspan", Int, T] = _.show
  given content[T]: Attribute["content", Text, T] = identity(_)
  given contenteditable[T]: Attribute["contenteditable", Boolean, T] = if _ then t"true" else t"false"
  given controls[T]: Attribute["controls", Boolean, T] = _ => Unset
  given coords[T]: Attribute["coords", Seq[Double], T] = _.map(Showable(_).show).join(t",")
  given crossorigin[T]: Attribute["crossorigin", Crossorigin, T] = _.show
  given data[T]: Attribute["data", Text, T] = identity(_)
  given datetime[T]: Attribute["datetime", Text, T] = identity(_) // To be provided by Temporaneous
  given default[T]: Attribute["default", Boolean, T] = _ => Unset
  given defer[T]: Attribute["defer", Boolean, T] = _ => Unset
  given dir[T]: Attribute["dir", Dir, T] = _.show
  given dirname[T]: Attribute["dirname", Text, T] = identity(_) // Should be the name of an input in a form, followed by `.dir`
  given disabled[T]: Attribute["disabled", Boolean, T] = _ => Unset
  given download[T]: Attribute["download", Text, T] = identity(_) // should be a filename, but probably best as `Text`
  given draggable[T]: Attribute["draggable", Boolean, T] = if _ then t"true" else t"false"
  given enctype[T]: Attribute["enctype", Text, T] = identity(_) // provided by Gesticulate
  
  given hfor[T]: Attribute["hfor", DomId, T] with
    override def rename: Option[Text] = Some(t"for")
    def convert(value: DomId): Text = value.name
  
  given hfors[T]: Attribute["hfor", Seq[DomId], T] with
    override def rename: Option[Text] = Some(t"for")
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given form[T]: Attribute["form", DomId, T] = _.name
  given formaction[T]: Attribute["formaction", Text, T] = identity(_) // Provided by Scintillate
  given formenctype[T]: Attribute["formenctype", Text, T] = identity(_)
  given formmethod[T]: Attribute["formmethod", Method, T] = _.show
  given formnovalidate[T]: Attribute["formnovalidate", Boolean, T] = _ => Unset
  given formtarget[T]: Attribute["formtarget", Target, T] = _.show
  given headers[T]: Attribute["headers", DomId, T] = _.name
  given headers2[T]: Attribute["headers", Set[DomId], T] = _.map(_.name).join(t" ")
  given height[T]: Attribute["height", Int, T] = _.show
  given hidden[T]: Attribute["hidden", Boolean, T] = _ => Unset
  given high[T]: Attribute["high", Double, T] = Showable(_).show
  given href: Attribute["href", Text, Text] = identity(_)
  given hreflang[T]: Attribute["hreflang", Text, T] = identity(_) // Needs to be provided by Cosmopolite
  
  given httpEquiv[T]: Attribute["httpEquiv", HttpEquiv, T] with
    override def rename: Option[Text] = Some(t"http-equiv")
    def convert(value: HttpEquiv): Text = value.show
  
  given id[T]: Attribute["id", DomId, T] = _.name
  given ismap[T]: Attribute["ismap", Boolean, T] = _ => Unset
  given kind[T]: Attribute["kind", Kind, T] = _.show
  given label[T]: Attribute["label", Text, T] = identity(_)
  given lang[T]: Attribute["lang", Text, T] = identity(_) // Should be provided by Cosmopolite
  given list[T]: Attribute["list", Seq[DomId], T] = _.map(_.name).join(t" ")
  given list2[T]: Attribute["list", DomId, T] = _.name
  given loop[T]: Attribute["loop", Boolean, T] = _ => Unset
  given low[T]: Attribute["low", Double, T] = Showable(_).show
  given manifest[T]: Attribute["manifest", Text, T] = identity(_) // Provided by Scintillate
  given max[T]: Attribute["max", Double | Int, T] = Showable(_).show
  given maxlength[T]: Attribute["maxlength", Int, T] = _.show
  given minlength[T]: Attribute["minlength", Int, T] = _.show
  given media[T]: Attribute["media", Text, T] = identity(_) // Should be provided by Cataract
  given method[T]: Attribute["method", Method, T] = _.show
  given min[T]: Attribute["min", Double | Int, T] = Showable(_).show
  given multiple[T]: Attribute["multiple", Boolean, T] = _ => Unset
  given muted[T]: Attribute["muted", Boolean, T] = _ => Unset
  given name[T]: Attribute["name", Text, T] = identity(_) // Should provide special `name` identifiers
  given name2: Attribute["name", Target, "iframe" | "object"] = _.show
  given nonce[T]: Attribute["nonce", Text, T] = identity(_) // Should be provided by Gastronomy
  given novalidate[T]: Attribute["novalidate", Boolean, T] = _ => Unset
  given open[T]: Attribute["open", Boolean, T] = _ => Unset
  given optimum[T]: Attribute["optimum", Double, T] = Showable(_).show
  given pattern[T]: Attribute["pattern", Text, T] = identity(_) // Provide with Kaleidoscope
  given placeholder[T]: Attribute["placeholder", Text, T] = identity(_)
  given poster[T]: Attribute["poster", Text, T] = identity(_)
  given preload[T]: Attribute["preload", Preload, T] = _.show
  given readonly[T]: Attribute["readonly", Boolean, T] = _ => Unset
  given referrerpolicy[T]: Attribute["referrerpolicy", Text, T] = identity(_)
  given rel[T]: Attribute["rel", Rel, T] = _.show
  given rel2[T]: Attribute["rel", Seq[Rel], T] = _.map(_.show).join(t" ")
  given required[T]: Attribute["required", Boolean, T] = _ => Unset
  given rev[T]: Attribute["rev", Rev, T] = _.show
  given rows[T]: Attribute["rows", Int, T] = _.show
  given rowspan[T]: Attribute["rowspan", Int, T] = _.show
  given sandbox[T]: Attribute["sandbox", Sandbox, T] = _.show
  given scope[T]: Attribute["scope", Scope, T] = _.show
  given selected[T]: Attribute["selected", Boolean, T] = _ => Unset
  given shape[T]: Attribute["shape", Shape, T] = _.show
  given size[T]: Attribute["size", Int, T] = _.show
  given sizes[T]: Attribute["sizes", Text, T] = identity(_) // This should perhaps be a Map
  given slot[T]: Attribute["slot", Text, T] = identity(_)
  given span[T]: Attribute["span", Int, T] = _.show
  given spellcheck[T]: Attribute["spellcheck", Boolean, T] = if _ then t"true" else t"false"
  given src[T]: Attribute["src", Text, T] = identity(_)
  given srcdoc[T]: Attribute["srcdoc", Html[?], T] = _.show
  given srclang[T]: Attribute["srclang", Text, T] = identity(_)
  given srcset[T]: Attribute["srcset", Text, T] = identity(_) // This should be provided by Cataract
  given start[T]: Attribute["start", Int, T] = _.show
  given step[T]: Attribute["step", Double, T] = Showable(_).show
  given style[T]: Attribute["style", Text, T] = identity(_) // This should be provided by Cataract
  given tabindex[T]: Attribute["tabindex", Int, T] = _.show
  given target[T]: Attribute["target", Target, T] = _.show
  given title[T]: Attribute["title", Text, T] = identity(_)
  given translate[T]: Attribute["translate", Boolean, T] = _ => Unset
  
  given htype[T]: Attribute["htype", HType, T] with
    override def rename: Option[Text] = Some(t"type")
    def convert(value: HType): Text = value.show
  
  given usemap[T]: Attribute["usemap", Text, T] = identity(_) // This needs a representation of HTML names
  given value[T]: Attribute["value", Double, T] = Showable(_).show
  given valueInt[T]: Attribute["value", Int, T] = _.show
  given width[T]: Attribute["width", Int, T] = _.show
  given wrap[T]: Attribute["wrap", Wrap, T] = _.show

extension (sc: StringContext)
  def cls(): Cls = Cls(Text(sc.parts.head))
  def id(): DomId = DomId(Text(sc.parts.head))
