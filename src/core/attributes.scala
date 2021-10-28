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
  def convert(value: Value): Maybe[Txt]
  def rename: Option[Txt] = None

enum Rel:
  case Alternate, Author, Bookmark, Canonical, DnsPrefetch, External, Help, Icon, License, Manifest,
      Modulepreload, Next, Nofollow, Noopener, Noreferrer, Opener, Pingback, Preconnect, Prefetch,
      Preload, Prerender, Prev, Search, Stylesheet, Tag

  def text: Txt = this match
    case DnsPrefetch => str"dns-prefetch"
    case other       => Txt(toString).lower

enum Autocomplete:
  case On, Off

  def text: Txt = Txt(toString).lower

enum Method:
  case Post, Get, Dialog
  
  def text: Txt = Txt(toString).lower

enum Target:
  case Browse(value: Txt)
  case Self, Blank, Parent, Top

  def text: Txt = this match
    case Browse(value) => value
    case Self          => str"_self"
    case Blank         => str"_blank"
    case Parent        => str"_parent"
    case Top           => str"_top"

extension (sc: StringContext)
  def cls(): Cls = Cls(Txt(sc.parts.head))
  def id(): DomId = DomId(Txt(sc.parts.head))


object Cls:
  given clairvoyant.CssSelection[Cls] = cls => str".${cls.name}".s

case class Cls(name: Txt)

object DomId:
  given clairvoyant.CssSelection[DomId] = id => str"#${id.name}".s

case class DomId(name: Txt)

enum Crossorigin:
  case UseCredentials, Anonymous

  def text: Txt = this match
    case Anonymous      => str"anonymous"
    case UseCredentials => str"use-credentials"

enum Dir:
  case Ltr, Rtl, Auto
  def text: Txt = Txt(toString).lower

enum HttpEquiv:
  case ContentSecurityPolicy, ContentType, DefaultStyle, Refresh

  def text: Txt = this match
    case ContentSecurityPolicy => str"content-security-policy"
    case ContentType           => str"content-type"
    case DefaultStyle          => str"default-style"
    case Refresh               => str"refresh"

enum Kind:
  case Subtitles, Captions, Descriptions, Chapters, Metadata
  
  def text: Txt = Txt(toString).lower

enum Preload:
  case None, Metadata, Auto
  
  def text: Txt = Txt(toString).lower

enum Sandbox:
  case AllowForms, AllowPointerLock, AllowPopups, AllowPresentation, AllowSameOrigin, AllowScripts,
      AllowTopNavigation
  
  def text: Txt = this match
    case AllowForms         => str"allow-forms"
    case AllowPointerLock   => str"allow-pointer-lock"
    case AllowPopups        => str"allow-popups"
    case AllowPresentation  => str"allow-presentation"
    case AllowSameOrigin    => str"allow-same-origin"
    case AllowScripts       => str"allow-scripts"
    case AllowTopNavigation => str"allow-top-navigation"

enum Scope:
  case Row, Col, Rowgroup, Colgroup

  def text: Txt = Txt(toString).lower

enum Shape:
  case Circle, Default, Poly, Rect

  def text: Txt = Txt(toString).lower

enum Wrap:
  case Soft, Hard

  def text: Txt = Txt(toString).lower

object Attribute:
  given [L <: Label, V, T](using att: clairvoyant.HtmlAttribute[L, V]): Attribute[L, V, T] with
    def convert(value: V): Maybe[Txt] = Txt(att.serialize(value))
    override def rename: Option[Txt] = Some(Txt(att.name))

  given any[T, L <: Label]: Attribute[L, Txt, T] = identity(_)

  given accept[T]: Attribute["accept", List[Txt], T] = _.join(str",")
  given accesskey[T]: Attribute["accesskey", Char, T] = _.show
  given allowfullscreen[T]: Attribute["allowfullscreen", Boolean, T] = _ => Unset
  given allowpaymentrequest[T]: Attribute["allowpaymentrequest", Boolean, T] = _ => Unset
  given alt[T]: Attribute["alt", Txt, T] = identity(_)
  given async[T]: Attribute["async", Boolean, T] = _ => Unset
  given autocomplete[T]: Attribute["autocomplete", Autocomplete, T] = _.text
  given autoplay[T]: Attribute["autoplay", Boolean, T] = _ => Unset
  given autofocus[T]: Attribute["autofocus", Boolean, T] = _ => Unset
  given border[T]: Attribute["border", Boolean, T] = if _ then str"1" else str""
  given checkedBoolean[T]: Attribute["checked", Boolean, T] = _ => Unset
  given cite[T]: Attribute["cite", Txt, T] = identity(_)

  given hclass[T]: Attribute["hclass", List[Cls], T] with
    override def rename: Option[Txt] = Some(str"class")
    def convert(value: List[Cls]): Txt = value.map(_.name).join(str" ")
 
  given hclass2[T]: Attribute["hclass", Cls, T] with
    override def rename: Option[Txt] = Some(str"class")
    def convert(value: Cls): Txt = value.name
 
  given code[T]: Attribute["code", Txt, T] = identity(_)
  given codebase[T]: Attribute["codebase", Txt, T] = identity(_)
  given cols[T]: Attribute["cols", Int, T] = _.show
  given colspan[T]: Attribute["colspan", Int, T] = _.show
  given content[T]: Attribute["content", Txt, T] = identity(_)
  given contenteditable[T]: Attribute["contenteditable", Boolean, T] = if _ then str"true" else str"false"
  given controls[T]: Attribute["controls", Boolean, T] = _ => Unset
  given coords[T]: Attribute["coords", Seq[Double], T] = _.map { d => Txt(d.toString) }.join(str",")
  given crossorigin[T]: Attribute["crossorigin", Crossorigin, T] = _.text
  given data[T]: Attribute["data", Txt, T] = identity(_)
  given datetime[T]: Attribute["datetime", Txt, T] = identity(_)
  given default[T]: Attribute["default", Boolean, T] = _ => Unset
  given defer[T]: Attribute["defer", Boolean, T] = _ => Unset
  given dir[T]: Attribute["dir", Dir, T] = _.text
  given dirname[T]: Attribute["dirname", Txt, T] = identity(_)
  given disabled[T]: Attribute["disabled", Boolean, T] = _ => Unset
  given download[T]: Attribute["download", Txt, T] = identity(_)
  given draggable[T]: Attribute["draggable", Boolean, T] = if _ then str"true" else str"false"
  given enctype[T]: Attribute["enctype", Txt, T] = identity(_)
  
  given hfor[T]: Attribute["hfor", DomId, T] with
    override def rename: Option[Txt] = Some(str"for")
    def convert(value: DomId): Txt = value.name
  
  given hfors[T]: Attribute["hfor", Seq[DomId], T] with
    override def rename: Option[Txt] = Some(str"for")
    def convert(value: Seq[DomId]): Txt = value.map(_.name).join(str" ")

  given form[T]: Attribute["form", DomId, T] = _.name
  given formaction[T]: Attribute["formaction", Txt, T] = identity(_)
  given formenctype[T]: Attribute["formenctype", Txt, T] = identity(_)
  given formmethod[T]: Attribute["formmethod", Method, T] = _.text
  given formnovalidate[T]: Attribute["formnovalidate", Boolean, T] = _ => Unset
  given formtarget[T]: Attribute["formtarget", Target, T] = _.text
  given headers[T]: Attribute["headers", DomId, T] = _.name
  given headers2[T]: Attribute["headers", Set[DomId], T] = _.map(_.name).join(str" ")
  given height[T]: Attribute["height", Int, T] = _.show
  given hidden[T]: Attribute["hidden", Boolean, T] = _ => Unset
  given high[T]: Attribute["high", Double, T] = d => Txt(d.toString)
  given href: Attribute["href", Txt, Txt] = identity(_)
  given hreflang[T]: Attribute["hreflang", Txt, T] = identity(_)
  
  given httpEquiv[T]: Attribute["httpEquiv", HttpEquiv, T] with
    override def rename: Option[Txt] = Some(str"http-equiv")
    def convert(value: HttpEquiv): Txt = value.text
  
  given id[T]: Attribute["id", DomId, T] = _.name
  given ismap[T]: Attribute["ismap", Boolean, T] = _ => Unset
  given kind[T]: Attribute["kind", Kind, T] = _.text
  given label[T]: Attribute["label", Txt, T] = identity(_)
  given lang[T]: Attribute["lang", Txt, T] = identity(_)
  given list[T]: Attribute["list", Seq[DomId], T] = _.map(_.name).join(str" ")
  given list2[T]: Attribute["list", DomId, T] = _.name
  given loop[T]: Attribute["loop", Boolean, T] = _ => Unset
  given low[T]: Attribute["low", Double, T] = d => Txt(d.toString)
  given manifest[T]: Attribute["manifest", Txt, T] = identity(_)
  given max[T]: Attribute["max", Double | Int, T] = n => Txt(n.toString)
  given maxlength[T]: Attribute["maxlength", Int, T] = _.show
  given minlength[T]: Attribute["minlength", Int, T] = _.show
  given media[T]: Attribute["media", Txt, T] = identity(_)
  given method[T]: Attribute["method", Method, T] = _.text
  given min[T]: Attribute["min", Double | Int, T] = n => Txt(n.toString)
  given multiple[T]: Attribute["multiple", Boolean, T] = _ => Unset
  given muted[T]: Attribute["muted", Boolean, T] = _ => Unset
  given name[T]: Attribute["name", Txt, T] = identity(_)
  given name2: Attribute["name", Target, "iframe" | "object"] = _.text
  given nonce[T]: Attribute["nonce", Txt, T] = identity(_)
  given novalidate[T]: Attribute["novalidate", Boolean, T] = _ => Unset
  given open[T]: Attribute["open", Boolean, T] = _ => Unset
  given optimum[T]: Attribute["optimum", Double, T] = d => Txt(d.toString)
  given pattern[T]: Attribute["pattern", Txt, T] = identity(_)
  given placeholder[T]: Attribute["placeholder", Txt, T] = identity(_)
  given poster[T]: Attribute["poster", Txt, T] = identity(_)
  given preload[T]: Attribute["preload", Preload, T] = _.text
  given readonly[T]: Attribute["readonly", Boolean, T] = _ => Unset
  given referrerpolicy[T]: Attribute["referrerpolicy", Txt, T] = identity(_)
  given rel[T]: Attribute["rel", Rel, T] = _.text
  given rel2[T]: Attribute["rel", Seq[Rel], T] = _.map(_.text).join(str" ")
  given required[T]: Attribute["required", Boolean, T] = _ => Unset
  given rev[T]: Attribute["rev", Txt, T] = identity(_)
  given rows[T]: Attribute["rows", Int, T] = _.show
  given rowspan[T]: Attribute["rowspan", Int, T] = _.show
  given sandbox[T]: Attribute["sandbox", Sandbox, T] = _.text
  given scope[T]: Attribute["scope", Scope, T] = _.text
  given selected[T]: Attribute["selected", Boolean, T] = _ => Unset
  given shape[T]: Attribute["shape", Shape, T] = _.text
  given size[T]: Attribute["size", Int, T] = _.show
  given sizes[T]: Attribute["sizes", Txt, T] = identity(_) // todo
  given slot[T]: Attribute["slot", Txt, T] = identity(_)
  given span[T]: Attribute["span", Int, T] = _.show
  given spellcheck[T]: Attribute["spellcheck", Boolean, T] = if _ then str"true" else str"false"
  given src[T]: Attribute["src", Txt, T] = identity(_)
  given srcdoc[T]: Attribute["srcdoc", Txt, T] = identity(_) // todo
  given srclang[T]: Attribute["srclang", Txt, T] = identity(_)
  given srcset[T]: Attribute["srcset", Txt, T] = identity(_) // todo
  given start[T]: Attribute["start", Int, T] = _.show
  given step[T]: Attribute["step", Double, T] = d => Txt(d.toString)
  given style[T]: Attribute["style", Txt, T] = identity(_)
  given tabindex[T]: Attribute["tabindex", Int, T] = _.show
  given target[T]: Attribute["target", Target, T] = _.text
  given title[T]: Attribute["title", Txt, T] = identity(_)
  given translate[T]: Attribute["translate", Boolean, T] = _ => Unset
  
  given htype[T]: Attribute["htype", HType, T] with
    override def rename: Option[Txt] = Some(str"type")
    def convert(value: HType): Txt = value.text
  
  given usemap[T]: Attribute["usemap", Txt, T] = identity(_) // todo
  given value[T]: Attribute["value", Double, T] = d => Txt(d.toString)
  given valueInt[T]: Attribute["value", Int, T] = _.show
  given width[T]: Attribute["width", Int, T] = _.show
  given wrap[T]: Attribute["wrap", Wrap, T] = _.text

enum HType:
  case Button, Checkbox, Color, Date, DatetimeLocal, Email, File, Hidden, Image, Month, Number,
      Password, Radio, Range, Reset, Search, Submit, Tel, Text, Time, Url, Week
  
  def text: Txt = this match
    case DatetimeLocal => str"datetime-local"
    case other         => Txt(other.toString).lower
