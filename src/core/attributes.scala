package honeycomb

trait Attribute[Key <: Label, -Value, -T]:
  def convert(value: Value): String | Boolean
  def rename: Option[String] = None

enum Rel:
  case Alternate, Author, Bookmark, Canonical, DnsPrefetch, External, Help, Icon, License, Manifest,
      Modulepreload, Next, Nofollow, Noopener, Noreferrer, Opener, Pingback, Preconnect, Prefetch, Preload,
      Prerender, Prev, Search, Stylesheet, Tag

enum Autocomplete:
  case On, Off

enum Method:
  case Post, Get, Dialog

enum Target:
  case Browse(value: String)
  case Self, Blank, Parent, Top

  override def toString(): String = this match
    case Browse(value) => value
    case Self          => "_self"
    case Blank         => "_blank"
    case Parent        => "_parent"
    case Top           => "_top"

extension (sc: StringContext)
  def cls(): Cls = Cls(sc.parts.head)
  def id(): DomId = DomId(sc.parts.head)

case class Cls(name: String)
case class DomId(name: String)

enum Crossorigin:
  case UseCredentials, Anonymous
  
  override def toString(): String = this match
    case Anonymous      => "anonymous"
    case UseCredentials => "use-credentials"

enum Dir:
  case Ltr, Rtl, Auto

  override def toString(): String = toString.toLowerCase

enum HttpEquiv:
  case ContentSecurityPolicy, ContentType, DefaultStyle, Refresh

  override def toString(): String = this match
    case ContentSecurityPolicy => "content-security-policy"
    case ContentType           => "content-type"
    case DefaultStyle          => "default-style"
    case Refresh               => "refresh"

enum Kind:
  case Subtitles, Captions, Descriptions, Chapters, Metadata

enum Preload:
  case None, Metadata, Auto

enum Sandbox:
  case AllowForms, AllowPointerLock, AllowPopups, AllowPresentation, AllowSameOrigin, AllowScripts,
      AllowTopNavigation
  
  override def toString(): String = this match
    case AllowForms         => "allow-forms"
    case AllowPointerLock   => "allow-pointer-lock"
    case AllowPopups        => "allow-popups"
    case AllowPresentation  => "allow-presentation"
    case AllowSameOrigin    => "allow-same-origin"
    case AllowScripts       => "allow-scripts"
    case AllowTopNavigation => "allow-top-navigation"

enum Scope:
  case Row, Col, Rowgroup, Colgroup

enum Shape:
  case Circle, Default, Poly, Rect

enum Wrap:
  case Soft, Hard

package attributes:
  package strings:
    given any[T, L <: Label]: Attribute[L, String, T] = identity(_)

  given accept[T]: Attribute["accept", List[String], T] = _.mkString(",")
  given accesskey[T]: Attribute["accesskey", Char, T] = _.toString
  given allowfullscreen[T]: Attribute["allowfullscreen", Boolean, T] = identity(_)
  given allowpaymentrequest[T]: Attribute["allowpaymentrequest", Boolean, T] = identity(_)
  given alt[T]: Attribute["alt", String, T] = identity(_)
  given async[T]: Attribute["async", Boolean, T] = identity(_)
  given autocomplete[T]: Attribute["autocomplete", Autocomplete, T] = _.toString.toLowerCase
  given autofocus[T]: Attribute["autofocus", Boolean, T] = identity(_)
  given autoplay[T]: Attribute["autoplay", Boolean, T] = identity(_)
  given border[T]: Attribute["border", Boolean, T] = if _ then "1" else ""
  given checked[T]: Attribute["checked", Boolean, T] = identity(_)
  given cite[T]: Attribute["cite", String, T] = identity(_)

  given cls[T]: Attribute["cls", List[Cls], T] with
    override def rename: Option[String] = Some("class")
    def convert(value: List[Cls]): String = value.map(_.name).mkString(" ")
 
  given code[T]: Attribute["code", String, T] = identity(_)
  given codebase[T]: Attribute["codebase", String, T] = identity(_)
  given cols[T]: Attribute["cols", Int, T] = _.toString
  given colspan[T]: Attribute["colspan", Int, T] = _.toString
  given content[T]: Attribute["content", String, T] = identity(_)
  given contenteditable[T]: Attribute["contenteditable", Boolean, T] = if _ then "true" else "false"
  given controls[T]: Attribute["controls", Boolean, T] = identity(_)
  given coords[T]: Attribute["coords", Seq[Double], T] = _.map(_.toString).mkString(",")
  given crossorigin[T]: Attribute["crossorigin", Crossorigin, T] = _.toString
  given data[T]: Attribute["data", String, T] = identity(_)
  given datetime[T]: Attribute["datetime", String, T] = identity(_)
  given default[T]: Attribute["default", Boolean, T] = identity(_)
  given defer[T]: Attribute["defer", Boolean, T] = identity(_)
  given dir[T]: Attribute["dir", Dir, T] = _.toString
  given dirname[T]: Attribute["dirname", String, T] = identity(_)
  given disabled[T]: Attribute["disabled", Boolean, T] = identity(_)
  given download[T]: Attribute["download", String, T] = identity(_)
  given draggable[T]: Attribute["draggable", Boolean, T] = if _ then "true" else "false"
  given enctype[T]: Attribute["enctype", String, T] = identity(_)
  
  given hfor[T]: Attribute["hfor", DomId, T] with
    override def rename: Option[String] = Some("for")
    def convert(value: DomId): String = value.name
  
  given hfors[T]: Attribute["hfor", Seq[DomId], T] with
    override def rename: Option[String] = Some("for")
    def convert(value: Seq[DomId]): String = value.map(_.name).mkString(" ")

  given form[T]: Attribute["form", DomId, T] = _.name
  given formaction[T]: Attribute["formaction", String, T] = identity(_)
  given formenctype[T]: Attribute["formenctype", String, T] = identity(_)
  given formmethod[T]: Attribute["formmethod", Method, T] = _.toString.toLowerCase
  given formnovalidate[T]: Attribute["formnovalidate", Boolean, T] = identity(_)
  given formtarget[T]: Attribute["formtarget", Target, T] = _.toString
  given headers[T]: Attribute["headers", DomId, T] = _.name
  given headers2[T]: Attribute["headers", Set[DomId], T] = _.map(_.name).mkString(" ")
  given height[T]: Attribute["height", Int, T] = _.toString
  given hidden[T]: Attribute["hidden", Boolean, T] = identity(_)
  given high[T]: Attribute["high", Double, T] = _.toString
  given href[T]: Attribute["href", String, T] = identity(_)
  given hreflang[T]: Attribute["hreflang", String, T] = identity(_)
  
  given httpEquiv[T]: Attribute["httpEquiv", HttpEquiv, T] with
    override def rename: Option[String] = Some("http-equiv")
    def convert(value: HttpEquiv): String = value.toString
  
  given id[T]: Attribute["id", DomId, T] = _.name
  given ismap[T]: Attribute["ismap", Boolean, T] = identity(_)
  given kind[T]: Attribute["kind", Kind, T] = _.toString.toLowerCase
  given label[T]: Attribute["label", String, T] = identity(_)
  given lang[T]: Attribute["lang", String, T] = identity(_)
  given list[T]: Attribute["list", Seq[DomId], T] = _.map(_.name).mkString(" ")
  given list2[T]: Attribute["list", DomId, T] = _.name
  given loop[T]: Attribute["loop", Boolean, T] = identity(_)
  given low[T]: Attribute["low", Double, T] = _.toString
  given manifest[T]: Attribute["manifest", String, T] = identity(_)
  given max[T]: Attribute["max", Double | Int, T] = _.toString
  given maxlength[T]: Attribute["maxlength", Int, T] = _.toString
  given minlength[T]: Attribute["minlength", Int, T] = _.toString
  given media[T]: Attribute["media", String, T] = identity(_)
  given method[T]: Attribute["method", Method, T] = _.toString.toLowerCase
  given min[T]: Attribute["min", Double | Int, T] = _.toString
  given multiple[T]: Attribute["multiple", Boolean, T] = identity(_)
  given muted[T]: Attribute["muted", Boolean, T] = identity(_)
  given name[T]: Attribute["name", String, T] = identity(_)
  given name2: Attribute["name", Target, "iframe" | "object"] = _.toString
  given nonce[T]: Attribute["nonce", String, T] = identity(_)
  given novalidate[T]: Attribute["novalidate", Boolean, T] = identity(_)
  given open[T]: Attribute["open", Boolean, T] = identity(_)
  given optimum[T]: Attribute["optimum", Double, T] = _.toString
  given pattern[T]: Attribute["pattern", String, T] = identity(_)
  given placeholder[T]: Attribute["placeholder", String, T] = identity(_)
  given poster[T]: Attribute["poster", String, T] = identity(_)
  given preload[T]: Attribute["preload", Preload, T] = _.toString.toLowerCase
  given readonly[T]: Attribute["readonly", Boolean, T] = identity(_)
  given referrerpolicy[T]: Attribute["referrerpolicy", String, T] = identity(_)
  given rel[T]: Attribute["rel", Rel, T] = _.toString.toLowerCase
  given rel2[T]: Attribute["rel", Seq[Rel], T] = _.map(_.toString.toLowerCase).mkString(" ")
  given required[T]: Attribute["required", Boolean, T] = identity(_)
  given rev[T]: Attribute["rev", String, T] = identity(_)
  given rows[T]: Attribute["rows", Int, T] = _.toString
  given rowspan[T]: Attribute["rowspan", Int, T] = _.toString
  given sandbox[T]: Attribute["sandbox", Sandbox, T] = _.toString
  given scope[T]: Attribute["scope", Scope, T] = _.toString.toLowerCase
  given selected[T]: Attribute["selected", Boolean, T] = identity(_)
  given shape[T]: Attribute["shape", Shape, T] = _.toString
  given size[T]: Attribute["size", Int, T] = _.toString
  given sizes[T]: Attribute["sizes", String, T] = identity(_) // todo
  given slot[T]: Attribute["slot", String, T] = identity(_)
  given span[T]: Attribute["span", Int, T] = _.toString
  given spellcheck[T]: Attribute["spellcheck", Boolean, T] = if _ then "true" else "false"
  given src[T]: Attribute["src", String, T] = identity(_)
  given srcdoc[T]: Attribute["srcdoc", String, T] = identity(_) // todo
  given srclang[T]: Attribute["srclang", String, T] = identity(_)
  given srcset[T]: Attribute["srcset", String, T] = identity(_) // todo
  given start[T]: Attribute["start", Int, T] = _.toString
  given step[T]: Attribute["step", Double, T] = _.toString
  given style[T]: Attribute["style", String, T] = identity(_)
  given tabindex[T]: Attribute["tabindex", Int, T] = _.toString
  given target[T]: Attribute["target", Target, T] = _.toString
  given title[T]: Attribute["title", String, T] = identity(_)
  given translate[T]: Attribute["translate", Boolean, T] = identity(_)
  
  given typeName[T]: Attribute["typeName", String, T] with
    override def rename: Option[String] = Some("type")
    def convert(value: String): String = value
  
  given usemap[T]: Attribute["usemap", String, T] = identity(_) // todo
  given value[T]: Attribute["value", Double, T] = _.toString
  given width[T]: Attribute["width", Int, T] = _.toString
  given wrap[T]: Attribute["wrap", Wrap, T] = _.toString.toLowerCase