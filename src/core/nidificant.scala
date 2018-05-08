package nidificant

import language.implicitConversions, language.dynamics, language.experimental.macros

final case class Tag[Children, -This, Attributes](tagName: String,
                                                  unclosed: Boolean = false,
                                                  inline: Boolean = false)
    extends Dynamic
    with Element[This] {
  type Ref

  def attributes = Map()
  def children = Nil

  def applyDynamic(methodName: String)(children: Node[Children]*): Element[This] =
    Element[This](tagName, Map(), children, inline)

  def attributes[A >: Attributes](
    attributes: Attribute[A]*
  ): AttributedTag[Children, This] =
    AttributedTag(tagName, attributes.map { a =>
      (a.key, a.value)
    }.toMap, unclosed)

  def applyDynamicNamed(
    methodName: String
  )(attributes: (String, Any)*): AttributedTag[Children, This] =
    macro Macros.rewriteAttributes
}

final case class AttributedTag[Children, -This](tagName: String,
                                                attributes: Map[String, String],
                                                unclosed: Boolean = false,
                                                inline: Boolean = false)
    extends Element[This] {
  def apply(children: Node[Children]*): Element[This] =
    Element[This](tagName, attributes, children, inline)

  def children: Seq[Node[_]] = Nil
}

final case class TransparentTag[Extras, Attributes](tagName: String,
                                                    unclosed: Boolean = false,
                                                    inline: Boolean = false)
    extends Dynamic {
  def children[Children](children: Node[Children with Extras]*): Element[Children] =
    Element[Children](tagName, Map(), children, inline)

  def applyDynamicNamed(
    methodName: String
  )(attributes: (String, Any)*): TransparentAttributedTag[Extras] =
    macro Macros.rewriteAttributes

  def attributes[A >: Attributes](
    attributes: Attribute[A]*
  ): TransparentAttributedTag[Extras] =
    TransparentAttributedTag(tagName, attributes.map { a =>
      (a.key, a.value)
    }.toMap, unclosed)

}

final case class TransparentAttributedTag[Extras](tagName: String,
                                                  attributes: Map[String, String],
                                                  unclosed: Boolean = false,
                                                  inline: Boolean = false) extends Element[Nothing] {
  def children: Seq[Node[_]] = Nil
  
  def apply[Children](children: Node[Children with Extras]*): Element[Children] =
    Element[Children](tagName, attributes, children, inline)

}

sealed trait Node[-TagType]

object Element {
  def apply[T](tagNameValue: String, attributesValue: Map[String, String], childrenValue: Seq[Node[_]], inlineValue: Boolean) =
    new Element[T] {
      def unclosed: Boolean = false
      def tagName: String = tagNameValue
      def attributes = attributesValue
      def children: Seq[Node[_]] = childrenValue
      def inline: Boolean = inlineValue
    }
}

trait Element[-TagType] extends Node[TagType] {
  def tagName: String
  def attributes: Map[String, String]
  def children: Seq[Node[_]]
  def unclosed: Boolean
  def inline: Boolean

  override def toString(): String = {
    val attributesString =
      if (attributes.isEmpty) ""
      else attributes.map { case (k, v) => s"""$k="$v"""" }.mkString(" ", " ", "")
    
    s"<$tagName$attributesString>" + children.mkString +
        (if (unclosed) "" else s"</$tagName>")
  }

}

final case class Elements[-TagType](elements: List[Element[TagType]])
    extends Node[TagType] {
  override def toString(): String = elements.mkString
}

object ShowAttribute {
  implicit val showString: ShowAttribute[String] = identity
  implicit val showInt: ShowAttribute[Int] = _.toString
  implicit val showBoolean: ShowAttribute[Boolean] = if (_) "1" else "0"
}

trait ShowAttribute[T] { def show(t: T): String }

trait AttributeFactory[T] {
  type Type
  def attributeKey: String
  def update(value: T)(implicit show: ShowAttribute[T]): Attribute[Type] =
    Attribute(attributeKey, show.show(value))
}
final case class Attribute[-AttType](key: String, value: String) {
  override def toString = s"""$key="$value""""
}

final case class AttributeKey[T](key: String) extends AttributeFactory[T] {
  type Type = this.type
  def attributeKey = key
}

final case class Text(str: String) extends Node[Html5#PlainText] {
  override def toString(): String = str
}

final case class InputType(id: String)

object html5 extends Html5 {
  type Javascript = String
  type OnOff = String
}

trait Html5 {

  type OnOff
  type Javascript

  type Global =
    accesskeyAttribute.type with classAttribute.type with contenteditableAttribute.type with contextmenuAttribute.type with dirAttribute.type with draggableAttribute.type with dropzoneAttribute.type with hiddenAttribute.type with idAttribute.type with langAttribute.type with spellcheckAttribute.type with styleAttribute.type with tabindexAttribute.type with titleAttribute.type

  type EventHandlers =
    onabortAttribute.type with onblurAttribute.type with oncanplayAttribute.type with oncanplaythroughAttribute.type with onchangeAttribute.type with onclickAttribute.type with oncontextmenuAttribute.type with oncuechangeAttribute.type with ondblclickAttribute.type with ondragAttribute.type with ondragendAttribute.type with ondragenterAttribute.type with ondragleaveAttribute.type with ondragoverAttribute.type with ondragstartAttribute.type with ondropAttribute.type with ondurationchangeAttribute.type with onemptiedAttribute.type with onendedAttribute.type with onerrorAttribute.type with onfocusAttribute.type with oninputAttribute.type with oninvalidAttribute.type with onkeydownAttribute.type with onkeypressAttribute.type with onkeyupAttribute.type with onloadAttribute.type with onloadeddataAttribute.type with onloadedmetadataAttribute.type with onloadstartAttribute.type with onmousedownAttribute.type with onmousemoveAttribute.type with onmouseoutAttribute.type with onmouseoverAttribute.type with onmouseupAttribute.type with onmousewheelAttribute.type with onpauseAttribute.type with onplayAttribute.type with onplayingAttribute.type with onprogressAttribute.type with onratechangeAttribute.type with onreadystatechangeAttribute.type with onresetAttribute.type with onscrollAttribute.type with onseekedAttribute.type with onseekingAttribute.type with onselectAttribute.type with onshowAttribute.type with onstalledAttribute.type with onsubmitAttribute.type with onsuspendAttribute.type with ontimeupdateAttribute.type with onvolumechangeAttribute.type with onwaitingAttribute.type

  val onabortAttribute = AttributeKey[Javascript]("onabort")
  val onblurAttribute = AttributeKey[Javascript]("onblur")
  val oncanplayAttribute = AttributeKey[Javascript]("oncanplay")
  val oncanplaythroughAttribute = AttributeKey[Javascript]("oncanplaythrough")
  val onchangeAttribute = AttributeKey[Javascript]("onchange")
  val onclickAttribute = AttributeKey[Javascript]("onclick")
  val oncontextmenuAttribute = AttributeKey[Javascript]("oncontextmenu")
  val oncuechangeAttribute = AttributeKey[Javascript]("oncuechange")
  val ondblclickAttribute = AttributeKey[Javascript]("ondblclick")
  val ondragAttribute = AttributeKey[Javascript]("ondrag")
  val ondragendAttribute = AttributeKey[Javascript]("ondragend")
  val ondragenterAttribute = AttributeKey[Javascript]("ondragenter")
  val ondragleaveAttribute = AttributeKey[Javascript]("ondragleave")
  val ondragoverAttribute = AttributeKey[Javascript]("ondragover")
  val ondragstartAttribute = AttributeKey[Javascript]("ondragstart")
  val ondropAttribute = AttributeKey[Javascript]("ondrop")
  val ondurationchangeAttribute = AttributeKey[Javascript]("ondurationchange")
  val onemptiedAttribute = AttributeKey[Javascript]("onemptied")
  val onendedAttribute = AttributeKey[Javascript]("onended")
  val onerrorAttribute = AttributeKey[Javascript]("onerror")
  val onfocusAttribute = AttributeKey[Javascript]("onfocus")
  val oninputAttribute = AttributeKey[Javascript]("oninput")
  val oninvalidAttribute = AttributeKey[Javascript]("oninvalid")
  val onkeydownAttribute = AttributeKey[Javascript]("onkeydown")
  val onkeypressAttribute = AttributeKey[Javascript]("onkeypress")
  val onkeyupAttribute = AttributeKey[Javascript]("onkeyup")
  val onloadAttribute = AttributeKey[Javascript]("onload")
  val onloadeddataAttribute = AttributeKey[Javascript]("onloadeddata")
  val onloadedmetadataAttribute = AttributeKey[Javascript]("onloadedmetadata")
  val onloadstartAttribute = AttributeKey[Javascript]("onloadstart")
  val onmousedownAttribute = AttributeKey[Javascript]("onmousedown")
  val onmousemoveAttribute = AttributeKey[Javascript]("onmousemove")
  val onmouseoutAttribute = AttributeKey[Javascript]("onmouseout")
  val onmouseoverAttribute = AttributeKey[Javascript]("onmouseover")
  val onmouseupAttribute = AttributeKey[Javascript]("onmouseup")
  val onmousewheelAttribute = AttributeKey[Javascript]("onmousewheel")
  val onpauseAttribute = AttributeKey[Javascript]("onpause")
  val onplayAttribute = AttributeKey[Javascript]("onplay")
  val onplayingAttribute = AttributeKey[Javascript]("onplaying")
  val onprogressAttribute = AttributeKey[Javascript]("onprogress")
  val onratechangeAttribute = AttributeKey[Javascript]("onratechange")
  val onreadystatechangeAttribute = AttributeKey[Javascript]("onreadystatechange")
  val onresetAttribute = AttributeKey[Javascript]("onreset")
  val onscrollAttribute = AttributeKey[Javascript]("onscroll")
  val onseekedAttribute = AttributeKey[Javascript]("onseeked")
  val onseekingAttribute = AttributeKey[Javascript]("onseeking")
  val onselectAttribute = AttributeKey[Javascript]("onselect")
  val onshowAttribute = AttributeKey[Javascript]("onshow")
  val onstalledAttribute = AttributeKey[Javascript]("onstalled")
  val onsubmitAttribute = AttributeKey[Javascript]("onsubmit")
  val onsuspendAttribute = AttributeKey[Javascript]("onsuspend")
  val ontimeupdateAttribute = AttributeKey[Javascript]("ontimeupdate")
  val onvolumechangeAttribute = AttributeKey[Javascript]("onvolumechange")
  val onwaitingAttribute = AttributeKey[Javascript]("onwaiting")

  val acceptAttribute = AttributeKey[String]("accept")
  val acceptCharsetAttribute = AttributeKey[String]("accept-charset")
  val accesskeyAttribute = AttributeKey[String]("accesskey")
  val actionAttribute = AttributeKey[String]("action")
  val alignAttribute = AttributeKey[String]("align")
  val altAttribute = AttributeKey[String]("alt")
  val asyncAttribute = AttributeKey[String]("async")
  val autocapitalizeAttribute = AttributeKey[String]("autocapitalize")
  val autocompleteAttribute = AttributeKey[String]("autocomplete")
  val autofocusAttribute = AttributeKey[String]("autofocus")
  val autoplayAttribute = AttributeKey[String]("autoplay")
  val bufferedAttribute = AttributeKey[String]("buffered")
  val challengeAttribute = AttributeKey[String]("challenge")
  val charsetAttribute = AttributeKey[String]("charset")
  val checkedAttribute = AttributeKey[String]("checked")
  val citeAttribute = AttributeKey[String]("cite")
  val classAttribute = AttributeKey[String]("class")
  val codeAttribute = AttributeKey[String]("code")
  val codebaseAttribute = AttributeKey[String]("codebase")
  val colsAttribute = AttributeKey[String]("cols")
  val colspanAttribute = AttributeKey[String]("colspan")
  val contentAttribute = AttributeKey[String]("content")
  val contenteditableAttribute = AttributeKey[String]("contenteditable")
  val contextmenuAttribute = AttributeKey[String]("contextmenu")
  val controlsAttribute = AttributeKey[String]("controls")
  val coordsAttribute = AttributeKey[String]("coords")
  val crossoriginAttribute = AttributeKey[String]("crossorigin")
  val dataAttribute = AttributeKey[String]("data")
  val datetimeAttribute = AttributeKey[String]("datetime")
  val defaultAttribute = AttributeKey[String]("default")
  val deferAttribute = AttributeKey[String]("defer")
  val dirAttribute = AttributeKey[String]("dir")
  val dirnameAttribute = AttributeKey[String]("dirname")
  val disabledAttribute = AttributeKey[String]("disabled")
  val downloadAttribute = AttributeKey[String]("download")
  val draggableAttribute = AttributeKey[String]("draggable")
  val dropzoneAttribute = AttributeKey[String]("dropzone")
  val enctypeAttribute = AttributeKey[String]("enctype")
  val forAttribute = AttributeKey[String]("for")
  val formAttribute = AttributeKey[String]("form")
  val formactionAttribute = AttributeKey[String]("formaction")
  val headersAttribute = AttributeKey[String]("headers")
  val heightAttribute = AttributeKey[String]("height")
  val hiddenAttribute = AttributeKey[String]("hidden")
  val highAttribute = AttributeKey[String]("high")
  val hrefAttribute = AttributeKey[String]("href")
  val hreflangAttribute = AttributeKey[String]("hreflang")
  val httpEquivAttribute = AttributeKey[String]("http-equiv")
  val iconAttribute = AttributeKey[String]("icon")
  val idAttribute = AttributeKey[String]("id")
  val integrityAttribute = AttributeKey[String]("integrity")
  val ismapAttribute = AttributeKey[String]("ismap")
  val itempropAttribute = AttributeKey[String]("itemprop")
  val keytypeAttribute = AttributeKey[String]("keytype")
  val kindAttribute = AttributeKey[String]("kind")
  val labelAttribute = AttributeKey[String]("label")
  val langAttribute = AttributeKey[String]("lang")
  val languageAttribute = AttributeKey[String]("language")
  val listAttribute = AttributeKey[String]("list")
  val loopAttribute = AttributeKey[String]("loop")
  val lowAttribute = AttributeKey[String]("low")
  val manifestAttribute = AttributeKey[String]("manifest")
  val maxAttribute = AttributeKey[String]("max")
  val maxlengthAttribute = AttributeKey[String]("maxlength")
  val minlengthAttribute = AttributeKey[String]("minlength")
  val mediaAttribute = AttributeKey[String]("media")
  val methodAttribute = AttributeKey[String]("method")
  val minAttribute = AttributeKey[String]("min")
  val multipleAttribute = AttributeKey[String]("multiple")
  val mutedAttribute = AttributeKey[String]("muted")
  val nameAttribute = AttributeKey[String]("name")
  val novalidateAttribute = AttributeKey[String]("novalidate")
  val openAttribute = AttributeKey[String]("open")
  val optimumAttribute = AttributeKey[String]("optimum")
  val patternAttribute = AttributeKey[String]("pattern")
  val pingAttribute = AttributeKey[String]("ping")
  val placeholderAttribute = AttributeKey[String]("placeholder")
  val posterAttribute = AttributeKey[String]("poster")
  val preloadAttribute = AttributeKey[String]("preload")
  val radiogroupAttribute = AttributeKey[String]("radiogroup")
  val readonlyAttribute = AttributeKey[String]("readonly")
  val relAttribute = AttributeKey[String]("rel")
  val requiredAttribute = AttributeKey[String]("required")
  val reversedAttribute = AttributeKey[String]("reversed")
  val rowsAttribute = AttributeKey[String]("rows")
  val rowspanAttribute = AttributeKey[String]("rowspan")
  val sandboxAttribute = AttributeKey[String]("sandbox")
  val scopeAttribute = AttributeKey[String]("scope")
  val scopedAttribute = AttributeKey[String]("scoped")
  val seamlessAttribute = AttributeKey[String]("seamless")
  val selectedAttribute = AttributeKey[String]("selected")
  val shapeAttribute = AttributeKey[String]("shape")
  val sizeAttribute = AttributeKey[String]("size")
  val sizesAttribute = AttributeKey[String]("sizes")
  val slotAttribute = AttributeKey[String]("slot")
  val spanAttribute = AttributeKey[String]("span")
  val spellcheckAttribute = AttributeKey[String]("spellcheck")
  val srcAttribute = AttributeKey[String]("src")
  val srcdocAttribute = AttributeKey[String]("srcdoc")
  val srclangAttribute = AttributeKey[String]("srclang")
  val srcsetAttribute = AttributeKey[String]("srcset")
  val startAttribute = AttributeKey[String]("start")
  val stepAttribute = AttributeKey[String]("step")
  val styleAttribute = AttributeKey[String]("style")
  val summaryAttribute = AttributeKey[String]("summary")
  val tabindexAttribute = AttributeKey[String]("tabindex")
  val targetAttribute = AttributeKey[String]("target")
  val titleAttribute = AttributeKey[String]("title")
  val translateAttribute = AttributeKey[String]("translate")
  val typeAttribute = AttributeKey[String]("type")
  val usemapAttribute = AttributeKey[String]("usemap")
  val valueAttribute = AttributeKey[String]("value")
  val widthAttribute = AttributeKey[String]("width")
  val wrapAttribute = AttributeKey[OnOff]("wrap")

  trait Flow extends Heading with Sectioning with Phrasing
  trait HtmlItems
  trait Phrasing extends Embedded with PlainText
  trait PlainText
  trait Sectioning
  trait Heading
  trait Embedded
  trait ListType
  trait Ruby
  trait HeadItems
  trait FigureContent
  trait LegendContent
  trait TemplateContent
  trait ImgContent
  trait OptionContent
  trait OptGroupContent
  trait SourceContent
  trait SummaryContent
  trait Scripting
  trait StyleContent
  trait ScriptContent
  trait MetaContent
  trait LinkContent
  trait ParamContent
  trait TrackContent
  trait TrContent
  trait TheadContent
  trait TbodyContent
  trait ColgroupContent
  trait ColContent
  trait CaptionContent
  trait TdContent
  trait TfootContent
  trait DtContent
  trait DdContent
  trait Empty
  trait Root

  val html = Tag[HtmlItems, Root, Global with manifestAttribute.type]("html")

  val head = Tag[
    HeadItems with ScriptContent with MetaContent with LinkContent with StyleContent with TemplateContent,
    HtmlItems,
    Global
  ]("head")

  val title = Tag[PlainText, HeadItems, Global]("title")

  val base =
    Tag[Text, HeadItems, Global with hrefAttribute.type with targetAttribute.type]("base")

  val link = Tag[
    Empty,
    LinkContent,
    Global with crossoriginAttribute.type with hrefAttribute.type with hreflangAttribute.type with integrityAttribute.type with mediaAttribute.type with relAttribute.type with sizesAttribute.type
  ]("link", unclosed = true)

  val meta = Tag[
    Empty,
    MetaContent,
    Global with charsetAttribute.type with contentAttribute.type with httpEquivAttribute.type with nameAttribute.type
  ]("meta", unclosed = true)

  val style = Tag[
    Text,
    StyleContent,
    Global with mediaAttribute.type with scopedAttribute.type with typeAttribute.type
  ]("style")

  val body = Tag[Flow with TemplateContent, HtmlItems, Global]("body")

  val article = Tag[Flow, Flow, Global]("article")

  val section = Tag[Flow, Flow, Global]("section") // FIXME: Not a descendant of an Address

  val nav = Tag[Flow, Flow, Global]("nav")

  val aside = Tag[Flow, Flow, Global]("aside") // FIXME: Not a descendant of an address

  val h1 = Tag[Phrasing, Flow, Global]("h1")

  val h2 = Tag[Phrasing, Flow, Global]("h2")

  val h3 = Tag[Phrasing, Flow, Global]("h3")

  val h4 = Tag[Phrasing, Flow, Global]("h4")

  val h5 = Tag[Phrasing, Flow, Global]("h5")

  val h6 = Tag[Phrasing, Flow, Global]("h6")

  val header = Tag[Flow, Flow, Global]("header") // FIXME: Not inside another header, footer, or address

  val footer = Tag[Flow, Flow, Global]("footer") // FIXME: Not inside another header, footer, or address

  val p = Tag[Phrasing, Flow, Global]("p")

  val address = Tag[Flow, Flow, Global]("address")

  val hr = Tag[Empty, Flow, Global with alignAttribute.type]("hr", unclosed = true)

  val pre = Tag[Phrasing, Flow, Global]("pre")

  val blockquote = Tag[Flow, Flow, Global with citeAttribute.type]("blockquote")

  val ol =
    Tag[ListType, Flow, Global with reversedAttribute.type with startAttribute.type]("ol")

  val ul = Tag[ListType, Flow, Global]("ul")

  val li = Tag[Flow, ListType, Global with valueAttribute.type]("li")

  val dl = Tag[DtContent with DdContent, Flow, Global]("dl") // FIXME with TemplateContent

  val dt = Tag[Flow, DtContent, Global]("dt")

  val dd = Tag[Flow, DdContent, Global]("dd")

  val figure = Tag[FigureContent with Flow, Flow, Global]("figure")

  val figcaption = Tag[Flow, FigureContent, Global]("figcaption")

  val main = Tag[Flow, Flow, Global]("main")

  val div = Tag[Flow, Flow, Global]("div")

  val a = TransparentTag[
    Empty,
    Global with downloadAttribute.type with hrefAttribute.type with hreflangAttribute.type with mediaAttribute.type with pingAttribute.type with relAttribute.type with shapeAttribute.type with targetAttribute.type
  ]("a")

  val em = Tag[Phrasing, Phrasing, Global]("em")

  val strong = Tag[Phrasing, Phrasing, Global]("strong")

  val small = Tag[Phrasing, Phrasing, Global]("small")

  val s = Tag[Phrasing, Phrasing, Global]("s")

  val cite = Tag[Phrasing, Phrasing, Global]("cite")

  val q = Tag[Phrasing, Phrasing, Global]("q")

  val dfn = Tag[Phrasing, Phrasing, Global]("dfn") // FIXME: No dfn may be a descendant

  val abbr = Tag[Phrasing, Phrasing, Global]("abbr")

  val ruby = Tag[Ruby, Phrasing, Global]("ruby")

  val rb = Tag[Phrasing, Ruby, Global]("rb")

  val rt = Tag[Phrasing, Ruby, Global]("rt")

  val rtc = Tag[Phrasing with Ruby, Ruby, Global]("rtc")

  val rp = Tag[Text, Ruby, Global]("rp")

  val data = Tag[Phrasing, Phrasing, Global]("data")

  val time = Tag[Phrasing, Phrasing, Global]("time")

  val code = Tag[Phrasing, Phrasing, Global]("code")

  val `var` = Tag[Phrasing, Phrasing, Global]("var")

  val varTag = `var`

  val samp = Tag[Phrasing, Phrasing, Global]("samp")

  val kbd = Tag[Phrasing, Phrasing, Global]("kbd")

  val sub = Tag[Phrasing, Phrasing, Global]("sub")

  val sup = Tag[Phrasing, Phrasing, Global]("sup")

  val i = Tag[Phrasing, Phrasing, Global]("i")

  val b = Tag[Phrasing, Phrasing, Global]("b")

  val u = Tag[Phrasing, Phrasing, Global]("u")

  val mark = Tag[Phrasing, Phrasing, Global]("mark")

  val bdi = Tag[Phrasing, Phrasing, Global]("bdi")

  val bdo = Tag[Phrasing, Phrasing, Global]("bdo")

  val span = Tag[Phrasing, Phrasing, Global]("span")

  val br = Tag[Empty, Phrasing, Global]("br", unclosed = true)

  val wbr = Tag[Empty, Phrasing, Global]("wbr")

  val ins =
    TransparentTag[Empty, Global with citeAttribute.type with datetimeAttribute.type](
      "ins"
    )

  val del =
    TransparentTag[Empty, Global with citeAttribute.type with datetimeAttribute.type](
      "del"
    )

  val picture = Tag[SourceContent with ImgContent, Flow, Global]("picture")

  val source = Tag[
    Empty,
    SourceContent,
    Global with mediaAttribute.type with sizesAttribute.type with srcAttribute.type with typeAttribute.type
  ]("source")

  val img = Tag[
    Empty,
    Embedded,
    Global with alignAttribute.type with altAttribute.type with crossoriginAttribute.type with heightAttribute.type with ismapAttribute.type with sizesAttribute.type with srcAttribute.type with srcsetAttribute.type with usemapAttribute.type with widthAttribute.type
  ]("img", unclosed = true)

  val iframe = TransparentTag[
    Empty,
    Global with alignAttribute.type with heightAttribute.type with nameAttribute.type with sandboxAttribute.type with seamlessAttribute.type with srcAttribute.type with srcdocAttribute.type with widthAttribute.type
  ]("iframe")

  val embed = Tag[
    Empty,
    Embedded,
    Global with heightAttribute.type with typeAttribute.type with widthAttribute.type
  ]("embed")

  val `object` = TransparentTag[
    ParamContent,
    Global with dataAttribute.type with formAttribute.type with nameAttribute.type with typeAttribute.type with usemapAttribute.type with widthAttribute.type
  ]("object")

  val objectTag = `object`

  val param =
    Tag[Empty, ParamContent, Global with nameAttribute.type with valueAttribute.type](
      "param"
    )

  val video = TransparentTag[
    TrackContent with SourceContent,
    Global with autoplayAttribute.type with bufferedAttribute.type with controlsAttribute.type with crossoriginAttribute.type with heightAttribute.type with loopAttribute.type with mutedAttribute.type with posterAttribute.type with preloadAttribute.type with srcAttribute.type with widthAttribute.type
  ]("video")

  val audio = TransparentTag[
    TrackContent with SourceContent,
    Global with autoplayAttribute.type with bufferedAttribute.type with controlsAttribute.type with crossoriginAttribute.type with loopAttribute.type with mutedAttribute.type with preloadAttribute.type with srcAttribute.type
  ]("audio")

  val track = Tag[
    Empty,
    TrackContent,
    Global with defaultAttribute.type with kindAttribute.type with labelAttribute.type with srcAttribute.type with srclangAttribute.type
  ]("track")

  val map = TransparentTag[Empty, Global with nameAttribute.type]("map")

  val area = Tag[
    Empty,
    Phrasing,
    Global with altAttribute.type with coordsAttribute.type with downloadAttribute.type with hrefAttribute.type with hreflangAttribute.type with mediaAttribute.type with pingAttribute.type with relAttribute.type with shapeAttribute.type with targetAttribute.type
  ]("area", unclosed = true)

  val table = Tag[
    CaptionContent with ColgroupContent with TheadContent with TrContent with TbodyContent with TfootContent,
    Flow,
    Global with alignAttribute.type with summaryAttribute.type
  ]("table")

  val caption = Tag[Flow, CaptionContent, Global with alignAttribute.type]("caption")

  val colgroup = Tag[ColContent,
                     ColgroupContent,
                     Global with alignAttribute.type with spanAttribute.type]("colgroup")
  val col =
    Tag[Empty, ColContent, Global with alignAttribute.type with spanAttribute.type](
      "col",
      unclosed = true
    )

  val tbody = Tag[TrContent, TbodyContent, Global with alignAttribute.type]("tbody")

  val thead = Tag[TrContent, TheadContent, Global with alignAttribute.type]("thead")

  val tfoot = Tag[TrContent, TbodyContent, Global with alignAttribute.type]("tfoot")

  val tr = Tag[TdContent, TrContent, Global with alignAttribute.type]("tr")

  val td = Tag[
    Flow,
    TdContent,
    Global with alignAttribute.type with colspanAttribute.type with headersAttribute.type with rowspanAttribute.type
  ]("td")

  val th = Tag[
    Flow,
    TdContent,
    Global with alignAttribute.type with colspanAttribute.type with headersAttribute.type with rowspanAttribute.type with scopeAttribute.type
  ]("th")

  val form =
    Tag[
      Flow,
      Flow,
      Global with acceptAttribute.type with acceptCharsetAttribute.type with actionAttribute.type with autocompleteAttribute.type with enctypeAttribute.type with methodAttribute.type with nameAttribute.type with novalidateAttribute.type with targetAttribute.type
    ](
      "form"
    )

  val label =
    Tag[Phrasing, Phrasing, Global with forAttribute.type with formAttribute.type](
      "label"
    )

  val input = Tag[
    Empty,
    Phrasing,
    Global with acceptAttribute.type with altAttribute.type with autocompleteAttribute.type with autofocusAttribute.type with checkedAttribute.type with dirnameAttribute.type with disabledAttribute.type with formAttribute.type with formactionAttribute.type with heightAttribute.type with listAttribute.type with maxAttribute.type with maxlengthAttribute.type with minAttribute.type with multipleAttribute.type with nameAttribute.type with patternAttribute.type with placeholderAttribute.type with readonlyAttribute.type with requiredAttribute.type with sizeAttribute.type with srcAttribute.type with stepAttribute.type with typeAttribute.type with usemapAttribute.type with widthAttribute.type
  ]("input", unclosed = true)

  val button = Tag[
    Phrasing,
    Phrasing,
    Global with autofocusAttribute.type with disabledAttribute.type with formAttribute.type with formactionAttribute.type with nameAttribute.type with typeAttribute.type with valueAttribute.type
  ]("button")

  val select = Tag[
    OptionContent with OptGroupContent,
    Flow,
    Global with autofocusAttribute.type with disabledAttribute.type with formAttribute.type with multipleAttribute.type with nameAttribute.type with requiredAttribute.type with sizeAttribute.type
  ]("select")

  val datalist = Tag[OptionContent with Phrasing, Phrasing, Global]("datalist")

  val optgroup =
    Tag[OptionContent, OptGroupContent, Global with disabledAttribute.type]("optgroup")

  val option = Tag[
    Text,
    OptionContent,
    Global with disabledAttribute.type with selectedAttribute.type with valueAttribute.type
  ]("option")

  val textarea = Tag[
    Text,
    Flow,
    Global with autocompleteAttribute.type with autofocusAttribute.type with colsAttribute.type with dirnameAttribute.type with disabledAttribute.type with formAttribute.type with maxlengthAttribute.type with nameAttribute.type with placeholderAttribute.type with readonlyAttribute.type with requiredAttribute.type with rowsAttribute.type with wrapAttribute.type
  ]("textarea")

  val output =
    Tag[Phrasing,
        Phrasing,
        Global with forAttribute.type with heightAttribute.type with nameAttribute.type](
      "output"
    )

  val progress =
    Tag[Phrasing,
        Phrasing,
        Global with formAttribute.type with maxAttribute.type with valueAttribute.type](
      "progress"
    )

  val meter = Tag[
    Phrasing,
    Phrasing,
    Global with formAttribute.type with highAttribute.type with lowAttribute.type with maxAttribute.type with minAttribute.type with optimumAttribute.type with valueAttribute.type
  ]("meter")

  val fieldset = Tag[
    LegendContent with Flow,
    Flow,
    Global with disabledAttribute.type with formAttribute.type with nameAttribute.type
  ]("fieldset")

  val legend = Tag[Phrasing, LegendContent, Global]("legend")

  val details =
    Tag[Flow with SummaryContent, Flow, Global with openAttribute.type]("details")

  val menu = Tag[Flow with ListType with Scripting with TemplateContent,
                 Flow,
                 Global with typeAttribute.type]("menu")

  val summary = Tag[Phrasing with Heading, SummaryContent, Global]("summary")

  val dialog = Tag[Flow, Flow, Global]("dialog")

  val script = Tag[
    PlainText,
    ScriptContent,
    Global with asyncAttribute.type with charsetAttribute.type with crossoriginAttribute.type with deferAttribute.type with integrityAttribute.type with languageAttribute.type with srcAttribute.type with typeAttribute.type
  ]("script")

  val noscript =
    TransparentTag[LinkContent with StyleContent with MetaContent, Global]("noscript")

  val template = Tag[Empty, TemplateContent, Global]("template")

  val canvas =
    TransparentTag[Empty, Global with heightAttribute.type with widthAttribute.type](
      "canvas"
    )

  implicit def stringToText(str: String): Node[Html5#PlainText] = Text(str)
  implicit def autoIterable[T](elements: Iterable[Element[T]]): Node[T] =
    Elements(elements.to[List])
  implicit def autoEmptyAttributedTag[T](tag: AttributedTag[_, T]): Node[T] = tag()
  implicit def autoEmptyTransparentAttributedTag[T, E](
    tag: TransparentAttributedTag[E]
  ): Node[T] = tag()

  object inputTypes {
    val button = InputType("button")
    val checkbox = InputType("checkbox")
    val color = InputType("color")
    val date = InputType("date")
    val datetime = InputType("datetime")
    val datetimeLocal = InputType("datetime-local")
    val email = InputType("email")
    val file = InputType("file")
    val hidden = InputType("hidden")
    val image = InputType("image")
    val month = InputType("month")
    val number = InputType("number")
    val password = InputType("password")
    val radio = InputType("radio")
    val range = InputType("range")
    val reset = InputType("reset")
    val search = InputType("search")
    val submit = InputType("submit")
    val tel = InputType("tel")
    val text = InputType("text")
    val time = InputType("time")
    val url = InputType("url")
    val week = InputType("week")
  }

}
