package progeny

import language.implicitConversions, language.dynamics, language.experimental.macros

case class Tag[C, T, A](tagName: String, unclosed: Boolean = false) extends Dynamic {
  type Children = C
  type This = T
  type Attributes = A

  def applyDynamic(tag: String)(children: Node[_ >: Children]*): Element[This] =
    Element[This](tagName, Map(), children)

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

case class AttributedTag[Children, This](tagName: String,
                                         attributes: Map[String, String],
                                         unclosed: Boolean = false) {
  def apply(children: Node[_ >: Children]*): Element[This] =
    Element[This](tagName, attributes, children)
}

case class TransparentTag(val tagName: String) {
  def apply[Children](children: Node[Children]*): Element[Children] =
    Element[Children](tagName, Map(), children)
}
sealed trait Node[TagType]

case class Element[TagType](tagName: String,
                            attributes: Map[String, String],
                            children: Seq[Node[_]])
    extends Node[TagType] {

  def attributesString =
    if (attributes.isEmpty) ""
    else attributes.map { case (k, v) => s"""$k="$v"""" }.mkString(" ", " ", "")

  override def toString(): String =
    s"<$tagName$attributesString>${children.mkString}</$tagName>"

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
case class Attribute[AttType](key: String, value: String)

case class Att[T](key: String) extends AttributeFactory[T] {
  type Type = this.type
  def attributeKey = key
}

object html5 extends Html5 {
  type Javascript = String
  type TrueFalse = Boolean
}

trait Html5 {

  type TrueFalse
  type Javascript

  type Global =
    accesskeyAttribute.type with classAttribute.type with contenteditableAttribute.type with contextmenuAttribute.type with dirAttribute.type with draggableAttribute.type with dropzoneAttribute.type with hiddenAttribute.type with idAttribute.type with langAttribute.type with spellcheckAttribute.type with styleAttribute.type with tabindexAttribute.type with titleAttribute.type

  type EventHandlers =
    onabortAttribute.type with onblurAttribute.type with oncanplayAttribute.type with oncanplaythroughAttribute.type with onchangeAttribute.type with onclickAttribute.type with oncontextmenuAttribute.type with oncuechangeAttribute.type with ondblclickAttribute.type with ondragAttribute.type with ondragendAttribute.type with ondragenterAttribute.type with ondragleaveAttribute.type with ondragoverAttribute.type with ondragstartAttribute.type with ondropAttribute.type with ondurationchangeAttribute.type with onemptiedAttribute.type with onendedAttribute.type with onerrorAttribute.type with onfocusAttribute.type with oninputAttribute.type with oninvalidAttribute.type with onkeydownAttribute.type with onkeypressAttribute.type with onkeyupAttribute.type with onloadAttribute.type with onloadeddataAttribute.type with onloadedmetadataAttribute.type with onloadstartAttribute.type with onmousedownAttribute.type with onmousemoveAttribute.type with onmouseoutAttribute.type with onmouseoverAttribute.type with onmouseupAttribute.type with onmousewheelAttribute.type with onpauseAttribute.type with onplayAttribute.type with onplayingAttribute.type with onprogressAttribute.type with onratechangeAttribute.type with onreadystatechangeAttribute.type with onresetAttribute.type with onscrollAttribute.type with onseekedAttribute.type with onseekingAttribute.type with onselectAttribute.type with onshowAttribute.type with onstalledAttribute.type with onsubmitAttribute.type with onsuspendAttribute.type with ontimeupdateAttribute.type with onvolumechangeAttribute.type with onwaitingAttribute.type

  val onabortAttribute = Att[Javascript]("onabort")
  val onblurAttribute = Att[Javascript]("onblur")
  val oncanplayAttribute = Att[Javascript]("oncanplay")
  val oncanplaythroughAttribute = Att[Javascript]("oncanplaythrough")
  val onchangeAttribute = Att[Javascript]("onchange")
  val onclickAttribute = Att[Javascript]("onclick")
  val oncontextmenuAttribute = Att[Javascript]("oncontextmenu")
  val oncuechangeAttribute = Att[Javascript]("oncuechange")
  val ondblclickAttribute = Att[Javascript]("ondblclick")
  val ondragAttribute = Att[Javascript]("ondrag")
  val ondragendAttribute = Att[Javascript]("ondragend")
  val ondragenterAttribute = Att[Javascript]("ondragenter")
  val ondragleaveAttribute = Att[Javascript]("ondragleave")
  val ondragoverAttribute = Att[Javascript]("ondragover")
  val ondragstartAttribute = Att[Javascript]("ondragstart")
  val ondropAttribute = Att[Javascript]("ondrop")
  val ondurationchangeAttribute = Att[Javascript]("ondurationchange")
  val onemptiedAttribute = Att[Javascript]("onemptied")
  val onendedAttribute = Att[Javascript]("onended")
  val onerrorAttribute = Att[Javascript]("onerror")
  val onfocusAttribute = Att[Javascript]("onfocus")
  val oninputAttribute = Att[Javascript]("oninput")
  val oninvalidAttribute = Att[Javascript]("oninvalid")
  val onkeydownAttribute = Att[Javascript]("onkeydown")
  val onkeypressAttribute = Att[Javascript]("onkeypress")
  val onkeyupAttribute = Att[Javascript]("onkeyup")
  val onloadAttribute = Att[Javascript]("onload")
  val onloadeddataAttribute = Att[Javascript]("onloadeddata")
  val onloadedmetadataAttribute = Att[Javascript]("onloadedmetadata")
  val onloadstartAttribute = Att[Javascript]("onloadstart")
  val onmousedownAttribute = Att[Javascript]("onmousedown")
  val onmousemoveAttribute = Att[Javascript]("onmousemove")
  val onmouseoutAttribute = Att[Javascript]("onmouseout")
  val onmouseoverAttribute = Att[Javascript]("onmouseover")
  val onmouseupAttribute = Att[Javascript]("onmouseup")
  val onmousewheelAttribute = Att[Javascript]("onmousewheel")
  val onpauseAttribute = Att[Javascript]("onpause")
  val onplayAttribute = Att[Javascript]("onplay")
  val onplayingAttribute = Att[Javascript]("onplaying")
  val onprogressAttribute = Att[Javascript]("onprogress")
  val onratechangeAttribute = Att[Javascript]("onratechange")
  val onreadystatechangeAttribute = Att[Javascript]("onreadystatechange")
  val onresetAttribute = Att[Javascript]("onreset")
  val onscrollAttribute = Att[Javascript]("onscroll")
  val onseekedAttribute = Att[Javascript]("onseeked")
  val onseekingAttribute = Att[Javascript]("onseeking")
  val onselectAttribute = Att[Javascript]("onselect")
  val onshowAttribute = Att[Javascript]("onshow")
  val onstalledAttribute = Att[Javascript]("onstalled")
  val onsubmitAttribute = Att[Javascript]("onsubmit")
  val onsuspendAttribute = Att[Javascript]("onsuspend")
  val ontimeupdateAttribute = Att[Javascript]("ontimeupdate")
  val onvolumechangeAttribute = Att[Javascript]("onvolumechange")
  val onwaitingAttribute = Att[Javascript]("onwaiting")

  val acceptAttribute = Att[String]("accept")
  val acceptCharsetAttribute = Att[String]("accept-charset")
  val accesskeyAttribute = Att[String]("accesskey")
  val actionAttribute = Att[String]("action")
  val alignAttribute = Att[String]("align")
  val altAttribute = Att[String]("alt")
  val asyncAttribute = Att[String]("async")
  val autocapitalizeAttribute = Att[String]("autocapitalize")
  val autocompleteAttribute = Att[String]("autocomplete")
  val autofocusAttribute = Att[String]("autofocus")
  val autoplayAttribute = Att[String]("autoplay")
  val bgcolorAttribute = Att[String]("bgcolor")
  val borderAttribute = Att[String]("border")
  val bufferedAttribute = Att[String]("buffered")
  val challengeAttribute = Att[String]("challenge")
  val charsetAttribute = Att[String]("charset")
  val checkedAttribute = Att[String]("checked")
  val citeAttribute = Att[String]("cite")
  val classAttribute = Att[String]("class")
  val codeAttribute = Att[String]("code")
  val codebaseAttribute = Att[String]("codebase")
  val colorAttribute = Att[String]("color")
  val colsAttribute = Att[String]("cols")
  val colspanAttribute = Att[String]("colspan")
  val contentAttribute = Att[String]("content")
  val contenteditableAttribute = Att[String]("contenteditable")
  val contextmenuAttribute = Att[String]("contextmenu")
  val controlsAttribute = Att[String]("controls")
  val coordsAttribute = Att[String]("coords")
  val crossoriginAttribute = Att[String]("crossorigin")
  val dataAttribute = Att[String]("data")
  val datetimeAttribute = Att[String]("datetime")
  val defaultAttribute = Att[String]("default")
  val deferAttribute = Att[String]("defer")
  val dirAttribute = Att[String]("dir")
  val dirnameAttribute = Att[String]("dirname")
  val disabledAttribute = Att[String]("disabled")
  val downloadAttribute = Att[String]("download")
  val draggableAttribute = Att[String]("draggable")
  val dropzoneAttribute = Att[String]("dropzone")
  val enctypeAttribute = Att[String]("enctype")
  val forAttribute = Att[String]("for")
  val formAttribute = Att[String]("form")
  val formactionAttribute = Att[String]("formaction")
  val headersAttribute = Att[String]("headers")
  val heightAttribute = Att[String]("height")
  val hiddenAttribute = Att[String]("hidden")
  val highAttribute = Att[String]("high")
  val hrefAttribute = Att[String]("href")
  val hreflangAttribute = Att[String]("hreflang")
  val httpEquivAttribute = Att[String]("http-equiv")
  val iconAttribute = Att[String]("icon")
  val idAttribute = Att[String]("id")
  val integrityAttribute = Att[String]("integrity")
  val ismapAttribute = Att[String]("ismap")
  val itempropAttribute = Att[String]("itemprop")
  val keytypeAttribute = Att[String]("keytype")
  val kindAttribute = Att[String]("kind")
  val labelAttribute = Att[String]("label")
  val langAttribute = Att[String]("lang")
  val languageAttribute = Att[String]("language")
  val listAttribute = Att[String]("list")
  val loopAttribute = Att[String]("loop")
  val lowAttribute = Att[String]("low")
  val manifestAttribute = Att[String]("manifest")
  val maxAttribute = Att[String]("max")
  val maxlengthAttribute = Att[String]("maxlength")
  val minlengthAttribute = Att[String]("minlength")
  val mediaAttribute = Att[String]("media")
  val methodAttribute = Att[String]("method")
  val minAttribute = Att[String]("min")
  val multipleAttribute = Att[String]("multiple")
  val mutedAttribute = Att[String]("muted")
  val nameAttribute = Att[String]("name")
  val novalidateAttribute = Att[String]("novalidate")
  val openAttribute = Att[String]("open")
  val optimumAttribute = Att[String]("optimum")
  val patternAttribute = Att[String]("pattern")
  val pingAttribute = Att[String]("ping")
  val placeholderAttribute = Att[String]("placeholder")
  val posterAttribute = Att[String]("poster")
  val preloadAttribute = Att[String]("preload")
  val radiogroupAttribute = Att[String]("radiogroup")
  val readonlyAttribute = Att[String]("readonly")
  val relAttribute = Att[String]("rel")
  val requiredAttribute = Att[String]("required")
  val reversedAttribute = Att[String]("reversed")
  val rowsAttribute = Att[String]("rows")
  val rowspanAttribute = Att[String]("rowspan")
  val sandboxAttribute = Att[String]("sandbox")
  val scopeAttribute = Att[String]("scope")
  val scopedAttribute = Att[String]("scoped")
  val seamlessAttribute = Att[String]("seamless")
  val selectedAttribute = Att[String]("selected")
  val shapeAttribute = Att[String]("shape")
  val sizeAttribute = Att[String]("size")
  val sizesAttribute = Att[String]("sizes")
  val slotAttribute = Att[String]("slot")
  val spanAttribute = Att[String]("span")
  val spellcheckAttribute = Att[String]("spellcheck")
  val srcAttribute = Att[String]("src")
  val srcdocAttribute = Att[String]("srcdoc")
  val srclangAttribute = Att[String]("srclang")
  val srcsetAttribute = Att[String]("srcset")
  val startAttribute = Att[String]("start")
  val stepAttribute = Att[String]("step")
  val styleAttribute = Att[String]("style")
  val summaryAttribute = Att[String]("summary")
  val tabindexAttribute = Att[String]("tabindex")
  val targetAttribute = Att[String]("target")
  val titleAttribute = Att[String]("title")
  val translateAttribute = Att[String]("translate")
  val typeAttribute = Att[String]("type")
  val usemapAttribute = Att[String]("usemap")
  val valueAttribute = Att[String]("value")
  val widthAttribute = Att[String]("width")
  val wrapAttribute = Att[TrueFalse]("wrap")

  case class Text(str: String) extends Node[Phrasing] {
    override def toString(): String = str
  }

  trait Flow
  trait HtmlItems
  trait HeadItems
  trait Phrasing
  trait PlainText
  trait Metadata
  trait Sectioning
  trait Heading
  trait Embedded
  trait Interactive
  trait Palpable
  trait Colgroup
  trait ListType
  trait Ruby
  trait FigureContent
  trait LegendContent
  trait TemplateContent
  trait ImgContent
  trait OptionContent
  trait OptGroupContent
  trait SourceContent
  trait DtContent
  trait DdContent

  val html = Tag[HtmlItems, Nothing, Global]("html")
  val head = Tag[HeadItems with TemplateContent, HtmlItems, Global]("head")
  val title = Tag[PlainText, HeadItems, Global]("title")
  val base = Tag[Text, HeadItems, Global]("base")
  val link = Tag[Nothing, HeadItems, Global]("link", unclosed = true)
  val meta = Tag[Nothing, HeadItems, Global]("meta", unclosed = true)
  val style = Tag[Text, HeadItems, Global]("style")
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
  val hr = Tag[Nothing, Flow, Global]("hr", unclosed = true)
  val pre = Tag[Phrasing, Flow, Global]("pre")
  val blockquote = Tag[Flow, Flow, Global]("blockquote")
  val ol = Tag[ListType, Flow, Global]("ol")
  val ul = Tag[ListType, Flow, Global]("ul")
  val li = Tag[Flow, ListType, Global]("li")
  val dl = Tag[DtContent with DdContent, Flow, Global]("dl") // FIXME with TemplateContent
  val dt = Tag[Flow, DtContent, Global]("dt")
  val dd = Tag("dd")
  val figure = Tag[FigureContent with Flow, Flow, Global]("figure")
  val figcaption = Tag[Flow, FigureContent, Global]("figcaption")
  val main = Tag[Flow, Flow, Global]("main")
  val div = Tag[Flow, Flow, Global]("div")

  val a = TransparentTag("a")
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
  val br = Tag[Nothing, Phrasing, Global]("br", unclosed = true)
  val wbr = Tag[Nothing, Phrasing, Global]("wbr")

  val ins = TransparentTag("ins")
  val del = TransparentTag("del")

  val picture = Tag[SourceContent with ImgContent, Flow, Global]("picture")
  val source = Tag[Nothing, SourceContent, Global]("source")
  val img = Tag[Nothing, Embedded, Global]("img", unclosed = true)

  val iframe = Tag("iframe")
  val embed = Tag("embed")
  val `object` = Tag("object")
  val param = Tag("param")
  val video = Tag("video")
  val audio = Tag("audio")
  val track = Tag("track")
  val map = Tag("map")
  val area = Tag("area")

  val table = Tag("table")
  val caption = Tag("caption")
  val colgroup = Tag("colgroup") // FIXME with TemplateContent
  val col = Tag[Nothing, Colgroup, Global]("col", unclosed = true)
  val tbody = Tag("tbody")
  val thead = Tag("thead")
  val tfoot = Tag("tfoot")
  val tr = Tag("tr")
  val td = Tag("td")
  val th = Tag("th")

  val form = Tag[Flow, Flow, Global]("form")
  val label = Tag[Phrasing, Phrasing, Global]("label")
  val input = Tag[Nothing, Phrasing, Global]("input", unclosed = true)
  val button = Tag[Phrasing, Phrasing, Global]("button")
  val select = Tag[OptionContent with OptGroupContent, Flow, Global]("select")
  val datalist = Tag("datalist")
  val optgroup = Tag[OptionContent, OptGroupContent, Global]("optgroup")
  val option = Tag[Text, OptGroupContent, Global]("option")
  val textarea = Tag[Text, Flow, Global]("textarea")
  val output = Tag[Phrasing, Phrasing, Global]("output")
  val progress = Tag[Phrasing, Phrasing, Global]("progress")
  val meter = Tag[Phrasing, Phrasing, Global]("meter")
  val fieldset = Tag[LegendContent with Flow, Flow, Global]("fieldset")
  val legend = Tag[Phrasing, LegendContent, Global]("legend")

  val details = Tag("details")
  val summary = Tag("summary")
  val dialog = Tag("dialog")
  val script = Tag("script")
  val noscript = Tag("noscript")
  val template = Tag[Nothing, TemplateContent, Global]("template")
  val canvas = TransparentTag("canvas")

  implicit def stringToText(str: String): Text = Text(str)
  implicit def autoApplyTag[T](tag: Tag[_, T, _]): Node[T] = tag()

  case class InputType(id: String)

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

