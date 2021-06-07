package honeycomb

import annotation.*

import language.dynamics

case class Node[+TagType](tag: EmptyTag, attributes: Map[String, String], children: Seq[Node[_] | String]):
  lazy val inline: Boolean = children.forall {
    case node: Node[_] => node.tag.inline && node.inline
    case text: String  => true
  }

class EmptyTag(val tagName: String, val inline: Boolean = false, val unclosed: Boolean = true) extends Dynamic:
  def applyDynamicNamed(method: "apply")(attributes: (String, String)*): Node[this.type] =
    Node(this, attributes.to(Map), Nil)

case class TransparentExtrasTag[Children, Extras](name: String, override val unclosed: Boolean = false, override val inline: Boolean = false) extends EmptyTag(name, inline, unclosed):
  def applyDynamic[C <: Children](method: "apply")(children: (Node[C | Extras] | String)*): Node[C] =
    Node(this, Map(), children)
  
  def applyDynamicNamed[C <: Children](method: "apply")(attributes: (String, String)*)(children: Node[C | Extras]*): Node[C] =
    Node(this, attributes.to(Map), children)

case class TransparentTag[Children](name: String, override val unclosed: Boolean = false, override val inline: Boolean = false) extends EmptyTag(name, inline, unclosed):
  def applyDynamic[C <: Children](method: "apply")(children: (Node[C] | String)*): Node[C] =
    Node(this, Map(), children)
  
  def applyDynamicNamed[C <: Children](method: "apply")(attributes: (String, String)*)(children: Node[C]*): Node[C] =
    Node(this, attributes.to(Map), children)

case class Tag[Children](name: String, override val unclosed: Boolean = false, override val inline: Boolean = false) extends EmptyTag(name, inline, unclosed):
  def applyDynamic(method: "apply")(children: (Node[Children] | String)*): Node[this.type] =
    Node(this, Map(), children)
  
  def applyDynamicNamed(method: "apply")(attributes: (String, String)*)(children: Node[Children]*): Node[this.type] =
    Node(this, attributes.to(Map), children)

object Html5:
  def serialize(): String = ""

object A extends TransparentTag[NonInteractive]("a", inline = true)
object Abbr extends Tag[Phrasing]("abbr", inline = true)
object Address extends Tag[Flow | Palpable]("address")
object Area extends EmptyTag("area", inline = true)
object Article extends Tag[Flow]("article") // further constraints on descendants
object Aside extends Tag[Flow]("aside")
object Audio extends TransparentExtrasTag[Any, Track.type]("audio", inline = true) // complicated!
object B extends Tag[Phrasing]("b", inline = true)
object Base extends EmptyTag("base")
object Bdi extends Tag[Phrasing]("bdi", inline = true)
object Bdo extends Tag[Phrasing]("bdo", inline = true)
object Blockquote extends Tag[Flow]("blockquote")
object Body extends Tag[Flow]("body")
object Br extends EmptyTag("br", inline = true)
object Button extends Tag[Phrasing]("button", inline = true)
object Canvas extends TransparentTag[NonInteractive]("canvas", inline = true) // complicated
object Caption extends Tag[Flow]("caption") // no tables
object Cite extends Tag[Phrasing]("cite", inline = true)
object Code extends Tag[Phrasing]("code", inline = true)
object Col extends EmptyTag("col")
object Colgroup extends Tag[Col.type | Template.type]("colgroup")
object Data extends Tag[Phrasing]("data", inline = true)
object Datalist extends Tag[Phrasing | Option.type | ScriptSupporting]("datalist", inline = true)
object Dd extends Tag[Flow]("dd")
object Del extends TransparentTag[Any]("del", inline = true)
object Details extends Tag[Summary.type | Flow]("details")
object Dfn extends Tag[Phrasing]("dfn", inline = true)
object Dialog extends Tag[Flow]("dialog")
object Div extends Tag[Flow]("div")
object Dl extends Tag[Dt.type | DlType | Div.type]("dl")
object Dt extends Tag[Flow]("dt") // further constraints
object Em extends Tag[Phrasing]("em", inline = true)
object Embed extends EmptyTag("embed", inline = true)
object Fieldset extends Tag[Legend.type | Flow]("fieldset")
object Figcaption extends Tag[Flow]("figcaption")
object Figure extends Tag[Figcaption.type | Flow]("figure") // first or last element may be figcaption, but not both
object Footer extends Tag[Flow]("footer")
object Form extends Tag[Flow]("form")
object H1 extends Tag[Phrasing]("h1")
object H2 extends Tag[Phrasing]("h2")
object H3 extends Tag[Phrasing]("h3")
object H4 extends Tag[Phrasing]("h4")
object H5 extends Tag[Phrasing]("h5")
object H6 extends Tag[Phrasing]("h6")
object HMap extends TransparentTag[Phrasing | Flow | Palpable]("map", inline = true)
object Head extends Tag[Metadata]("head")
object Header extends Tag[Flow]("header")
object Hgroup extends Tag[H1.type | H2.type | H3.type | H4.type | H5.type | H6.type]("hgroup")
object Hr extends EmptyTag("hr")
object Html extends Tag[Head.type | Body.type]("html") // head followed by body
object I extends Tag[Phrasing]("i", inline = true)
object Iframe extends EmptyTag("iframe", unclosed = false, inline = true)
object Img extends EmptyTag("img", inline = true)
object Input extends EmptyTag("input", inline = true)
object Ins extends TransparentTag[Any]("ins", inline = true)
object Kbd extends Tag[Phrasing]("kbd", inline = true)
object Label extends Tag[Phrasing]("label", inline = true)
object Legend extends Tag[Phrasing | Heading]("legend")
object Li extends Tag[Flow]("li")
object Link extends EmptyTag("link", inline = true)
object Main extends Tag[Flow]("main")
object Mark extends Tag[Phrasing]("mark", inline = true)
object Menu extends Tag[Flow]("menu")
object Meta extends EmptyTag("meta", inline = true)
object Meter extends Tag[Phrasing]("meter", inline = true)
object Nav extends Tag[Flow]("nav")
object Noscript extends TransparentExtrasTag[Any, Link.type | Style.type | Meta.type]("noscript", inline = true)
object Object extends TransparentExtrasTag[Any, Param.type]("object", inline = true)
object Ol extends Tag[Li.type | ScriptSupporting]("ol")
object Optgroup extends Tag[Option.type | ScriptSupporting]("optgroup")
object Option extends Tag[Nothing]("option")
object Output extends Tag[Phrasing]("output", inline = true)
object P extends Tag[Phrasing]("p")
object Param extends EmptyTag("param")
object Picture extends Tag[Source.type | Img.type | ScriptSupporting]("picture", inline = true)
object Pre extends Tag[Phrasing]("pre")
object Progress extends Tag[Phrasing]("progress", inline = true)
object Q extends Tag[Phrasing]("q", inline = true)
object Rb extends Tag[Phrasing]("rb")
object Rp extends Tag[Nothing]("rp")
object Rt extends Tag[Phrasing]("rt")
object Ruby extends Tag[Phrasing | Rp.type | Rt.type]("ruby", inline = true)
object S extends Tag[Phrasing]("s", inline = true)
object Samp extends Tag[Phrasing]("samp", inline = true)
object Script extends Tag[Nothing]("script", inline = true)
object Section extends Tag[Flow]("section")
object Select extends Tag[Option.type | Optgroup.type | ScriptSupporting]("select", inline = true)
object Slot extends TransparentTag[Any]("slot", inline = true)
object Small extends Tag[Phrasing]("small", inline = true)
object Source extends EmptyTag("source")
object Span extends Tag[Phrasing]("span", inline = true)
object Strong extends Tag[Phrasing]("strong", inline = true)
object Style extends Tag[Nothing]("style")
object Sub extends Tag[Phrasing]("sub", inline = true)
object Summary extends Tag[Phrasing | Heading]("summary")
object Sup extends Tag[Phrasing]("sup", inline = true)
object Table extends Tag[Caption.type | Colgroup.type | Thead.type | Tbody.type | Tr.type | Tfoot.type | ScriptSupporting]("table")
object Tbody extends Tag[Tr.type | ScriptSupporting]("tbody")
object Td extends Tag[Flow]("td")
object Template extends EmptyTag("template", unclosed = false, inline = true)
object Textarea extends Tag[Nothing]("textarea", inline = true)
object Tfoot extends Tag[Tr.type | ScriptSupporting]("tfoot")
object Th extends Tag[Flow]("th")
object Thead extends Tag[Tr.type | ScriptSupporting]("thead")
object Time extends Tag[Phrasing]("time", inline = true)
object Title extends Tag[Nothing]("title")
object Tr extends Tag[Td.type | Th.type | ScriptSupporting]("tr")
object Track extends EmptyTag("track")
object U extends Tag[Phrasing]("u", inline = true)
object Ul extends Tag[Li.type | ScriptSupporting]("ul")
object Var extends Tag[Nothing]("var", inline = true)
object Video extends TransparentExtrasTag[Any, Track.type | Source.type]("video", inline = true) // complicated!
object Wbr extends EmptyTag("wbr", inline = true)
type DlType = Dl.type



type Metadata = Base.type | Link.type | Meta.type | Noscript.type | Script.type | Style.type | Template.type |
    Title.type

type Phrasing = A.type | Abbr.type | Area.type | Audio.type | B.type | Bdi.type | Bdo.type | Br.type |
    Button.type | Canvas.type | Cite.type | Code.type | Data.type | Datalist.type | Del.type | Dfn.type |
    Em.type | Embed.type | I.type | Iframe.type | Img.type | Input.type | Ins.type | Kbd.type | Label.type |
    Link.type | HMap.type | Mark.type | Meta.type | Meter.type | Noscript.type | Object.type |
    Output.type | Picture.type | Progress.type | Q.type | Ruby.type | S.type | Samp.type | Script.type |
    Select.type | Slot.type | Small.type | Span.type | Strong.type | Sub.type | Sup.type |
    Template.type | Textarea.type | Time.type | U.type | Var.type | Video.type | Wbr.type

type Flow = A.type | Abbr.type | Address.type | Area.type | Article.type | Aside.type | Audio.type | B.type |
    Bdi.type | Bdo.type | Blockquote.type | Br.type | Button.type | Canvas.type | Cite.type | Code.type |
    Data.type | Datalist.type | Del.type | Details.type | Dfn.type | Dialog.type | Div.type | Dl.type |
    Em.type | Embed.type | Fieldset.type | Figure.type | Footer.type | Form.type | H1.type | H2.type | H3.type |
    H4.type | H5.type | H6.type | Header.type | Hgroup.type | Hr.type | I.type | Iframe.type | Img.type |
    Input.type | Ins.type | Kbd.type | Label.type | Link.type | Main.type | HMap.type | Mark.type |
    Menu.type | Meta.type | Meter.type | Nav.type | Noscript.type | Object.type | Ol.type | Output.type |
    P.type | Picture.type | Pre.type | Progress.type | Q.type | Ruby.type | S.type | Samp.type | Script.type |
    Section.type | Select.type | Slot.type | Small.type | Span.type | Strong.type | Sub.type | Sup.type |
    Table.type | Template.type | Textarea.type | Time.type | U.type | Ul.type | Var.type |
    Video.type | Wbr.type

type Sectioning = Article.type | Aside.type | Nav.type | Section.type

type Heading = H1.type | H2.type | H3.type | H4.type | H5.type | H6.type | Hgroup.type

type Embedded = Audio.type | Canvas.type | Embed.type | Iframe.type | Img.type | Object.type |
    Picture.type | Video.type

type Interactive = A.type | Audio.type | Button.type | Details.type | Embed.type | Iframe.type | Img.type |
    Input.type | Label.type | Select.type | Textarea.type | Video.type

type Palpable = A.type | Abbr.type | Address.type | Article.type | Aside.type | Audio.type | B.type | Bdi.type |
    Bdo.type | Blockquote.type | Button.type | Canvas.type | Cite.type | Code.type | Data.type | Details.type |
    Dfn.type | Div.type | Dl.type | Em.type | Embed.type | Fieldset.type | Figure.type | Footer.type |
    Form.type | H1.type | H2.type | H3.type | H4.type | H5.type | H6.type | Header.type | Hgroup.type | I.type |
    Iframe.type | Img.type | Input.type | Ins.type | Kbd.type | Label.type | Main.type | HMap.type | Mark.type |
    Menu.type | Meter.type | Nav.type | Object.type | Ol.type | Output.type | P.type | Pre.type |
    Progress.type | Q.type | Ruby.type | S.type | Samp.type | Section.type | Select.type | Small.type |
    Span.type | Strong.type | Sub.type | Sup.type | Table.type | Textarea.type | Time.type | U.type |
    Ul.type | Var.type | Video.type

type ScriptSupporting = Script.type | Template.type

type NonInteractive = Abbr.type | Address.type | Area.type | Article.type | Aside.type | Audio.type | B.type |
    Base.type | Bdi.type | Bdo.type | Blockquote.type | Br.type | Canvas.type | Cite.type | Code.type |
    Data.type | Datalist.type | Del.type | Dfn.type | Dialog.type | Div.type | Dl.type | Em.type |
    Fieldset.type | Figure.type | Footer.type | Form.type | H1.type | H2.type | H3.type | H4.type | H5.type |
    H6.type | Header.type | Hgroup.type | Hr.type | I.type | Img.type | Input.type | Ins.type | Kbd.type |
    Link.type | Main.type | HMap.type | Mark.type | Menu.type | Meta.type | Meter.type | Nav.type |
    Noscript.type | Object.type | Ol.type | Output.type | P.type | Picture.type | Pre.type | Progress.type |
    Q.type | Ruby.type | S.type | Samp.type | Script.type | Section.type | Slot.type | Small.type | Span.type |
    Strong.type | Style.type | Sub.type | Sup.type | Table.type | Template.type | Time.type |
    Title.type | U.type | Ul.type | Var.type | Wbr.type

object Example:
  
  val html: Document = Document(Html(
    Head(
      Base(foo = "bar"),
      Meta(x = "y")
    ),
    Body(
      A(href = "hello")(P("Hello world"))
    ),
    Body(
      Table(
        Thead(att = "value")(Tr()),
        Tbody(
          Tr(
            Td("Text content"),
            Td("Text content ", B("with bold"), " text")
          ),
          Tr(
            Td("Text content"),
            Td("Text content ", B("with bold"), " text")
          )
        ),
      )
    )
  ))

case class Document(root: Node[Html.type]):
  def serialize: String =
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    
    def newline(n: Int = 0): Unit =
      indent += n
      linebreak = true
    
    def whitespace(): Unit =
      if linebreak then
        buf.append("\n")
        for i <- 1 to indent do buf.append("  ")
      linebreak = false

    def next(node: Node[_] | String): Unit = node match
      case node: Node[_] =>
        whitespace()
        buf.append("<")
        buf.append(node.tag.tagName)
        for (key, value) <- node.attributes do
          buf.append(" ")
          buf.append(key)
          buf.append("=\"")
          buf.append(value)
          buf.append("\"")
        buf.append(">")
        if !node.inline then newline(1)
        for child <- node.children do
          val splitLine = child match
            case node: Node[_] => !node.inline || !node.tag.inline
            case _             => false
          if splitLine then newline()
          next(child)
          if splitLine then newline()
        if !node.inline then newline(-1)
        if !node.tag.unclosed then
          whitespace()
          buf.append("</")
          buf.append(node.tag.tagName)
          buf.append(">")
          if !node.inline then newline(0)
      case text: String =>
        whitespace()
        buf.append(text)
    
    buf.append("<!DOCTYPE html>\n")
    next(root)

    buf.toString


@main def run(): Unit =
  println(Example.html.serialize)