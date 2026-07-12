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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import language.dynamics

import java.lang as jl
import java.util as ju

import scala.annotation.tailrec
import scala.quoted.*
import scala.util.NotGiven

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import typonym.*
import vacuous.*
import zephyrine.*

object Html extends Tag.Container
  ( label       = "html",
    autoclose   = true,
    admissible  = Set("head", "body"),
    mode        = Html.Mode.Whitespace,
    insertable  = true,
    foreign     = false,
    boundary    = true ), Format:
  // Controls how an `Html` document is serialized by `emit`. `indented` lays whitespace-mode
  // elements out one per indented line (the default); `flatHtmlFormatting` keeps it on one line.
  // (`.show` of a bare node is always compact.) Bundled as `formatting.indentedHtmlFormatting`
  // and `formatting.flatHtmlFormatting`.
  object Formatting:
    def apply(indented: Boolean): Formatting = Basic(indented)
    private case class Basic(indented: Boolean) extends Formatting
    given default: Formatting = apply(indented = true)

  trait Formatting extends zephyrine.Formatting:
    def indented: Boolean

  type Topic = "html"
  type Transport = "head" | "body"

  sealed trait Integral
  sealed trait Decimal
  sealed trait Id

  given media: Document[Html] is Media:
    extension (value: Document[Html])
      def mediaType: MediaType = media"text/html"(charset = "UTF-8")

  def doctype: Doctype = Doctype(t"html")

  extension (html: Seq[Html])
    def nodes: IArray[Node] =
      var count = 0

      for item <- html do item match
        case fragment: Fragment => count += fragment.nodes.length
        case _                  => count += 1

      val array = new Array[Node](count)

      var index = 0

      for item <- html do item match
        case Fragment(nodes*) => for node <- nodes do
          writable(array)(index) = node
          index += 1

        case node: Node =>
          writable(array)(index) = node
          index += 1

      array.immutable(using Unsafe)

  inline given interpolator: Html is Interpolable:
    type Result = Html

    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Html =

      ${honeycomb.internal.interpolator[parts, origins]('insertions)}

  inline given extrapolator: Html is Extrapolable:

    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple]
      ( scrutinee: Html )
    :   Extrapolation[Html] =

      ${honeycomb.internal.extractor[parts, origins]('scrutinee)}


  given addable: [dom        <: Dom,
                  leftTopic  <: Label,
                  rightTopic <: Label,
                  left       <: Html of leftTopic in dom,
                  right      <: Html of rightTopic in dom]
  =>  left is Addable by right to (Fragment of leftTopic | rightTopic in dom) =

    Addable: (left, right) =>
      Fragment(List(left, right).nodes*).of[leftTopic | rightTopic].in[dom]


  // Internal Tactic used by the permissive-variant givens. Recovery warnings
  // (`raise`) are discarded; truly unrecoverable conditions (`abort`) still
  // throw, since the caller hasn't supplied a way to handle them.
  private def lenientTactic: Tactic[ParseError] = new Tactic[ParseError]:
    given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny
    def diagnostics: Diagnostics = errorDiagnostics.stackTracesDiagnostics
    def record(error: Diagnostics ?=> ParseError): Unit = ()

    def abort(error: Diagnostics ?=> ParseError): Nothing =
      throw error(using diagnostics)

    def certify(): Unit = ()


  given strictAggregable: [content <: Label: Reifiable to List[String]]
  =>  ( dom:    Dom,
        tactic: Tactic[ParseError],
        strict: NotGiven[Html.Recovery.Permissive] )
  =>  (((Html of content) is Aggregable by Text)^{tactic, caps.any}) =

    new Aggregable:
      type Self = Html of content
      type Operand = Text

      def aggregate(input: LazyList[Text]): Html of content =
        val root = Tag.root(content.reify.map(_.tt).to(Set))
        HtmlParser.fromIterator(input.iterator, permissive = false).parseHtml(root).of[content]

      override def accept(stream: (Stream[Text] over Credit)^): Html of content =
        val root = Tag.root(content.reify.map(_.tt).to(Set))
        HtmlParser.fromStream(stream, permissive = false).parseHtml(root).of[content]

  given strictAggregable2: (dom: Dom, tactic: Tactic[ParseError])
  =>  ( strict: NotGiven[Html.Recovery.Permissive] )
  =>  ((Html is Aggregable by Text)^{tactic, caps.any}) =
    new Aggregable:
      type Self = Html
      type Operand = Text

      def aggregate(input: LazyList[Text]): Html =
        HtmlParser.fromIterator(input.iterator, permissive = false)
        . parseHtml(dom.generic, doctypes = false)

      override def accept(stream: (Stream[Text] over Credit)^): Html =
        HtmlParser.fromStream(stream, permissive = false)
        . parseHtml(dom.generic, doctypes = false)

  given strictLoadable: (dom: Dom, tactic: Tactic[ParseError])
  =>  ( strict: NotGiven[Html.Recovery.Permissive] )
  =>  ((Html is Loadable by Text)^{tactic, caps.any}) = stream =>
    val root = Tag.root(Set(t"html"))

    HtmlParser.fromIterator(stream.iterator, permissive = false)
    . parseHtml(root, doctypes = true) match
      case Fragment(Doctype(doctype), content) => Document(content, dom)
      case html@Element("html", _, _, _)       => Document(html, dom)

      case _ =>
        abort(ParseError(Html, Position(1.u, 1.u), Issue.BadDocument))


  // Last-resort safety net for the permissive variants. Tokenizer-level
  // conditions that haven't been wired into the parser's per-site recovery
  // (e.g. EOF mid-comment) still call `fail`, which propagates as a thrown
  // `ParseError`. Permissive callers haven't supplied a `Tactic[ParseError]`,
  // so we swallow it here and yield an empty `Fragment` rather than letting
  // the exception escape — the permissive reader is contractually no-throw.
  private inline def lenient[result](inline fallback: => result)(inline body: => result): result =
    try body catch case _: ParseError => fallback

  given permissiveAggregable: [content <: Label: Reifiable to List[String]]
  =>  ( dom: Dom )
  =>  Html.Recovery.Permissive
  =>  (Html of content) is Aggregable by Text =

    new Aggregable:
      type Self = Html of content
      type Operand = Text

      def aggregate(input: LazyList[Text]): Html of content =
        given Tactic[ParseError] = lenientTactic
        val root = Tag.root(content.reify.map(_.tt).to(Set))

        lenient(Fragment().of[content]):
          HtmlParser.fromIterator(input.iterator, permissive = true).parseHtml(root).of[content]

      override def accept(stream: (Stream[Text] over Credit)^): Html of content =
        given Tactic[ParseError] = lenientTactic
        val root = Tag.root(content.reify.map(_.tt).to(Set))

        lenient(Fragment().of[content]):
          HtmlParser.fromStream(stream, permissive = true).parseHtml(root).of[content]

  given permissiveAggregable2: (dom: Dom)
  =>  Html.Recovery.Permissive
  =>  Html is Aggregable by Text =
    new Aggregable:
      type Self = Html
      type Operand = Text

      def aggregate(input: LazyList[Text]): Html =
        given Tactic[ParseError] = lenientTactic

        lenient(Fragment()):
          HtmlParser.fromIterator(input.iterator, permissive = true)
          . parseHtml(dom.generic, doctypes = false)

      override def accept(stream: (Stream[Text] over Credit)^): Html =
        given Tactic[ParseError] = lenientTactic

        lenient(Fragment()):
          HtmlParser.fromStream(stream, permissive = true)
          . parseHtml(dom.generic, doctypes = false)

  given permissiveLoadable: (dom: Dom)
  =>  Html.Recovery.Permissive
  =>  Html is Loadable by Text = stream =>
    given Tactic[ParseError] = lenientTactic
    val root = Tag.root(Set(t"html"))

    lenient(Document(Fragment(), dom)):
      HtmlParser.fromIterator(stream.iterator, permissive = true)
      . parseHtml(root, doctypes = true) match
        case Fragment(Doctype(doctype), content) => Document(content, dom)
        case html@Element("html", _, _, _)       => Document(html, dom)
        case other                               => Document(other, dom)

  // Up to 32 levels of two-space indentation
  private val indentation: Text =
    "\n                                                                "

  // Streams a document's HTML source, using the document's own DOM and the indentation from the
  // contextual `Formatting`. The whole emission lives in this instance so `.stream` is the single
  // route to streamed HTML; the producing code runs on a separate fiber.
  given streamable: (monitor: Monitor, probate: Probate)
  =>  ((Document[Html] is Streamable by Text)^{monitor, caps.any}) = document =>
    val formatting = summon[Formatting]
    val dom = document.metadata
    val producer = Producer[Text](4096)
    val block = formatting.indented

    async:
      writeHtml(producer, dom, document.metadata.doctype, 0, block, Mode.Whitespace)
      writeHtml(producer, dom, document.root, 0, block, Mode.Whitespace)
      producer.finish()

    producer.iterator.to(LazyList)

  // `.show` serializes against the standard WHATWG (HTML5) DOM and without indentation, so a bare
  // node renders correctly (void elements, escaping) even outside a `Document`. The `Streamable`
  // instance above uses the document's own DOM and indents.
  given showable: [html <: Html] => html is Showable = node =>
    Producer.collect[Text](): producer =>
      writeHtml(producer, doms.html.whatwg, node, 0, false, Mode.Whitespace)

  // HTML5 text-content escaping: `&`, `<` and `>`. Raw-text elements such as `script` and `style`
  // use `Mode.Raw` and are written verbatim by `writeHtml`.
  private def writeEscapedText(producer: Producer[Text]^, text: Text): Unit =
    val source = text.s
    val length = source.length
    var start = 0
    var index = 0

    inline def escape(entity: Text): Unit =
      if index > start then producer.put(text, start.z, index - start)
      producer.put(entity)
      start = index + 1

    while index < length do
      source.charAt(index) match
        case '&' => escape(t"&amp;")
        case '<' => escape(t"&lt;")
        case '>' => escape(t"&gt;")
        case _   => ()

      index += 1

    if length > start then producer.put(text, start.z, length - start)

  // HTML5 escaping for a double-quoted attribute value: `&` and the `"` delimiter. `<` and `>` are
  // permitted literally in attribute values.
  private def writeEscapedAttribute(producer: Producer[Text]^, text: Text): Unit =
    val source = text.s
    val length = source.length
    var start = 0
    var index = 0

    inline def escape(entity: Text): Unit =
      if index > start then producer.put(text, start.z, index - start)
      producer.put(entity)
      start = index + 1

    while index < length do
      source.charAt(index) match
        case '&' => escape(t"&amp;")
        case '"' => escape(t"&quot;")
        case _   => ()

      index += 1

    if length > start then producer.put(text, start.z, length - start)

  // The single, spec-correct HTML serializer, shared by streaming `emit` and synchronous
  // `showable` so the two cannot drift. Void elements omit their close tag; raw-text elements
  // suppress escaping; whitespace-mode elements are indented when `block` is set.
  private def writeHtml
    ( producer: Producer[Text]^,
      dom:      Dom,
      node:     Html,
      indent:   Int,
      block:    Boolean,
      mode:     Mode )
  :   Unit =

    node match
      case Fragment(nodes*) =>
        nodes.each(writeHtml(producer, dom, _, indent, block, mode))

      case Comment(comment) =>
        producer.put("<!--")
        producer.put(comment)
        producer.put("-->")

      case Doctype(text) =>
        producer.put("<!DOCTYPE ")
        producer.put(text) // FIXME: entities
        producer.put(">")

      case TextNode(text) =>
        mode match
          case Mode.Raw => producer.put(text)
          case _        => writeEscapedText(producer, text)

      case Element(label, attributes, nodes, _) =>
        if block then producer.put(indentation, Prim, indent*2 + 1)
        producer.put("<")
        producer.put(label)

        // No emptiness guard: `each` no-ops on an empty `Attributes`, and the
        // inline `nil` extension fails to dealias the opaque through its
        // capture-decorated pattern proxy under capture checking.
        attributes.each: (key, value) =>
          producer.put(" ")
          producer.put(key)

          value.let: value =>
            producer.put("=\"")
            writeEscapedAttribute(producer, value)
            producer.put("\"")

        producer.put(">")

        val mode = dom.elements(label).lay(Mode.Normal)(_.mode)

        val whitespace =
          (mode == Mode.Whitespace || !nodes.exists(_.isInstanceOf[TextNode])) &&
            block

        if !dom.elements(label).lay(false)(_.void) then
          nodes.each(writeHtml(producer, dom, _, indent + 1, whitespace, mode))

          if block && whitespace
          then producer.put(indentation, Prim, (indent*2 + 1).min(indentation.length))

          producer.put("</")
          producer.put(label)
          producer.put(">")


  private enum Token:
    case Close, Comment, Empty, Open, Doctype, Cdata

  private enum Level:
    case Ascend, Descend, Peer, Skip

  private val formattingTags: Set[Text] = Set(
    t"a", t"b", t"big", t"code", t"em", t"font", t"i", t"nobr",
    t"s", t"small", t"strike", t"strong", t"tt", t"u" )

  trait Populable:
    node: Element =>
      def apply(children: Optional[Html of (? <: node.Transport)]*)
      :   Element of node.Topic in node.Form =

        new Element(node.label, node.attributes, children.compact.nodes, node.foreign):
          type Topic = node.Topic
          type Form = node.Form


  trait Transparent:
    node: Element =>
      def apply[labels <: Label](children: Optional[Html of (? <: (labels | node.Transport))]*)
      :   Element of labels in node.Form =

        new Element(node.label, node.attributes, children.compact.nodes, node.foreign):
          type Topic = labels
          type Form = node.Form


  import Issue.*
  def name: Text = t"HTML"

  given text: [label >: "#text" <: Label] => Conversion[Text, Html of label] =
    TextNode(_).of[label]

  given string: [label >: "#text" <: Label] => Conversion[String, Html of label] =
    string => TextNode(string.tt).of[label]

  given conversion3: [label <: Label, content >: label <: Label]
  =>  Conversion[Html of label, Html of content] =

    _.of[content]


  given comment: [content <: Label] =>  Conversion[Comment, Html of content] =
    _.of[content]

  given string2: Conversion[String, Html of "#foreign"] =
    string => TextNode(string.tt).of["#foreign"]


  given renderable: [content <: Label, value: Renderable in content]
  =>  Conversion[value, Html of content] =

    value.render(_)


  given sequences: [nodal, html <: Html] => (conversion: Conversion[nodal, html])
  =>  Conversion[Seq[nodal], Seq[html]] =

    (sequence: Seq[nodal]) =>
      sequence.map(conversion(_))


  enum Issue extends Format.Issue:
    case BadInsertion
    case ExpectedMore
    case UnexpectedDoctype
    case BadDocument
    case InvalidCdata
    case InvalidTag(name: Text)
    case InvalidTagStart(prefix: Text)
    case DuplicateAttribute(name: Text)
    case InadmissibleTag(name: Text, parent: Text)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: Text)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: Text, close: Text)
    case UnopenedTag(close: Text)
    case Incomplete(tag: Text)
    case UnknownAttribute(name: Text)
    case UnknownAttributeStart(name: Text)
    case InvalidAttributeUse(attribute: Text, element: Text)

    def describe: Message = this match
      case BadInsertion                   => m"a value cannot be inserted into HTML at this point"
      case ExpectedMore                   => m"the content ended prematurely"
      case UnexpectedDoctype              => m"the document type declaration was not expected here"
      case BadDocument                    => m"the document did not contain a single root tag"
      case InvalidTag(name)               => m"<$name> is not a valid tag"
      case InvalidTagStart(prefix)        => m"there is no valid tag whose name starts $prefix"
      case DuplicateAttribute(name)       => m"the attribute $name already exists on this tag"
      case InadmissibleTag(name, parent)  => m"<$name> cannot be a child of <$parent>"
      case Unexpected(char)               => m"the character $char was not expected"
      case UnknownEntity(name)            => m"the entity &$name is not defined"
      case UnopenedTag(close)             => m"the tag </$close> has no corresponding opening tag"
      case Incomplete(tag)                => m"the content ended while the tag <$tag> was left open"
      case UnknownAttribute(name)         => m"$name is not a recognized attribute"
      case UnknownAttributeStart(name)    => m"there is no valid attribute whose name starts $name"
      case InvalidAttributeUse(name, tag) => m"the attribute $name cannot be used on the tag <$tag>"

      case InvalidCdata =>
        m"CDATA content is only permitted in foreign namespaces"

      case OnlyWhitespace(char) =>
        m"the character $char was found where only whitespace is permitted"

      case ForbiddenUnquoted(char) =>
        m"the character $char is forbidden in an unquoted attribute"

      case MismatchedTag(open, close) =>
        m"the tag </$close> did not match the opening tag <$open>"

  case class Position
    ( line:                Ordinal,
      column:              Ordinal,
      override val offset: Optional[Int] = Unset,
      override val length: Optional[Int] = Unset )
  extends Format.Position:
    def describe: Text = t"line ${line.n1}, column ${column.n1}"
    override def span: Span = Span.line(line, column, length.or(0))


  enum Mode:
    case Raw, Rcdata, Whitespace, Normal

  // Bringing a `Recovery.Permissive` into scope selects the permissive HTML
  // reader, which recovers from a defined subset of malformed inputs instead
  // of aborting and does not require a `Tactic[ParseError]`. To enable it,
  // `import honeycomb.recoveries.permissiveRecovery` (defined in honeycomb_core.scala).
  object Recovery:
    class Permissive

  enum Hole:
    case Text, Tagbody, Comment
    case Element(tag: Text)
    case Attribute(tag: Text, attribute: Text)
    case Node(parent: Text)

  // Direct parser: bypasses Cursor entirely. Operates directly on the
  // underlying String with a `var pos`. Currently handles a focused subset
  // of WHATWG HTML5 — doctype, regular elements (with attributes and text),
  // void elements (looked up via `dom.elements`), comments, character/named
  // entities. Does NOT yet handle: RCDATA / Raw modes (script/style/title),
  // foster parenting in tables, autoclose inference, DOM inference (head/
  // body insertion), foreign elements (svg, math), or macro callbacks. The
  // existing cursor-based `parse` below remains the fall-back for those.
  // ───────────────────────────────────────────────────────────────────────
  // Single Cursor-backed parser. The same body runs whether the input is
  // an in-memory `Text` (pre-fills the cursor's buffer) or an
  // `Iterator[Text]` (pulls chunks via the loader). Slicing is uniform
  // (one buffer; one `arraycopy`) so there's no separate same-block fast
  // path versus cross-block grab path. Line/column tracking is delegated
  // to Cursor via `linefeedChars` lineation, so `computePosition` is
  // O(1) — the per-byte tracking cost is small and avoids re-walking the
  // source on each error (which used to be O(n)).

  // Exclusive write views over method-local growable arrays: a bare-typed local
  // reads as `.rd` under separation checking, but a capability-typed binding
  // would be hidden (by the statement rule) from the local defs that capture it.
  // `writable` grants a write through a cast; `confined` erases a fresh grown
  // array's capture as it replaces its confined predecessor.
  private[honeycomb] inline def writable[element](array: Array[element]): Array[element]^ =
    array.asInstanceOf[Array[element]^]

  private[honeycomb] inline def confined[element](array: Array[element]^): Array[element] =
    array.asInstanceOf[Array[element]]

  private[honeycomb] object HtmlParser:
    // Use untracked lineation in the cursor: avoids a per-`advance` newline
    // check and a per-`mark` write into the cursor's parallel offsets
    // array. Errors still carry an accurate absolute `offset` / `length`
    // span (what tests assert on and what users need to locate the
    // failure), but `line` / `column` are reconstructed on demand by
    // scanning the currently-buffered chars from offset 0 to the error
    // position — an O(buffer) cost paid only on the failure path.

    def fromText(text: Text, permissive: Boolean = false)(using Dom): HtmlParser^ =
      new HtmlParser(Cursor[Text](text), permissive)

    def fromIterator(input: Iterator[Text], permissive: Boolean = false)(using Dom): HtmlParser^ =
      new HtmlParser(Cursor[Text](input), permissive)

    def fromStream(input: (Stream[Text] over Credit)^, permissive: Boolean = false)(using Dom)
    :   HtmlParser^ =

      // Ownership moves through a neutral carrier: `Aggregable.accept` (this
      // factory's caller) cannot declare `consume` on its trait signature, so
      // the endpoint's single-ownership transfer is by convention here.
      val inputRef: AnyRef = input.asInstanceOf[AnyRef]

      new HtmlParser(Cursor[Text](inputRef.asInstanceOf[(Stream[Text] over Credit)^]), permissive)

  // An exclusive, stateful capability, like `caesura`'s parser: it CONSUMES its
  // cursor — single ownership moves into the parser — which is what entitles the
  // buffer-snapshot fields below to hold parameter-derived references.
  private[honeycomb] final class HtmlParser
    ( consume cursor1:  Cursor[Text, ?]^,
      val permissive: Boolean      = false )
    ( using dom: Dom )
  extends caps.ExclusiveCapability, caps.Stateful:
    // A neutral carrier with an inline accessor (the `perihelion.Reader` pattern):
    // an exclusive-typed field would hide the cursor from the parser's own methods.
    private val cursor0: AnyRef = cursor1.asInstanceOf[AnyRef]
    private inline def cursor: Cursor[Text, ?]^ = cursor0.asInstanceOf[Cursor[Text, ?]^]
    private var heldToken: Cursor.Held | Null = null

    type Region = Cursor.Mark

    // ─── parser-local snapshot of the cursor's buffer / position ───────────
    //
    // The cursor remains the source of truth at refill, mark, slice and
    // error points, but for the per-char hot loops (`peek`, `advance`,
    // `more`) the parser maintains its own snapshot of the current buffer
    // reference, read position, and write end. Keeping all three as parser
    // fields rather than re-reading them through cursor accessors on every
    // char lets the JIT keep them in registers across the parser's many
    // tight `@tailrec` scans (tagname, key, value, unquoted, comment,
    // cdata, doctype) and across `lay` / `let` peeks.
    //
    // Invariant: between `syncTo()` and `syncFrom()` calls, `pos` is the
    // authoritative read position; `cursor.unsafePos` is allowed to lag.
    // Whenever a cursor operation that depends on `pos` is performed
    // (refill via `more`'s slow path, mark, slice, error reporting,
    // backtracking via `cue`) the parser pushes `pos` to the cursor
    // first, then refreshes its snapshot from the cursor afterwards —
    // refill may compact the buffer, reallocate it, or reset `pos`.
    // Untracked: the snapshot intentionally aliases the parser-owned cursor's
    // buffer (see the invariant note below). Initialized empty — a field may
    // not derive from a constructor parameter under the provenance rule — and
    // populated by `syncFrom()` at the top of `parseHtml`.
    @caps.unsafe.untrackedCaptures
    private var bytes:  Array[Char] = new Array[Char](0)
    private var pos:    Int = 0
    private var bufEnd: Int = 0

    private inline def syncTo(): Unit =
      cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

    // Non-inline: a field assignment expanded from an inline method falsely
    // trips the separation checker's provenance rule (the lifted expansion
    // binding reads as "parameter x$0"). Called on slow paths only.
    private update def syncFrom(): Unit =
      bytes  = cursor.buffer(using Unsafe)
      pos    = cursor.unsafePos(using Unsafe)
      bufEnd = cursor.unsafeWriteEnd(using Unsafe)

    protected inline def more: Boolean = pos < bufEnd || moreSlow()

    // Out-of-line slow path so `more`'s inline budget stays small enough
    // for the JIT to keep `pos < bufEnd` as one register comparison in
    // hot loops.
    update private def moreSlow(): Boolean =
      syncTo()
      if cursor.more then { syncFrom(); true } else false

    protected inline def peek: Char = bytes(pos)
    protected inline def advance(): Unit = pos += 1

    protected inline def position: Int =
      syncTo()
      cursor.position.n0

    // Non-`inline` so that `cursor.mark`'s expansion is emitted once in
    // its own method rather than re-expanded into every call site,
    // keeping `read` / `tag` / `tagname` small enough for the JIT to
    // pick up. The body is still simple enough for the JIT to inline at
    // hot call sites via its own inlining heuristics.
    protected def begin(): Cursor.Mark =
      syncTo()
      cursor.mark(using heldToken.nn)

    protected def slice(start: Cursor.Mark, end: Cursor.Mark): Text =
      cursor.grab(start, end).asInstanceOf[Text]

    // ASCII-range character predicates and case fold. Lower the per-character
    // overhead from `Character.isLetter`/`Character.isDigit`/`Character.toLowerCase`
    // (Unicode-property table walks) to a couple of integer comparisons. HTML
    // tag and attribute names, decimal/hex digits, and the trie's keys are all
    // strictly ASCII, so this is a sound substitution on every parser hot path
    // that previously called the JDK helpers.
    protected inline def asciiLetter(char: Char): Boolean =
      (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

    protected inline def asciiDigit(char: Char): Boolean = char >= '0' && char <= '9'

    protected inline def asciiLower(char: Char): Char =
      if char >= 'A' && char <= 'Z' then (char + 32).toChar else char

    update protected def reset(start: Cursor.Mark): Unit =
      syncTo()
      cursor.cue(start)
      syncFrom()

    protected def cloneTo
      ( start: Cursor.Mark, end: Cursor.Mark )
      ( target: jl.StringBuilder )
    :   Unit =

      // A stable binding: the inline `cursor` accessor is not a valid prefix for
      // the path-dependent `addressable.Target`.
      val cursor1: Cursor[Text, ?]^ = cursor
      cursor1.clone(start, end)(target.asInstanceOf[cursor1.addressable.Target])

    protected def computePosition
      ( start: Optional[Cursor.Mark] = Unset,
        end:   Optional[Cursor.Mark] = Unset )
    :   Position =

      // The cursor uses untracked lineation in the hot path (see the
      // import at `HtmlParser`). On error, reconstruct (line, column) by
      // scanning the currently-buffered chars from offset 0 to the
      // current read position, counting newlines. Errors are rare, so the
      // O(buffer) cost here doesn't matter; the parser stays tight on the
      // success path. For the loadable path (single-chunk buffer) this is
      // fully accurate. For multi-chunk streaming, lines before the most
      // recent compaction aren't represented in the buffer; we under-
      // count by that amount but absolute `offset` and `length` remain
      // correct, which is what tests assert on.
      syncTo()
      var line = 1
      var col = 0  // counts chars seen on the current line
      var i = 0

      while i < pos do
        if bytes(i) == '\n' then
          line += 1
          col = 0
        else
          col += 1

        i += 1

      // Match the previous behaviour's off-by-one: report the column of the
      // *next* char to read (1-based), or — at end-of-input — the column of
      // the LAST char read.
      val reportCol = (if cursor.more then col + 1 else col).max(1)

      val endPos = end.lay(cursor.position.n0)(_.absolute.toInt)
      val offset: Optional[Int] = start.let(_.absolute.toInt)
      val length: Optional[Int] = start.let: mark => endPos - mark.absolute.toInt
      Position(line.u, reportCol.u, offset = offset, length = length)

    // Optional callback invoked for null-placeholder holes during macro
    // interpolation. Default no-op.
    // Untracked: the macro-expansion callback is installed and invoked only within
    // one `parseHtml` call.
    @caps.unsafe.untrackedCaptures
    // Pure-typed: the (sealed) callback closes only over the macro's hole map.
    var callback: Optional[(Ordinal, Hole) -> Unit] = Unset

    // Cursor-compat helpers so the algorithm body stays close to the
    // original cursor-based code.
    protected inline def lay[result](inline otherwise: => result)(inline body: Char => result)
    :   result =

      if more then body(peek) else otherwise


    protected inline def let(inline body: Char => Unit): Unit =
      if more then body(peek)

    type Mark = Region

    // Render line/column on demand for error messages.
    protected inline def currentPosition(): Position = computePosition()

    protected inline def currentPosition(start: Cursor.Mark): Position = computePosition(start)

    import Issue.*

    // The tactic is a plain using-parameter: a context-function result may not
    // hide `this`.
    update def parseHtml(root: Tag, doctypes: Boolean = false)(using Tactic[ParseError]): Html =
      // The snapshot fields initialize empty (a constructor may not derive field
      // values from its parameters under the provenance rule); sync them here.
      syncFrom()
      cursor.hold:
        heldToken = summon[Cursor.Held]
        try parseHtml0(root, doctypes) finally heldToken = null

    update private def parseHtml0(root: Tag, doctypes: Boolean)(using Tactic[ParseError])
    :   Html =
      val buffer: jl.StringBuilder = jl.StringBuilder()
      def result(): Text = buffer.toString.tt.also(buffer.setLength(0))
      var content: Text = t""
      var extra: Attributes = Attributes.empty
      // Resolved `Tag` for the current opening token. `tag()` already walks
      // `dom.elements` to look up the tag definition; stash the result so that
      // `read`'s `Token.Open` / `Token.Empty` arms don't have to repeat the
      // lookup against `dom.elements(content)`.
      var openTag: Tag = root
      // Shared scratch buffer for attribute accumulation (parser-lifetime).
      // Stores key/value pairs interleaved as `[k0, v0, k1, v1, ...]`. Keys are
      // never null; a null in a value slot encodes `Unset`. `attributes()`
      // writes here, then snapshots the populated prefix into a freshly-sized
      // `IArray[String | Null]` and wraps it as an opaque `Attributes`. Grows
      // geometrically when filled.
      var attrInterleaved: Array[String | Null] = new Array[String | Null](16)
      var nodes: Array[Node] = new Array(4)
      var index: Int = 0
      var stack: Array[Tag] = new Array(4)
      var depth: Int = 0
      var fragment: IArray[Node] = IArray()
      // Pending formatting tags awaiting reconstruction (see WHATWG "active
      // formatting elements"). Stored as parallel arrays of label/Attributes
      // pairs rather than a `List[(Text, Attributes)]`: `:+` on a `List` is
      // O(N) (and allocates a `Tuple2` per append), and even though valid
      // HTML rarely populates this list, malformed-input handling can append
      // multiple entries at once. Capacity grows geometrically when filled.
      var pendingFormattingLabels: Array[Text] = new Array[Text](4)
      var pendingFormattingAttrs:  Array[Attributes] = new Array[Attributes](4)
      var pendingFormattingSize: Int = 0
      var pendingAtDepth: Int = -1
      // Foster-parented children awaiting placement around the next `<table>`
      // close. Stored as a pair of `Array[Node]` buffers (with size counters)
      // rather than `List[Node]`s appended via `:+`: list `:+` is `O(N)`
      // and the per-append cons cell + traversal-on-flush is wasted on a
      // structure we always drain in arrival order.
      var fosteredBefore: Array[Node] = new Array[Node](4)
      var fosteredBeforeSize: Int = 0
      var fosteredAfter: Array[Node] = new Array[Node](4)
      var fosteredAfterSize: Int = 0
      var inTableContent: Boolean = false
      var pendingFosterDescend: Boolean = false

      def findAncestorIndex(label: Text): Int =
        var i = 0
        val end = depth - 1

        while i < end do
          if stack(i).label == label then return i
          i += 1
        -1

      def stackContainsAncestor(label: Text): Boolean = findAncestorIndex(label) >= 0

      def append(node: Node): Unit =
        if index >= nodes.length then
          val nodes2 = new Array[Node](nodes.length*2)
          System.arraycopy(nodes, 0, nodes2, 0, nodes.length)
          nodes = confined(nodes2)

        writable(nodes)(index) = node
        index += 1

      def push(tag: Tag): Unit =
        if depth >= stack.length then
          val stack2 = new Array[Tag](stack.length*2)
          System.arraycopy(stack, 0, stack2, 0, stack.length)
          stack = confined(stack2)

        writable(stack)(depth) = tag
        depth += 1

      def pop(): Unit = depth -= 1

      def next(): Unit =
        advance()
        if !more then raise(ParseError(Html, currentPosition(), ExpectedMore))

      // Non-inline: each `expect`/`expectInsensitive` call site otherwise
      // re-expands `advance` + `lay` + `fail` + `Issue.Unexpected.apply`,
      // which over the doctype/cdata parsers contributes a large chunk of
      // `tag\$8`'s bytecode.
      def expect(char: Char): Unit =
        advance()

        lay(fail(ExpectedMore)): datum =>
          if datum != char then fail(Unexpected(datum))

      def expectInsensitive(char: Char): Unit =
        advance()

        lay(fail(ExpectedMore)): datum =>
          if asciiLower(datum) != asciiLower(char) then fail(Unexpected(datum))

      def fail
        ( issue: Issue,
          start: Optional[Cursor.Mark] = Unset,
          end:   Optional[Cursor.Mark] = Unset )
      :   Nothing =

        abort(ParseError(Html, computePosition(start, end), issue))

      def warn(issue: Issue): Unit = raise(ParseError(Html, currentPosition(), issue))

      // While-loops rather than `@tailrec` over the inline `let`/`lay`
      // combinators: capture checking's beta-reduction of the inline lambdas
      // leaves the recursive calls in non-tail position.
      def skip(): Unit =
        var continue = true

        while continue && more do peek match
          case ' ' | '\f' | '\n' | '\r' | '\t' => advance()
          case _                               => continue = false

      def whitespace(): Unit =
        var continue = true

        while continue && more do peek match
          case ' ' | '\f' | '\n' | '\r' | '\t' => advance()
          case '<'                             => continue = false

          case char =>
            fail(OnlyWhitespace(char))
            continue = false

      @tailrec
      def tagname(mark: Mark, node: Int): Tag =
        lay(fail(ExpectedMore, mark)):
          case char if asciiLetter(char) || asciiDigit(char) =>
            val step = dom.elements.step(node, asciiLower(char))

            if step < 0 then
              advance()
              val end = begin()
              val name = slice(mark, end)
              reset(mark) yet fail(InvalidTagStart(name.lower), mark, end)
            else
              next() yet tagname(mark, step)

          case ' ' | '\f' | '\n' | '\r' | '\t' | '/' | '>' =>
            val tag = dom.elements.value(node)

            if tag != null then tag.nn else
              val end = begin()
              val name = slice(mark, end)
              reset(mark) yet fail(InvalidTag(name), mark, end)

          case '\u0000' =>
            fail(BadInsertion, mark)

          case char =>
            fail(Unexpected(char), mark)

      @tailrec
      def foreignTag(mark: Mark): Text = lay(fail(ExpectedMore, mark)):
        case char if asciiLetter(char)                   => next() yet foreignTag(mark)
        case ' ' | '\f' | '\n' | '\r' | '\t' | '/' | '>' => slice(mark, begin()).lower
        case '\u0000'                                    => fail(BadInsertion, mark)
        case char                                        => fail(Unexpected(char), mark)

      // Permissive recovery for `key`: drain the remaining attribute-name
      // characters (letters, digits, hyphens) so the cursor lands on the
      // terminator, then return a synthetic Attribute that targets every
      // tag — keeping the InvalidAttributeUse check happy.
      @tailrec
      def keyTail(mark: Mark): Attribute =
        lay(Attribute(slice(mark, begin()), Set(), true)):
          case char if asciiLetter(char) || asciiDigit(char) || char == '-' =>
            advance() yet keyTail(mark)

          case _ =>
            Attribute(slice(mark, begin()), Set(), true)

      @tailrec
      def key(mark: Mark, node: Int): Attribute =
        lay(fail(ExpectedMore, mark)):
          case char if asciiLetter(char) || char == '-' =>
            val step = dom.attributes.step(node, asciiLower(char))

            if step < 0 then
              if permissive then
                warn(UnknownAttributeStart(slice(mark, begin())))
                keyTail(mark)
              else
                fail(UnknownAttributeStart(slice(mark, begin())), mark)
            else
              next() yet key(mark, step)

          case ' ' | '\f' | '\n' | '\r' | '\t' | '=' | '>' =>
            val attr = dom.attributes.value(node)

            if attr != null then attr.nn else
              val end = begin()
              val name = slice(mark, end)

              if permissive then
                warn(UnknownAttribute(name))
                Attribute(name, Set(), true)
              else
                reset(mark)
                fail(UnknownAttribute(name), mark, end)

          case char =>
            if permissive then
              warn(Unexpected(char))
              keyTail(mark)
            else
              fail(Unexpected(char), mark)

      @tailrec
      def foreignKey(mark: Mark): Text = lay(fail(ExpectedMore, mark)):
        case char if asciiLetter(char) || char == '-'    => next() yet foreignKey(mark)
        case ' ' | '\f' | '\n' | '\r' | '\t' | '=' | '>' => slice(mark, begin())
        case '\u0000'                                    => fail(BadInsertion, mark)
        case char                                        => fail(Unexpected(char), mark)


      @tailrec
      def value(mark: Mark): Text = lay(fail(ExpectedMore)):
        case '\u0000' => callback.let(_(position.z, Hole.Text)) yet next() yet value(mark)

        case '"' =>
          cloneTo(mark, begin())(buffer)
          next() yet result()

        case '&' =>
          val start = begin()
          next()

          val mark2 = entity(begin()).lay(mark): text =>
            cloneTo(mark, start)(buffer)
            buffer.append(text)
            begin()

          value(mark2)

        case char =>
          next() yet value(mark)

      @tailrec
      def singleQuoted(mark: Mark): Text = lay(fail(ExpectedMore, mark)):
        case '\'' => slice(mark, begin()).also(next())
        case char => next() yet singleQuoted(mark)

      @tailrec
      def unquoted(mark: Mark): Text = lay(fail(ExpectedMore, mark)):
        case '>' | ' ' | '\f' | '\n' | '\r' | '\t' => slice(mark, begin())
        case '\u0000'                              => fail(BadInsertion, mark)

        case char@('"' | '\'' | '<' | '=' | '`') =>
          if permissive then
            warn(ForbiddenUnquoted(char))
            next()
            unquoted(mark)
          else
            fail(ForbiddenUnquoted(char), mark)

        case char                                  => next() yet unquoted(mark)

      def equality(): Boolean = skip() yet lay(fail(ExpectedMore)):
        case '='                                   => next() yet skip() yet true
        case '>' | ' ' | '\f' | '\n' | '\r' | '\t' => false
        case '\u0000'                              => fail(BadInsertion)
        case char                                  => fail(Unexpected(char))


      def attributes(tag: Text, foreign: Boolean): Attributes =
        // Append into the parser-shared interleaved scratch buffer (laid out
        // as `[k0, v0, k1, v1, ...]`); on close, snapshot the populated prefix
        // into a freshly-sized `IArray[String | Null]` and wrap it as the
        // opaque `Attributes`. `Unset` values are encoded as `null` in the
        // array.
        //
        // Duplicate detection uses a Bloom-filter-style cheap test before
        // falling back to a linear scan: maintain a running OR of the
        // hashCodes of all already-stored keys, and for each new key check
        // whether `(hashOr | h) == hashOr`. If the new hash has any bit
        // outside the accumulated envelope it cannot match any prior key
        // and the scan is skipped. Only when its bits are all already in
        // the envelope (rare for typical 0–7-attribute elements with
        // disjoint label hashes) do we walk the existing keys to confirm.
        // The check loses precision as the attribute count grows (the OR
        // eventually saturates), but for the HTML common case this turns
        // O(n^2) string comparisons into O(n) bit ops plus a handful of
        // false positives.
        var n = 0
        var done = false
        var hashOr = 0

        inline def ensureCapacity(): Unit =
          if 2*n >= attrInterleaved.length then
            val nu = new Array[String | Null](attrInterleaved.length*2)
            jl.System.arraycopy(attrInterleaved, 0, nu, 0, 2*n)
            attrInterleaved = confined(nu)

        while !done do
          skip()

          lay(fail(ExpectedMore)):
            case '>' | '/' => done = true

            case '\u0000' =>
              callback.let(_(position.z, Hole.Tagbody))
              next()
              skip()
              ensureCapacity()
              writable(attrInterleaved)(2*n) = "\u0000"
              writable(attrInterleaved)(2*n + 1) = null
              n += 1

            case _ =>
              val key2 = if foreign then foreignKey(begin()) else
                key(begin(), 0).tap: key =>
                  if !key.targets(tag) then
                    if permissive then warn(InvalidAttributeUse(key.label, tag))
                    else fail(InvalidAttributeUse(key.label, tag))

                . label

              val key2Str: String = key2.s
              val h: Int = key2Str.hashCode

              // Bloom-style fast-skip; only fall back to a linear scan when
              // the new hashcode's bits are entirely within the accumulated
              // envelope (i.e. it might match a prior key).
              var isDuplicate = false

              if (hashOr | h) == hashOr then
                var dup = 0

                while dup < 2*n do
                  if attrInterleaved(dup) == key2Str then
                    if permissive then
                      warn(DuplicateAttribute(key2))
                      isDuplicate = true
                      dup = 2*n
                    else
                      fail(DuplicateAttribute(key2))
                  else
                    dup += 2

              hashOr |= h

              val assignment: Optional[Text] =
                if !equality() then Unset else lay(fail(ExpectedMore)):
                  case '"'  => next() yet value(begin())
                  case '\'' => next() yet singleQuoted(begin())

                  case '\u0000' =>
                    callback.let(_(position.z, Hole.Attribute(tag, key2)))
                    next() yet t"\u0000"

                  case _ =>
                    unquoted(begin()) // FIXME: Only alphanumeric characters

              if !isDuplicate then
                ensureCapacity()
                writable(attrInterleaved)(2*n) = key2Str
                writable(attrInterleaved)(2*n + 1) = assignment.lay(null: String | Null)(_.s)
                n += 1

        if n == 0 then Attributes.empty
        else
          val arr = new Array[String | Null](2*n)
          jl.System.arraycopy(attrInterleaved, 0, arr, 0, 2*n)
          Attributes.fromInterleaved(arr.immutable(using Unsafe))


      def entity(mark: Mark): Optional[Text] = lay(fail(ExpectedMore, mark)):
        case '#'   => next() yet numericEntity(mark)
        case other => textEntity(mark, 0)

      def numericEntity(mark: Mark): Optional[Text] =
        lay(fail(ExpectedMore, mark)):
          case 'x' => next() yet hexEntity(mark, 0)
          case _   => decimalEntity(mark, 0)

      @tailrec
      def hexEntity(mark: Mark, value: Int): Optional[Text] =
        lay(fail(ExpectedMore, mark)):
          case digit if asciiDigit(digit) =>
            advance() yet hexEntity(mark, 16*value + (digit - '0'))

          case letter if 'a' <= letter <= 'f' =>
            advance() yet hexEntity(mark, 16*value + (letter - 87))

          case letter if 'A' <= letter <= 'F' =>
            advance() yet hexEntity(mark, 16*value + (letter - 55))

          case ';' =>
            advance() yet value.unicode

          case char =>
            Unset

      @tailrec
      def decimalEntity(mark: Mark, value: Int): Optional[Text] = lay(fail(ExpectedMore, mark)):
        case digit if asciiDigit(digit) => next() yet decimalEntity(mark, 10*value + (digit - '0'))
        case ';'                        => next() yet value.unicode
        case char                       => Unset

      @tailrec
      def textEntity(mark: Mark, node: Int): Optional[Text] =
        lay(fail(ExpectedMore, mark)):
          case char if asciiLetter(char) || asciiDigit(char) =>
            val step = dom.entities.step(node, char)

            if step < 0 then Unset
            else advance() yet textEntity(mark, step)

          case ';' =>
            advance()
            val step = dom.entities.step(node, ';')

            if step < 0 then Unset else
              val v = dom.entities.value(step)
              if v == null then Unset else v.nn

          case '=' =>
            Unset

          case '\u0000' =>
            fail(BadInsertion, mark)

          case char =>
            val v = dom.entities.value(node)
            if v == null then Unset else v.nn


      // Slow path: an entity reference or RCDATA close-tag check forced us to
      // switch to the buffer. Identical to the original textual() body.
      @tailrec
      def textualSlow(mark: Mark, close: Optional[Text], entities: Boolean): Text =
        lay(cloneTo(mark, begin())(buffer) yet result()):
          case '<' | '\u0000' =>
            close.lay(cloneTo(mark, begin())(buffer) yet result()): tag =>
              val end = begin()
              advance()
              val resume = begin()

              if lay(false)(_ == '/') then
                next()
                val tagStart = begin()
                repeat(tag.length)(advance())
                val candidate = slice(tagStart, begin())

                if more && candidate == tag then
                  if lay(false)(_ == '>')
                  then
                    cloneTo(mark, end)(buffer) yet result().also(advance())
                  else
                    reset(resume) yet textualSlow(mark, tag, entities)
                else
                  reset(resume) yet textualSlow(mark, tag, entities)
              else
                reset(resume) yet textualSlow(mark, tag, entities)

          case '&' if entities =>
            val start = begin()
            next()

            val mark2 = entity(begin()).lay(mark): text =>
              cloneTo(mark, start)(buffer)
              buffer.append(text)
              begin()

            textualSlow(mark2, close, entities)

          case char =>
            advance() yet textualSlow(mark, close, entities)

      // Fast path: scan the parser-local buffer with a register-resident
      // `var p`. While the scan stays inside the loaded buffer, hits no
      // entity reference, and `close` is absent (no RCDATA close-tag check
      // needed), the result comes from a single `String.substring` slice
      // without round-tripping through `buffer`. Falls back to
      // `textualSlow` for the harder cases.
      //
      // Uses the parser-level snapshot (`bytes` / `pos` / `bufEnd`)
      // rather than re-snapshotting from the cursor on each `fast()`
      // call, so the JIT can keep all three in registers across the
      // whole text-node walk. After scanning, `pos` already holds the
      // new position — no `cursor.unsafeBumpPos` reconciliation needed.
      def textual(mark: Mark, close: Optional[Text], entities: Boolean): Text =
        if close.present then textualSlow(mark, close, entities) else
          @tailrec
          def fast(): Text =
            if !more then slice(mark, begin())
            else
              var p = pos
              val limit = bufEnd
              var hit: Char = 1.toChar // sentinel: "no delimiter found"

              while p < limit && hit == 1.toChar do
                val c = bytes(p)

                if c == '<' || c == 0.toChar then hit = c
                else if c == '&' && entities then hit = c
                else p += 1

              pos = p

              if hit == '&' then textualSlow(mark, close, entities)
              else if hit != 1.toChar then
                slice(mark, begin())
              else
                fast() // buffer exhausted; outer @tailrec re-checks via `more`

          fast()


      def comment(mark: Mark): Text = lay(fail(ExpectedMore)):
        case '-' =>
          val end = begin()
          next()

          lay(fail(ExpectedMore)):
            case '-' => expect('>') yet slice(mark, end)
            case _   => comment(mark)

        case '\u0000' =>
          callback.let(_(position.z, Hole.Comment))
          next() yet comment(mark)

        case char =>
          next() yet comment(mark)

      def cdata(mark: Mark): Text = lay(fail(ExpectedMore, mark)):
        case ']' =>
          val end = begin()
          next()

          lay(fail(ExpectedMore, mark)):
            case ']' => expect('>') yet slice(mark, end)
            case _   => cdata(mark)

        case char =>
          next() yet cdata(mark)

      def doctype(mark: Mark): Text = lay(fail(ExpectedMore, mark)):
        case '>'   => slice(mark, begin()).also(next())
        case other => next() yet doctype(mark)

      def tag(doctypes: Boolean, foreign: Boolean): Token = lay(fail(ExpectedMore)):
        case '!' =>
          next()

          lay(fail(ExpectedMore)):
            case '-' =>
              expect('-')
              next()
              content = comment(begin())
              advance()
              Token.Comment

            case '[' =>
              expectInsensitive('c')
              expectInsensitive('d')
              expectInsensitive('a')
              expectInsensitive('t')
              expectInsensitive('a')
              expect('[')
              next()
              content = cdata(begin())
              advance()
              Token.Cdata

            case 'D' | 'd' if doctypes =>
              expectInsensitive('o')
              expectInsensitive('c')
              expectInsensitive('t')
              expectInsensitive('y')
              expectInsensitive('p')
              expectInsensitive('e')
              next()
              skip()
              content = doctype(begin())
              skip()
              Token.Doctype

            case char =>
              fail(Unexpected(char))

        case '/' =>
          next()

          if foreign then content = foreignTag(begin())
          else
            val tagDef = tagname(begin(), 0)
            content = tagDef.label
            // Stash the resolved closing tag so `read`'s `Token.Close` arm
            // can compare `openTag eq parent` (reference equality on the
            // interned `Tag` instance) instead of `content != parent.label`
            // (`String.equals` on the labels).
            openTag = tagDef

          Token.Close

        case '\u0000' => fail(BadInsertion)

        case char =>
          val newForeign: Boolean =
            if foreign then
              content = foreignTag(begin())
              true
            else
              val tagDef = tagname(begin(), 0)
              content = tagDef.label
              openTag = tagDef
              tagDef.foreign

          extra = attributes(content, foreign || newForeign)

          lay(fail(ExpectedMore)):
            case '/'       => expect('>') yet advance() yet Token.Empty
            case '>'       => advance() yet Token.Open
            case '\u0000'  => fail(BadInsertion)
            case char      => fail(Unexpected(char))

      def finish(parent: Tag, map: Attributes, count: Int): Node =
        if parent != root then
          if parent.autoclose then Element(parent.label, parent.attributes, array(count), false)
          else if permissive then
            warn(Incomplete(parent.label))
            Element(parent.label, map, array(count), parent.foreign)
          else
            fail(Incomplete(parent.label))
        else
          if count > 1 then fragment = array(count)
          nodes(index - 1)

      def array(count: Int): IArray[Node] =
        val result = new Array[Node](count)
        System.arraycopy(nodes, 0.max(index - count), result, 0, count)
        index -= count
        result.immutable(using Unsafe)

      def descend(parent: Tag, admissible: Set[Text], attrs: Attributes): Node =
        val admissible2 = if parent.transparent then admissible else parent.admissible
        read(parent, admissible2, attrs, 0)

      @tailrec
      def read(parent: Tag, admissible: Set[Text], map: Attributes, count: Int): Node =

        def admit(child: Text): Boolean =
          parent.foreign || parent.admissible(child) || parent.transparent && admissible(child)

        lay(finish(parent, map, count)):
          case '\u0000' =>
            callback.let(_(position.z, Hole.Node(parent.label)))
            next()
            append(TextNode("\u0000"))
            read(parent, admissible, map, count + 1)

          case '<' if parent.mode != Mode.Raw && parent.mode != Mode.Rcdata =>
            var level: Level = Level.Peer
            var current: Node = parent
            var focus: Tag = parent

            locally:
              val mark = begin()

              inline def node(): Unit =
                current = Element(content, extra, array(count), parent.foreign)

              inline def empty(): Unit =
                current = Element(content, extra, caps.unsafe.unsafeAssumePure(IArray()), parent.foreign)

              inline def close(): Unit =
                current = Element(parent.label, map, array(count), parent.foreign)
                level = Level.Ascend

              inline def infer(inline tag: Tag): Unit =
                reset(mark)
                // Use a direct `if inferred.absent` check rather than
                // `dom.infer(...).let { ... }.or { ... }`: `Optional.let` is
                // not inline, so the lambda body would capture the surrounding
                // `focus` and `level` `var`s as `ObjectRef`s on every `<`
                // entry, allocating two refs per element open.
                val inferred: Optional[Tag] = dom.infer(parent, tag)

                if inferred.absent then
                  if parent.autoclose then close()
                  else if permissive then
                    warn(InadmissibleTag(content, parent.label))
                    // At root, just accept the empty tag; otherwise auto-close
                    // the parent and let the tag retry at the grandparent.
                    if parent == root then empty() else close()
                  else
                    fail(InadmissibleTag(content, parent.label), mark)
                else
                  focus = inferred.vouch
                  level = Level.Descend

              next()

              if lay(false)(_ == '\u0000') then
                callback.let(_(position.z, Hole.Element(parent.label)))
                content = t"\u0000"
                node()
                expect('>')
                next()
              else
                tag(doctypes && parent == root, parent.foreign) match
                case Token.Comment => current = Comment(content)
                case Token.Doctype => current = Doctype(content)

                case Token.Cdata =>
                  current =
                    if parent.foreign then TextNode(content)
                    else if permissive then
                      warn(InvalidCdata)
                      Comment(t"[CDATA[${content}]]")
                    else
                      fail(InvalidCdata, mark)

                case Token.Empty =>
                  if admit(content) then empty() else infer:
                    if parent.foreign then Tag.foreign(content, extra) else openTag

                case Token.Open =>
                  focus = if parent.foreign then Tag.foreign(content, extra) else openTag

                  if !admit(content) then
                    val inferred = dom.infer(parent, focus)

                    if inferred.absent then
                      if parent.autoclose then
                        reset(mark)
                        close()
                      else if parent.tableLike && !focus.void then
                        pendingFosterDescend = true
                        level = Level.Descend
                      else if permissive then
                        warn(InadmissibleTag(content, parent.label))
                        // At root, descend anyway; otherwise auto-close the
                        // parent and retry the open tag at the grandparent.
                        if parent == root then
                          level = Level.Descend
                        else
                          reset(mark)
                          close()
                      else
                        reset(mark)
                        fail(InadmissibleTag(content, parent.label), mark)
                    else
                      reset(mark)
                      focus = inferred.vouch
                      level = Level.Descend
                  else if focus.void then
                    empty()
                  else if (content == t"a" || content == t"nobr") &&
                    (parent.label == content || stackContainsAncestor(content)) then
                    reset(mark)
                    close()
                  else
                    level = Level.Descend

                case Token.Close =>
                  // For non-foreign tags, `tagname` returns a `Tag` instance
                  // from the interned `dom.elements` trie and `parent` is
                  // itself one of those interned instances, so a mismatch is
                  // detectable via reference inequality — cheaper than
                  // `String.equals` on the labels. Foreign tags have to fall
                  // back to text equality since `Tag.foreign` mints fresh
                  // instances per element.
                  val nameMismatch =
                    if parent.foreign then content != parent.label
                    else openTag ne parent

                  if nameMismatch then
                    if parent.autoclose then
                      reset(mark)
                      close()
                    else if stackContainsAncestor(content) then
                      if formattingTags.has(parent.label) then
                        pendingAtDepth = findAncestorIndex(content)

                        if pendingFormattingSize >= pendingFormattingLabels.length then
                          val newCap = pendingFormattingLabels.length*2
                          val nl = new Array[Text](newCap)
                          // Confined at birth: an exclusive-typed local would be
                          // hidden from the arraycopy below.
                          val na = confined(new Array[Attributes](newCap))
                          val sz = pendingFormattingSize
                          jl.System.arraycopy(pendingFormattingLabels, 0, nl, 0, sz)
                          jl.System.arraycopy(pendingFormattingAttrs, 0, na, 0, sz)
                          pendingFormattingLabels = confined(nl)
                          pendingFormattingAttrs  = na

                        writable(pendingFormattingLabels)(pendingFormattingSize) = parent.label
                        writable(pendingFormattingAttrs)(pendingFormattingSize) = map
                        pendingFormattingSize += 1

                      reset(mark)
                      close()
                    else if formattingTags.has(content) then
                      advance()
                      level = Level.Skip
                    else if permissive then
                      if parent == root then warn(UnopenedTag(content))
                      else warn(MismatchedTag(parent.label, content))

                      advance()
                      level = Level.Skip
                    else
                      reset(mark)

                      if parent == root then fail(UnopenedTag(content), mark)
                      else fail(MismatchedTag(parent.label, content), mark)
                  else
                    advance()
                    level = Level.Ascend
                    current = Element(content, map, array(count), parent.foreign)

            def reconstructPending(): Int =
              if pendingFormattingSize == 0 || depth != pendingAtDepth then 0
              else if !more || lay(false)(_ == '<') then
                pendingFormattingSize = 0
                pendingAtDepth = -1
                0
              else
                val pendingCount = pendingFormattingSize
                pendingFormattingSize = 0
                pendingAtDepth = -1
                var added = 0
                var i = 0

                while i < pendingCount do
                  val label = pendingFormattingLabels(i)
                  val attrs = pendingFormattingAttrs(i)
                  val cloneTag: Optional[Tag] = dom.elements(label)

                  if cloneTag.present then
                    val tag = cloneTag.vouch
                    push(tag)
                    val cloneChild = descend(tag, admissible, attrs)
                    pop()

                    cloneChild match
                      case Element(_, _, children, _) if children.length == 0 => ()

                      case _ =>
                        append(cloneChild)
                        added += 1

                  i += 1

                added

            level match
              case Level.Ascend =>
                current

              case Level.Skip =>
                read(parent, admissible, map, count)

              case Level.Peer =>
                append(current)
                val added = reconstructPending()
                read(parent, admissible, map, count + 1 + added)

              case Level.Descend =>
                push(focus)
                val savedFosterFlag = pendingFosterDescend
                pendingFosterDescend = false
                if parent.isTable && !savedFosterFlag then inTableContent = true
                val child = descend(focus, admissible, extra)
                pop()

                if savedFosterFlag then
                  if inTableContent then
                    if fosteredAfterSize >= fosteredAfter.length then
                      val nu = new Array[Node](fosteredAfter.length*2)
                      jl.System.arraycopy(fosteredAfter, 0, nu, 0, fosteredAfterSize)
                      fosteredAfter = confined(nu)

                    writable(fosteredAfter)(fosteredAfterSize) = child
                    fosteredAfterSize += 1
                  else
                    if fosteredBeforeSize >= fosteredBefore.length then
                      val nu = new Array[Node](fosteredBefore.length*2)
                      jl.System.arraycopy(fosteredBefore, 0, nu, 0, fosteredBeforeSize)
                      fosteredBefore = confined(nu)

                    writable(fosteredBefore)(fosteredBeforeSize) = child
                    fosteredBeforeSize += 1

                  val added = reconstructPending()
                  read(parent, admissible, map, count + added)
                else if focus.isTable then
                  val beforeAdded = fosteredBeforeSize
                  var i = 0

                  while i < fosteredBeforeSize do
                    append(fosteredBefore(i))
                    i += 1

                  fosteredBeforeSize = 0
                  append(child)
                  val afterAdded = fosteredAfterSize
                  i = 0

                  while i < fosteredAfterSize do
                    append(fosteredAfter(i))
                    i += 1

                  fosteredAfterSize = 0
                  inTableContent = false
                  val added = reconstructPending()
                  read(parent, admissible, map, count + 1 + beforeAdded + afterAdded + added)
                else
                  append(child)
                  val added = reconstructPending()
                  read(parent, admissible, map, count + 1 + added)

          case char => parent.mode match
            case Mode.Whitespace =>
              if parent.tableLike then
                val text = textual(begin(), Unset, true)
                val trimmed = text.trim

                if trimmed.length > 0 then
                  val node = TextNode(trimmed)

                  if inTableContent then
                    if fosteredAfterSize >= fosteredAfter.length then
                      val nu = new Array[Node](fosteredAfter.length*2)
                      jl.System.arraycopy(fosteredAfter, 0, nu, 0, fosteredAfterSize)
                      fosteredAfter = confined(nu)

                    writable(fosteredAfter)(fosteredAfterSize) = node
                    fosteredAfterSize += 1
                  else
                    if fosteredBeforeSize >= fosteredBefore.length then
                      val nu = new Array[Node](fosteredBefore.length*2)
                      jl.System.arraycopy(fosteredBefore, 0, nu, 0, fosteredBeforeSize)
                      fosteredBefore = confined(nu)

                    writable(fosteredBefore)(fosteredBeforeSize) = node
                    fosteredBeforeSize += 1

                read(parent, admissible, map, count)
              else
                whitespace() yet read(parent, admissible, map, count)

            case Mode.Raw =>
              val text = textual(begin(), parent.label, false)

              if text.nil
              then Element(parent.label, parent.attributes, IArray(), parent.foreign)
              else
                Element(parent.label, parent.attributes,
                  caps.unsafe.unsafeAssumePure(IArray(TextNode(text))), parent.foreign)

            case Mode.Rcdata =>
              val text = textual(begin(), parent.label, true)

              if text.nil
              then Element(parent.label, parent.attributes, IArray(), parent.foreign)
              else
                Element(parent.label, parent.attributes,
                  caps.unsafe.unsafeAssumePure(IArray(TextNode(text))), parent.foreign)

            case Mode.Normal =>
              val text = textual(begin(), Unset, true)

              if text.length == 0 then read(parent, admissible, map, count + 1)
              else append(TextNode(text)) yet read(parent, admissible, map, count + 1)

      if !more then Fragment() else locally:
        skip()

        if !more then Fragment() else
          append(root)
          val head = read(root, root.admissible, Attributes.empty, 0)
          // Permissive recovery can drain the root with no surviving children
          // (e.g. input was all stray close tags), in which case `finish`
          // returns the root sentinel that was appended before `read`. Yield
          // an empty Fragment in that case rather than leaking the sentinel.
          if fragment.nil then if head eq root then Fragment() else head
          else Fragment(fragment*)


  // ───────────────────────────────────────────────────────────────────────
  // Public entry points.

  private[honeycomb] def parse[dom <: Dom]
    ( input:       Iterator[Text],
      root:        Tag,
      callback:    Optional[(Ordinal, Hole) => Unit] = Unset,
      fastforward: Int                               = 0,
      doctypes:    Boolean                           = false )
    ( using dom: Dom )
    ( using Tactic[ParseError] )
  :   Html =

    val parser = HtmlParser.fromIterator(input, permissive = false)
    // Sealed into the untracked field: the callback lives only for this parse.
    parser.callback = caps.unsafe.unsafeAssumePure(callback)
    parser.parseHtml(root, doctypes)

sealed into trait Html extends Topical, Documentary, Formal:
  type Topic <: Label
  type Transport <: Label
  type Metadata = Dom
  type Chunks = Text
  type Form <: Dom

  private[honeycomb] def of[topic <: Label]: this.type of topic = asInstanceOf[this.type of topic]
  private[honeycomb] def in[form]: this.type in form = asInstanceOf[this.type in form]

  private[honeycomb] def over[transport <: Label]: this.type over transport =
    asInstanceOf[this.type over transport]

  def / (tag: Tag): Fragment of tag.Topic in tag.Form = Fragment().of[tag.Topic].in[tag.Form]
  def body: Fragment of Topic over Transport in Form

sealed trait Node extends Html

case class Comment(text: Text) extends Node:
  override def equals(that: Any): Boolean = that match
    case Comment(text0)           => text0 == text
    case Fragment(Comment(text0)) => text0 == text
    case _                        => false

  def body: Fragment of Topic over Transport in Form = Fragment[Topic]().over[Transport].in[Form]
  override def toString(): String = this.show.s

case class TextNode(text: Text) extends Node:
  type Topic = "#text"

  override def toString(): String = this.show.s

  override def equals(that: Any): Boolean = that match
    case Fragment(textual: TextNode) => this == textual
    case TextNode(text0)             => text0 == text
    case _                           => false

  def body: Fragment of Topic over Transport in Form = Fragment[Topic]().over[Transport].in[Form]

object Element:
  def foreign(label: Text, attributes: Attributes, children: Html of "#foreign"*)
  :   Element of "#foreign" =

    Element(label, attributes, children.nodes, true).of["#foreign"]

  // Convenience for callers that still hold a Map.
  def foreign(label: Text, attributes: Map[Text, Optional[Text]], children: Html of "#foreign"*)
  :   Element of "#foreign" =

    Element(label, Attributes.from(attributes), children.nodes, true).of["#foreign"]

case class Element
  ( label:      Text,
    attributes: Attributes,
    children:   IArray[Node],
    foreign:    Boolean )
extends Node, Topical, Transportive, Dynamic:
  override def toString(): String = this.show.s

  override def / (tag: Tag): Fragment of tag.Topic in tag.Form =
    val children2 = children.collect:
      case element@Element(tag.label, _, _, _) => element.of[tag.Topic].in[tag.Form]

    // Cast-erased: the vararg splat wants an exclusive array of the refined
    // element type; the per-element decorations defeat an outer seal.
    Fragment[tag.Topic]
      ( caps.unsafe.unsafeAssumePure
          (children2.mutable(using Unsafe).asInstanceOf[Array[(Element of tag.Topic) { type Form = tag.Form }]])* )
    . in[tag.Form]

  def body: Fragment of Topic over Transport in Form =
    Fragment[Topic](children.map(_.of[Topic]).asInstanceOf[IArray[Node of Topic]]*)
    . over[Transport].in[Form]

  def ^+ (html: Html of Transport): Element of Topic over Transport in Form =
    (html: Html).match
      case fragment: Fragment =>
        Element
          ( label, attributes,
            (IArray.from(fragment.nodes).asInstanceOf[IArray[Node]] ++ children)
            . asInstanceOf[IArray[Node]],
            foreign )

      case node: Node =>
        Element(label, attributes, caps.unsafe.unsafeAssumePure(node +: children), foreign)

    . of[Topic]
    . over[Transport]
    . in[Form]

  def +^ (html: Html of Transport): Element of Topic over Transport in Form =
    (html: Html).match
      case fragment: Fragment =>
        Element(label, attributes, caps.unsafe.unsafeAssumePure(children ++ fragment.nodes), foreign)

      case node: Node =>
        Element(label, attributes, caps.unsafe.unsafeAssumePure(children :+ node), foreign)

    . of[Topic]
    . over[Transport]
    . in[Form]

  override def equals(that: Any): Boolean = that match
    case Fragment(node: Element) => this == node

    case Element(label, attributes, children, foreign) =>
      label == this.label && attributes.equalsAttributes(this.attributes) &&
        foreign == this.foreign &&
        ju.Arrays.equals(children.mutable(using Unsafe), this.children.mutable(using Unsafe))

    case _ =>
      false

  override def hashCode: Int =
    ju.Arrays.hashCode(children.mutable(using Unsafe)) ^ attributes.hashAttributes ^ label.hashCode

  transparent inline def selectDynamic(name: Label): Any =

    compiletime.summonFrom:
      case attribute: (name.type is Attribute on (? >: Topic) in Form) =>
        compiletime.summonFrom:
          case unattributive: (attribute.Topic is Unattributive) =>
            unattributive.unattribute(attributes(name.tt))

      case attribute: (name.type is Attribute in Form) =>
        compiletime.summonFrom:
          case unattributive: (attribute.Topic is Unattributive) =>
            unattributive.unattribute(attributes(name.tt))


  inline def updateDynamic[value](name: Label)(value: value)
  :   Element of Topic over Transport in Form =

    compiletime.summonFrom:
      case attribute: (name.type is Attribute on (? >: Topic) in Form) =>
        val attributive = infer[value is Attributive to attribute.Topic]

        attributive.attribute(name, value).match
          case Unset => Element(label, attributes.removed(name.tt), children, foreign)

          case pair =>
            val (key, value) = pair.asInstanceOf[(Text, Optional[Text])]
            Element(label, attributes.updated(key, value), children, foreign)

        . of[Topic]
        . over[Transport]
        . in[Form]

      case attribute: (name.type is Attribute in Form) =>
        val attributive = infer[value is Attributive to attribute.Topic]

        attributive.attribute(name, value).match
          case Unset => Element(label, attributes.removed(name.tt), children, foreign)

          case pair =>
            val (key, value) = pair.asInstanceOf[(Text, Optional[Text])]
            Element(label, attributes.updated(key, value), children, foreign)

        . of[Topic]
        . over[Transport]
        . in[Form]

object Fragment:
  @targetName("make")
  def apply[topic <: Label](nodes: Html of (? <: topic)*): Fragment of topic =
    new Fragment(nodes.nodes*).of[topic]

case class Fragment(nodes: Node*) extends Html:
  override def hashCode: Int = if nodes.length == 1 then nodes(0).hashCode else nodes.hashCode
  override def toString(): String = this.show.s

  override def equals(that: Any): Boolean = that match
    case Fragment(nodes0*) => nodes0 == nodes
    case node: Html        => nodes.length == 1 && nodes(0) == node
    case _                 => false

  override def / (tag: Tag): Fragment of tag.Topic in tag.Form =
    Fragment(nodes.flatMap { html => (html / tag).nodes }*).of[tag.Topic].in[tag.Form]

  def body: Fragment of Topic over Transport in Form = this

case class Doctype(text: Text) extends Node:
  override def equals(that: Any): Boolean = that match
    case Doctype(text0)           => text0 == text
    case Fragment(Doctype(text0)) => text0 == text
    case _                        => false

  def body: Fragment of Topic over Transport in Form = Fragment[Topic]().over[Transport].in[Form]

