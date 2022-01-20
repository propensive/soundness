package escapade

import gossamer.*
import rudiments.*
import iridescence.*
import contextual.*

opaque type Span = Long

object Span:
  def apply(start: Int, end: Int): Span = (start.toLong << 32) + end
  given Ordering[Span] = Ordering.Long.on[Span](identity(_))

extension (span: Span)
  def start: Int = (span >> 32).toInt
  def end: Int = span.toInt
  def isEmpty: Boolean = start == end
  def trimLeft(n: Int): Span = Span((start - n) max 0, (end - n) max 0)
  def takeLeft(n: Int): Span = Span(start min n, end min n)
  def shift(n: Int): Span = Span(start + n, end + n)

case class TextStyle(fg: Srgb = colors.White, bg: Option[Srgb] = None, italic: Boolean = false,
                         bold: Boolean = false, reverse: Boolean = false, underline: Boolean = false,
                         conceal: Boolean = false, strike: Boolean = false):
  import escapes.*
  val esc = 27.toChar
  
  private def italicEsc: Text = if italic then styles.Italic.on else styles.Italic.off
  private def boldEsc: Text = if bold then styles.Bold.on else styles.Bold.off
  private def reverseEsc: Text = if reverse then styles.Reverse.on else styles.Reverse.off
  private def underlineEsc: Text = if underline then styles.Underline.on else styles.Underline.off
  private def concealEsc: Text = if conceal then styles.Conceal.on else styles.Conceal.off
  private def strikeEsc: Text = if strike then styles.Strike.on else styles.Strike.off
  
  def changes(next: TextStyle): Text = List(
    if fg != next.fg then next.fg.ansiFg24 else t"",
    if bg != next.bg then next.bg.map(_.ansiBg24).getOrElse(t"$esc[49m") else t"",
    if italic != next.italic then t"${esc}${next.italicEsc}" else t"",
    if bold != next.bold then t"${esc}${next.boldEsc}" else t"",
    if reverse != next.reverse then t"${esc}${next.reverseEsc}" else t"",
    if underline != next.underline then t"${esc}${next.underlineEsc}" else t"",
    if conceal != next.conceal then t"${esc}${next.concealEsc}" else t"",
    if strike != next.strike then t"${esc}${next.strikeEsc}" else t""
  ).join

object rendering:
  given plain: Show[AnsiText] = _.plain
  given ansi: Show[AnsiText] = _.render
  
object Stylize:
  def apply(fn: TextStyle => TextStyle): Ansi.Input.Markup = Ansi.Input.Markup(fn)

object Ansi:
  type Transform = TextStyle => TextStyle

  def strip(txt: Text): Text = txt.sub(t"""\e\\[?.*?[\\@-~]""", t"")

  given Substitution[Ansi.Input, Text, "t"] = str => Ansi.Input.Textual(AnsiText(str))
  given Substitution[Ansi.Input, String, "t"] = str => Ansi.Input.Textual(AnsiText(Text(str)))
  
  given [T: Show]: Substitution[Ansi.Input, T, "t"] =
    value => Ansi.Input.Textual(AnsiText(summon[Show[T]].show(value)))
  
  given [T: AnsiShow]: Substitution[Ansi.Input, T, "t"] =
    value => Ansi.Input.Textual(summon[AnsiShow[T]].ansiShow(value))
  
  given Stylize[Escape] = identity(_)
  given Stylize[Color] = color => Stylize(_.copy(fg = color.standardSrgb))
  given Stylize[Bg] = bgColor => Stylize(_.copy(bg = Some(bgColor.color.standardSrgb)))

  given Stylize[Bold.type] = _ => Stylize(_.copy(bold = true))
  given Stylize[Italic.type] = _ => Stylize(_.copy(italic = true))
  given Stylize[Underline.type] = _ => Stylize(_.copy(underline = true))
  given Stylize[Strike.type] = _ => Stylize(_.copy(strike = true))
  given Stylize[Conceal.type] = _ => Stylize(_.copy(conceal = true))
  given Stylize[Reverse.type] = _ => Stylize(_.copy(reverse = true))
  
  enum Input:
    case Textual(text: AnsiText)
    case Markup(transform: Transform)
    case Escape(on: Text, off: Text)

  case class Frame(bracket: Char, start: Int, transform: Transform)
  
  case class State(text: Text = t"", last: Option[Transform] = None, stack: List[Frame] = Nil,
                       spans: TreeMap[Span, Transform] = TreeMap())

  object Interpolator extends contextual.Interpolator[Input, State, AnsiText]:
    private val complement = Map('[' -> ']', '(' -> ')', '{' -> '}', '<' -> '>', '«' -> '»')
    def initial: State = State()

    def parse(state: State, text: Text): State =
      state.last.fold(closures(state, text)):
        transform => safely(text(0)) match
          case '\\' =>
            closures(state, text.drop(1))
          case '[' | '(' | '<' | '«' | '{' =>
            val frame = Frame(unsafely(complement(text(0))), state.text.length, transform)
            closures(state.copy(stack = frame :: state.stack, last = None), text.drop(1))
  
          case _ =>
            closures(state, text)

    private def closures(state: State, text: Text): State =
      state.stack.headOption.fold(state.copy(text = state.text+text)):
        frame =>
          safely(text.where(_ == frame.bracket)) match
            case Unset =>
              state.copy(text = state.text+text)
            
            case idx: Int =>
              val newSpan: Span = Span(frame.start, state.text.length + idx)
              val spans: TreeMap[Span, Transform] = state.spans.updated(newSpan, frame.transform)
              val newText = state.text+text.take(idx)
              closures(State(newText, None, state.stack.tail, spans), text.drop(idx + 1))

    def insert(state: State, value: Input): State = value match
      case Input.Textual(text) =>
        val spans: TreeMap[Span, Transform] = state.spans.map:
          case (span, transform) => (span.shift(state.text.length): Span) -> transform

        State(state.text+text.plain, None, state.stack, spans)
      
      case Input.Markup(transform) =>
        state.copy(last = Some(transform))
    
      case Input.Escape(on, off) =>
        State(state.text+t"FIXME", None, state.stack, state.spans)
    
    def skip(state: State): State = insert(state, Input.Textual(AnsiText.empty))
    
    def complete(state: State): AnsiText =
      if !state.stack.isEmpty then throw InterpolationError(t"mismatched closing brace")

      AnsiText(state.text, state.spans)

object AnsiText:
  def empty: AnsiText = AnsiText(t"")
  given Joinable[AnsiText] = _.fold(empty)(_ + _)
  
  def make[T: Show](value: T, transform: Ansi.Transform): AnsiText =
    val str: Text = value.show
    AnsiText(str, TreeMap(Span(0, str.length) -> transform))

case class AnsiText(plain: Text, spans: TreeMap[Span, Ansi.Transform] = TreeMap()):
  def length: Int = plain.length
  def span(n: Int): AnsiText = take(n).padTo(n)
  def explicit: Text = render.flatMap { ch => if ch.toInt == 27 then t"\\e" else ch.show }
  def upper: AnsiText = AnsiText(plain.upper, spans)
  def lower: AnsiText = AnsiText(plain.lower, spans)

  @targetName("times")
  infix def *(n: Int): AnsiText = if n == 0 then AnsiText.empty else this*(n - 1)+this
  
  @targetName("add")
  infix def +(text: Text): AnsiText = AnsiText(t"$plain$text", spans)

  infix def +(text: AnsiText): AnsiText =
    val newSpans: TreeMap[Span, Ansi.Transform] = text.spans.map:
      case (span, transform) => (span.shift(length): Span) -> transform
    
    AnsiText(plain+text.plain, spans ++ newSpans)

  def drop(n: Int): AnsiText =
    val newSpans: TreeMap[Span, Ansi.Transform] =
      spans.map:
        case (span, transform) => (span.trimLeft(n): Span, transform)
      .filter(!_(0).isEmpty)
    
    AnsiText(plain.drop(n), newSpans)

  def take(n: Int): AnsiText =
    val newSpans: TreeMap[Span, Ansi.Transform] =
      spans.map:
        case (span, transform) => (span.takeLeft(n): Span, transform)
      .filter(!_(0).isEmpty)
    
    AnsiText(plain.take(n), newSpans)

  def padTo(n: Int, char: Char = ' ') =
    if length < n then this + AnsiText(char.show*(n - length)) else this

  def render: Text =
    val buf = StringBuilder()
    
    def recur(spans: TreeMap[Span, Ansi.Transform], pos: Int = 0, style: TextStyle = TextStyle(),
                  stack: List[(Span, TextStyle)] = Nil): Text =
      if spans.isEmpty then
        buf.add(plain.slice(pos, plain.length))
        buf.text
      else if stack.isEmpty || spans.head(0).start <= stack.head(0).end then
        val newStyle = spans.head(1)(style)
        buf.add(style.changes(newStyle))
        buf.add(plain.slice(pos, spans.head(0).start))
        recur(spans.tail, spans.head(0).start, newStyle, spans.head(0) -> style :: stack)
      else
        buf.add(style.changes(stack.head(1)))
        buf.add(plain.slice(pos, stack.head(0).end))
        recur(spans, stack.head(0).end, stack.head(1), stack.tail)

    recur(spans)

object Bold
object Italic
object Underline
object Strike
object Reverse
object Conceal
case class Bg(color: Color)

type Stylize[T] = Substitution[Ansi.Input, T, "esc"]