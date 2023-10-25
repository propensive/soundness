package profanity

import rudiments.*
import gossamer.*
import spectacular.*
import parasite.*
import turbulence.*
import anticipation.*
import eucalyptus.*

object LineEditor:
  def concealed(str: Text): Text = str.mapChars { _ => '*' }

  def ask(initial: Text = t"", render: Text => Text = identity(_))(using Terminal, Monitor): Text =
    Io.print(render(initial))
    
    def finished(key: Keypress | Signal) =
      key == Keypress.Enter || key == Keypress.Ctrl('D') || key == Keypress.Ctrl('C')
    
    summon[Terminal].in.takeWhile(!finished(_)).foldLeft(LineEditor(initial, initial.length)):
      case (editor, next) =>
        if editor.pos > 0 then Io.print(t"\e${editor.pos}D")
        val editor2 = editor(next)
        val line = t"${editor2.content}${t" "*(editor.content.length - editor2.content.length)}"
        Io.print(t"\e0K")
        Io.print(render(line))
        if line.length > 0 then Io.print(t"\e${line.length}D")
        if editor2.pos > 0 then Io.print(t"\e${editor2.pos}C")
        editor2
    .content

case class LineEditor(content: Text = t"", pos: Int = 0):
  import Keypress.*

  def apply(keypress: Keypress | Signal): LineEditor = try keypress match
    case Printable(ch)  => copy(t"${content.take(pos)}$ch${content.drop(pos)}", pos + 1)
    case Ctrl('U')      => copy(content.drop(pos), 0)
    
    case Ctrl('W')      => val prefix = content.take(0 max (pos - 1)).reverse.dropWhile(_ != ' ').reverse
                           copy(t"$prefix${content.drop(pos)}", prefix.length)
    
    case Delete         => copy(t"${content.take(pos)}${content.drop(pos + 1)}")
    case Backspace      => copy(t"${content.take(pos - 1)}${content.drop(pos)}", (pos - 1) max 0)
    case Home           => copy(pos = 0)
    case End            => copy(pos = content.length)
    case LeftArrow      => copy(pos = (pos - 1) max 0)
    case CtrlLeftArrow  => copy(pos = (pos - 2 max 0 to 0 by -1).find(content(_) == ' ').fold(0)(_ + 1))
    
    case CtrlRightArrow => val range = ((pos + 1) min (content.length - 1)) to (content.length - 1)
                           val newPos = range.find(content(_) == ' ').fold(content.length)(_ + 1)
                           copy(pos = newPos min content.length)
    case RightArrow     => copy(pos = (pos + 1) min content.length)
    case _              => this
  catch case e: OutOfRangeError => this

  def unapply(stream: LazyList[Keypress | Signal])(using interaction: Interaction[LineEditor, Text])
             (using Monitor, Terminal)
             : Option[(Text, LazyList[Keypress | Signal])] =
    interaction(summon[Terminal].in, this)(_(_))

trait Interaction[StateType, ResultType]:
  def before(): Unit = ()
  def render(state: Maybe[StateType], menu: StateType): Unit
  def after(): Unit = ()
  def result(state: StateType): ResultType

  @tailrec
  final def recur(stream: LazyList[Keypress | Signal], state: StateType, oldState: Maybe[StateType])
                 (key: (StateType, Keypress | Signal) => StateType)
                 : Option[(ResultType, LazyList[Keypress | Signal])] =
    render(oldState, state)
    
    stream match
      case Keypress.Enter #:: tail           => Some((result(state), tail))
      case Keypress.Ctrl('C' | 'D') #:: tail => None
      case other #:: tail                    => recur(tail, key(state, other), state)(key)
      case _                                 => None

  def apply(stream: LazyList[Keypress | Signal], state: StateType)(key: (StateType, Keypress | Signal) => StateType)
           : Option[(ResultType, LazyList[Keypress | Signal])] =
    before()
    recur(stream, state, Unset)(key).tap(after().waive)

object Interaction:
  given [ItemType: Show](using Terminal): Interaction[SelectMenu[ItemType], ItemType] with
    override def before(): Unit = Io.print(t"\e?25l")
    override def after(): Unit = Io.print(t"\eJ\e?25h")
    
    def render(old: Maybe[SelectMenu[ItemType]], menu: SelectMenu[ItemType]) =
      menu.options.foreach: opt =>
        Io.print((if opt == menu.current then t" > $opt" else t"   $opt")+t"\eK\n")
      Io.print(t"\e${menu.options.length}A")
    
    def result(state: SelectMenu[ItemType]): ItemType = state.current
  
  given (using Terminal): Interaction[LineEditor, Text] with
    def render(editor: Maybe[LineEditor], editor2: LineEditor): Unit =
      val prior = editor.or(editor2)
      if prior.pos > 0 then Io.print(t"\e${prior.pos}D")
      val line = t"${editor2.content}${t" "*(prior.content.length - editor2.content.length)}"
      Io.print(t"\e0K")
      Io.print(line)
      if line.length > 0 then Io.print(t"\e${line.length}D")
      if editor2.pos > 0 then Io.print(t"\e${editor2.pos}C")

    def result(editor: LineEditor): Text = editor.content

case class SelectMenu[ItemType](options: List[ItemType], current: ItemType):
  import Keypress.*
  def apply(keypress: Keypress | Signal): SelectMenu[ItemType] = try keypress match
    case UpArrow   => copy(current = options(0 max options.indexOf(current) - 1))
    case DownArrow => copy(current = options(options.size - 1 min options.indexOf(current) + 1))
    case _         => this
  catch case e: OutOfRangeError => this

  def unapply
      (stream: LazyList[Keypress | Signal])
      (using terminal: Terminal, interaction: Interaction[SelectMenu[ItemType], ItemType])
      (using Monitor)
      : Option[(ItemType, LazyList[Keypress | Signal])] =
    
    interaction(summon[Terminal].in, this)(_(_))

given realm: Realm = realm"profanity"