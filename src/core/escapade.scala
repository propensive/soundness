package escapade

import contextual.*
import iridescence.*
import rudiments.*

import scala.collection.immutable.TreeMap

case class AnsiString(string: String, escapes: TreeMap[Int, List[Ansi.Change]]):
  def length: Int = string.length
  def take(n: Int): AnsiString = AnsiString(string.take(n), escapes.takeWhile(_._1 <= n))
  def plain: String = string
  def explicit: String = render.map { ch => if ch.toInt == 27 then '^' else ch }

  def render: String =
    val buf = StringBuilder()
    
    def build(treeMap: TreeMap[Int, List[Ansi.Change]], pos: Int = 0, stack: List[Srgb] = Nil): String =
      if treeMap.isEmpty
      then
        buf.append(string.slice(pos, string.length))
        buf.toString
      else
        buf.append(string.slice(pos, treeMap.head(0)))
        val newStack = treeMap.head(1).foldLeft(stack) {
          case (s, Ansi.Change.Pop) =>
            buf.append(s.tail.headOption.fold(s"${27.toChar}[39m")(_.ansiFg24))
            s.tail
          
          case (s, Ansi.Change.Push(fn)) =>
            val next = fn(stack)
            buf.append(next.ansiFg24)
            next :: s
          
          case (s, Ansi.Change.Literal(str)) =>
            buf.append(27.toChar)
            buf.append(str)
            Nil
        }
        
        build(treeMap.tail, treeMap.head(0), newStack)
    
    build(escapes)

  private def shift(n: Int): TreeMap[Int, List[Ansi.Change]] =
    escapes.map { (k, v) => (k + n, v) }.to(TreeMap)

  def +(str: String): AnsiString = AnsiString(string+str, escapes)
  def addEsc(esc: Ansi.Change): AnsiString = addEsc(string.length, esc)
  
  def addEsc(pos: Int, esc: Ansi.Change): AnsiString =
    AnsiString(string, escapes.updated(string.length, esc :: escapes.get(string.length).getOrElse(Nil)))
  
  def +(ansi: AnsiString): AnsiString =
    AnsiString(string+ansi.string, escapes ++ ansi.shift(length))

object Ansi:
  enum Input:
    case Str(string: String)
    case Esc(code: String, reset: String)
    case Col(color: List[Srgb] => Srgb)

  enum Change:
     case Push(color: List[Srgb] => Srgb)
     case Pop
     case Literal(str: String)

  case class State(string: AnsiString, last: Option[Ansi.Change], stack: List[(Char, Ansi.Change)]):
    def add(str: String): State = copy(string = string + str, last = None)
    def addEsc(esc: Ansi.Change): State = copy(string = string.addEsc(esc), last = None)
    def addEsc(pos: Int, esc: Ansi.Change): State = copy(string = string.addEsc(pos, esc), last = None)
    def isEmpty: Boolean =
      string.string == "" && string.escapes.isEmpty && last.isEmpty && stack.isEmpty

  object Interpolator extends contextual.Interpolator[Input, State, AnsiString]:
    def initial: State = State(AnsiString("", TreeMap()), None, Nil)

    private def closures(state: State, str: String): State =
      if state.stack.isEmpty then state.add(str)
      else
        str.indexOf(state.stack.head(0)) match
          case -1 =>
            state.add(str)
          
          case i =>
            val newState = state.copy(stack = state.stack.tail).add(str.slice(0, i)).addEsc(state.stack.head(1))
            closures(newState, str.slice(i + 1, str.length))

    private def complement(ch: '[' | '(' | '{' | '<' | '«'): ']' | ')' | '}' | '>' | '»' = ch match
      case '[' => ']'
      case '(' => ')'
      case '{' => '}'
      case '<' => '>'
      case '«' => '»'

    def parse(state: State, string: String): State =
      if state.isEmpty then State(AnsiString(string, TreeMap()), None, Nil)
      else state.last match
        case None =>
          closures(state, string)
        
        case Some(last) =>
          string.headOption match
            case Some('\\') =>
              closures(state, string.tail)

            case Some(ch@('[' | '(' | '{' | '<' | '«')) =>
              closures(state.copy(last = None, stack = (complement(ch), last) :: state.stack), string.tail)
            
            case _ =>
              closures(state, string)

    def insert(state: State, value: Input): State =
      value match
        case Input.Str(string) =>
          State(state.string + string, None, state.stack)

        case Input.Col(fn) =>
          state.addEsc(Change.Push(fn)).copy(last = Some(Change.Pop))

        case Input.Esc(start, end) =>
          state.addEsc(Change.Literal(start)).copy(last = Some(Change.Literal(end)))

    def skip(state: State): State = insert(state, Input.Str(""))

    override def substitute(state: State, value: String): State =
      
      val dummy = value match
        case "esc" => Ansi.Input.Esc("[0m", "")
        case _     => Ansi.Input.Str("")
      
      insert(state, dummy)

    def complete(state: State): AnsiString =
      if !state.stack.isEmpty then throw InterpolationError("mismatched closing brace")

      state.string

given Substitution[Ansi.Input, String, "str"] = Ansi.Input.Str(_)
given Substitution[Ansi.Input, Int, "str"] = int => Ansi.Input.Str(int.toString)
given Substitution[Ansi.Input, Escape, "esc"] = identity(_)
given Substitution[Ansi.Input, Color, "esc"] = col => Ansi.Input.Col({ _ => col.standardSrgb })