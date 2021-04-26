package guillotine

import contextual.*
import rudiments.*

import scala.collection.JavaConverters.*
import scala.jdk.StreamConverters.StreamHasToScala

import java.util.HashMap as JMap
import java.io.*

type Stream = LazyList[String]

object envs:
  val enclosing: Env = Env(System.getenv.asScala.to(Map), None)
  val empty: Env = Env(Map(), None)

enum Context:
  case Awaiting, Unquoted, DoubleQuotes, SingleQuotes

case class State(current: Context, esc: Boolean, args: List[String])

object Executor:
  
  given stream: Executor[Stream] =
    proc => BufferedReader(InputStreamReader(proc.getInputStream)).lines().toScala(LazyList)
  
  given Executor[String] =
    stream.map { stream => if stream.isEmpty then "" else stream.reduce(_ + "\n" + _).trim }

trait Executor[T]:
  def interpret(process: Process): T
  def map[S](fn: T => S): Executor[S] = process => fn(interpret(process))

case class Command(args: String*):
  private lazy val processBuilder = ProcessBuilder(args*)

  def exec[T: Executor]()(using Env): T = summon[Executor[T]].interpret(processBuilder.start())

case class Env(vars: Map[String, String], workDir: Option[String]):
  private[guillotine] lazy val envArray: Array[String] = vars.map { (k, v) => s"$k=$v" }.to(Array)
  
  private[guillotine] lazy val javaMap: JMap[String, String] = vars.foldLeft(JMap()) { case (m, (k, v)) =>
    m.put(k, v)
    m
  }

  private lazy val workDirFile: File = File(workDir.getOrElse(System.getenv("PWD")))
  
case class ExecError(command: Command, stdout: Stream, stderr: Stream) extends Exception

object Sh extends Interpolator[List[String], State, Command]:
  import Context.*
  
  def complete(state: State): Command =
    val args = state.current match
      case DoubleQuotes       => throw ParseError("the double quotes have not been closed")
      case SingleQuotes       => throw ParseError("the single quotes have not been closed")
      case other if state.esc => throw ParseError("An escape character is not permitted at the end")
      case _                  => state.args
    
    Command(args*)

  def initial: State = State(Awaiting, false, Nil)

  def insert(state: State, value: Option[List[String]]): State = value.getOrElse(List("x")) match
    case Nil =>
      state
    case h :: t =>
      if state.esc then throw ParseError("escaping with '\\' is not allowed immediately before a substitution")
      
      state match
        case State(Awaiting, false, args) =>
          State(Unquoted, false, args ++ (h :: t))
        case State(Unquoted, false, args :+ last) =>
          State(Unquoted, false, args ++ (s"$last$h" :: t))
        case State(SingleQuotes, false, args :+ last) =>
          State(SingleQuotes, false, args :+ (s"$last$h" :: t).join(" "))
        case State(DoubleQuotes, false, args :+ last) =>
          State(DoubleQuotes, false, args :+ (s"$last$h" :: t).join(" "))

  def parse(state: State, next: String): State = next.foldLeft(state) {
    case (State(Awaiting, esc, args), ' ')                      => State(Awaiting, false, args)
    case (State(ctx, false, args), '\\') if ctx != SingleQuotes => State(ctx, true, args)
    case (State(Unquoted, esc, args), ' ')                      => State(Awaiting, false, args)
    case (State(SingleQuotes, esc, args), '\'')                 => State(Unquoted, false, args)
    case (State(DoubleQuotes, false, args), '"')                => State(Unquoted, false, args)
    case (State(Unquoted, false, args), '"')                    => State(DoubleQuotes, false, args)
    case (State(Unquoted, false, args), '\'')                   => State(SingleQuotes, false, args)
    case (State(Awaiting, false, args), '"')                    => State(DoubleQuotes, false, args :+ "")
    case (State(Awaiting, false, args), '\'')                   => State(SingleQuotes, false, args :+ "")
    case (State(Awaiting, esc, args), ch)                       => State(Unquoted, false, args :+ s"$ch")
    case (State(ctx, esc, Nil), ch)                             => State(ctx, false, List(s"$ch"))
    case (State(ctx, esc, rest :+ cur), ch)                     => State(ctx, false, rest :+ s"$cur$ch")
  }

given Insertion[List[String], String] = value => List(value)
given Insertion[List[String], List[String]] = identity(_)
given Insertion[List[String], Command] = _.args.to(List)