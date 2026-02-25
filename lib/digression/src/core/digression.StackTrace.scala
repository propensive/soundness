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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package digression

import anticipation.*
import fulminate.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

object StackTrace:
  case class Method(className: Text, method: Text):
    private lazy val pivot = className.s.lastIndexOf(".")

    lazy val cls: Text = if pivot >= 0 then className.s.substring(pivot + 1).nn.tt else className.s
    lazy val prefix: Text = if pivot >= 0 then className.s.substring(0, pivot).nn.tt else "".tt

  case class Frame(method: Method, file: Text, line: Optional[Int], native: Boolean)

  val legend: Map[Text, Text] =
    Map
      ( "λₙ".tt -> "anonymous function".tt,
        "αₙ".tt -> "anonymous class".tt,
        "ι".tt  -> "initialization".tt,
        "↑".tt  -> "super reference".tt,
        "⊢".tt  -> "extension method".tt,
        "∂".tt  -> "direct".tt,
        "δ".tt  -> "default".tt,
        "⁅⁆".tt -> "package file".tt,
        "ⲛ".tt  -> "class initializer".tt,
        "ℓ".tt  -> "lazy initializer".tt,
        "Σ".tt  -> "specialized method".tt )

  def rewrite(name: String, method: Boolean = false): Text =
    val buffer: StringBuilder = StringBuilder()

    inline def char(index: Int): Optional[Char] =
      if index < 0 || index >= name.length then Unset else name.charAt(index)

    def primitive(char: Char) = char match
      case 'Z' => "Boolean"
      case 'I' => "Int"
      case 'J' => "Long"
      case 'V' => "Unit"
      case 'S' => "Short"
      case 'F' => "Float"
      case 'C' => "Char"
      case 'D' => "Double"
      case 'L' => "Any"
      case _   => "?"

    @tailrec
    def recur(index: Int, digits: Boolean = false): Text =
      inline def token(index: Int, string: String, text: String, digits: Boolean = false): Text =
        if (0 until string.length).all { i => char(index + i) == string(i) }
        then buffer.append(text) yet recur(index + string.length, digits)
        else buffer.append('#') yet recur(index + 1, digits)

      inline def skip(): Text = token(index, "$", if method then "()." else "#")

      if index >= name.length then Text(buffer.toString+(if method then "()" else ""))
      else if digits then char(index) match
        case '0' => token(index, "0", "₀", true)
        case '1' => token(index, "1", "₁", true)
        case '2' => token(index, "2", "₂", true)
        case '3' => token(index, "3", "₃", true)
        case '4' => token(index, "4", "₄", true)
        case '5' => token(index, "5", "₅", true)
        case '6' => token(index, "6", "₆", true)
        case '7' => token(index, "7", "₇", true)
        case '8' => token(index, "8", "₈", true)
        case '9' => token(index, "9", "₉", true)
        case _   => recur(index, false)
      else char(index) match
        case '<' =>
          if (0 until 6).all { i => char(index + i) == "<init>"(i) }
          then
            buffer.append("ⲛ")
            recur(index + 6)
          else
            buffer.append("<")
            recur(index + 1)

        case 'i' =>
          if (0 until 8).all { i => char(index + i) == "initial$"(i) }
          then
            buffer.append("ι")
            recur(index + 8)
          else
            buffer.append("i")
            recur(index + 1)

        case 'l' =>
          if (0 until 7).all { i => char(index + i) == "lzyINIT"(i) }
          then
            buffer.append("ℓ")
            recur(index + 7, true)
          else
            buffer.append("l")
            recur(index + 1)

        case 's' =>
          if (0 until 6).all { i => char(index + i) == "super$"(i) }
          then
            buffer.append("↑")
            recur(index + 6)
          else
            buffer.append("s")
            recur(index + 1)

        case '$' =>
          char(index + 1) match
            case '_' => token(index,           "$_avoid_name_clash_$", "′")
            case 'a' => char(index + 2) match
              case 'm' => token(index,         "$amp",                 "&")
              case 'n' => char(index + 5) match
                case 'f' => token(index,       "$anonfun",             "λ")
                case _   => token(index,       "$anon",                "α")
              case 't' => token(index,         "$at",                  "@")
              case _   => skip()
            case 'b' => char(index + 2) match
              case 'a' => char(index + 3) match
                case 'n' => token(index,       "$bang",                "!")
                case 'r' => token(index,       "$bar",                 "|")
                case _   => skip()
              case 's' => token(index,         "$bslash",              "\\")
              case _   => skip()
            case 'c' => token(index,           "$colon",               ":")
            case 'd' => char(index + 2) match
              case 'e' => token(index,         "$default",             "δ")
              case 'i' => char(index + 3) match
                case 'r' => token(index,       "$direct",              "∂")
                case 'v' => token(index,       "$div",                 "/")
                case _   => skip()
              case _   => skip()
            case 'e' => char(index + 2) match
              case 'q' => token(index,         "$eq",                  "=")
              case 'x' => token(index,         "$extension",           "⊢")
              case _   => skip()
            case 'g' => token(index,           "$greater",             ">")
            case 'h' => token(index,           "$hash",                "#")
            case 'l' => token(index,           "$less",                "<")
            case 'm' => char(index + 2) match
              case 'c' =>
                var index2: Int = index + 3
                var arguments: List[Text] = Nil
                var current = char(index2)

                while current != '$' do
                  arguments = primitive(current.or('?')) :: arguments
                  index2 += 1
                  current = char(index2)

                val name2 =
                  if arguments.length == 2 then "Σ("+arguments.last+" -> "+arguments.head+")"
                  else arguments.tail.mkString("Σ((", ", ", ")")+" -> "+arguments.head+")"

                val mc = name.substring(index, index + 3).nn
                token(index, mc, name2)
              case 'i' => token(index,         "$minus",               "-")
              case _   => skip()
            case 'p' => char(index + 2) match
              case 'a' => token(index,         "$package",             "⁆")
              case 'e' => token(index,         "$percent",             "%")
              case 'l' => token(index,         "$plus",                "+")
              case _   => skip()
            case 'q' => token(index,           "$qmark",               "?")
            case 's' => char(index + 2) match
              case 'p' => token(index,         "$sp",                  "ζ")
              case _   => skip()
            case 't' => char(index + 2) match
              case 'i' => char(index + 3) match
                case 'l' => token(index,       "$tilde",               "~")
                case 'm' => token(index,       "$times",               "*")
                case _   => skip()
              case _  => skip()
            case 'u' => token(index,           "$up",                  "^")
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => recur(index + 1, true)
            case _   => skip()

        case ch =>
          buffer.append(ch.toString)
          recur(index + 1)

    val rewritten = recur(0)

    if rewritten.s.startsWith("scala.runtime.java8.JFunction") && rewritten.s.endsWith("#sp") then
      val types: String = rewritten.s.drop(29).dropRight(3)
      types.indexOf("#mc") match
        case -1 | 0 => rewritten

        case i =>
          Text:
            val types2 = types.drop(i + 3).to(List).map(primitive)
            if types2.length <= 2 then types2.mkString("(", " => ", ")")
            else types2.init.mkString("((", ", ", s") => ${types2.last})")

    else if rewritten.s.startsWith("scala.runtime.function.JProcedure") then Text:
      val n = try rewritten.s.drop(33).toInt catch case error: Exception => 0
      "("+(if n < 2 then s"Any" else List.fill(n)("Any").mkString("(", ", ", ")"))+" => Unit)"

    else if rewritten.s.endsWith("#") then
      val pivot = rewritten.s.lastIndexOf(".")
      val sub = if rewritten.s.endsWith("⁆#") then "⁅" else "Ξ"

      (rewritten.s.substring(0, pivot).nn+"."+sub+rewritten.s.substring(pivot + 1).nn.dropRight(1))
      . tt
    else rewritten

  def apply(exception: Throwable): StackTrace =
    val frames = List(exception.getStackTrace.nn.map(_.nn)*).map: frame =>
      StackTrace.Frame(
        StackTrace.Method(
          rewrite(frame.getClassName.nn),
          rewrite(frame.getMethodName.nn, method = true)
        ),
        Text(Option(frame.getFileName).map(_.nn).getOrElse("[no file]")),
        if frame.getLineNumber < 0 then Unset else frame.getLineNumber,
        frame.isNativeMethod
      )

    val cause = Option(exception.getCause)
    val fullClassName: Text = rewrite(exception.getClass.nn.getName.nn)
    val fullClass: List[Text] = List(fullClassName.s.split("\\.").nn.map(_.nn).map(Text(_))*)
    val className: Text = fullClass.last

    val component: Text =
      val length = fullClassName.s.length - className.s.length - 1
      Text(fullClassName.s.substring(0, 0.max(length)).nn)

    val message: Message = exception match
      case error: Error => error.message
      case other        => Message(Text(Option(exception.getMessage).map(_.nn).getOrElse("")))

    StackTrace(component, className, message, frames, cause.map(_.nn).map(StackTrace(_)).optional)

  given communicable: StackTrace is Communicable = stack =>
    val methodWidth = stack.frames.map(_.method.method.s.length).maxOption.getOrElse(0)
    val classWidth = stack.frames.map(_.method.className.s.length).maxOption.getOrElse(0)
    val fileWidth = stack.frames.map(_.file.s.length).maxOption.getOrElse(0)
    val fullClass = s"${stack.component}.${stack.className}".tt
    val init = s"$fullClass: ${stack.message}".tt
    val nbsp = "\u00a0".tt

    val root = stack.frames.fuse(init):
      val obj = next.method.className.s.endsWith("#")
      val drop = if obj then 1 else 0
      val file = (nbsp*(fileWidth - next.file.s.length))+next.file
      val dot = if obj then ".".tt else "#".tt
      val className = next.method.className.s.dropRight(drop)
      val classPad = (nbsp*(classWidth - className.length))
      val method = next.method.method
      val methodPad = (nbsp*(methodWidth - method.s.length))

      val line = next.line match
        case Unset => "?".tt
        case value => value.toString.tt

      s"$state\n\n${nbsp*2}at$nbsp$classPad$className$dot$method$methodPad$nbsp$file:$line".tt

    Message:
      stack.cause.lay(root): cause =>
        s"$root\ncaused by:\n\n$cause".tt

case class StackTrace
  ( component: Text,
    className: Text,
    message:   Message,
    frames:    List[StackTrace.Frame],
    cause:     Optional[StackTrace] ):

  def crop(cutClassName: Text, cutMethod: Text): StackTrace =
    val frames2 = frames.takeWhile: f =>
      f.method.className != cutClassName || f.method.method != cutMethod

    StackTrace(component, className, message, frames2, cause)

  def drop(n: Int): StackTrace =
    StackTrace(component, className, message, frames.drop(n), cause)

  def dropRight(n: Int): StackTrace =
    StackTrace(component, className, message, frames.dropRight(n), cause)
