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
package satirical

import anticipation.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

case class World(name: Ident, members: List[Import | Export])
case class Interface(name: Ident, members: List[Primitive | Func])
case class Ident(words: List[Text])
case class Package(namespace: Ident, name: Ident, version: Optional[Text])

case class Func(name: Ident, params: List[Primitive], returnType: Optional[Primitive])

case class Use(namespace: Ident, ident: Ident)
case class Import(ident: Ident)
case class Export(ident: Ident)

object Wit:
  given aggregable: Wit is Aggregable by Data = parse(_)

  def parse(input: Stream[Data]): Wit =
    val conduit = Conduit(input)

    def fail(msg: Message): Nothing = panic(msg)

    @tailrec
    def whitespace(): Unit = conduit.datum match
      case '\n' | '\r' | '\t' | ' ' => if conduit.next() then whitespace()

      case '/' =>
        if conduit.next() then
          if conduit.datum == '/' then comment() yet whitespace() else fail(m"expected /")
        else fail(m"early termination")

      case _ =>
        ()

    @tailrec
    def comment(): Unit = conduit.datum match
      case '\n' => ()
      case char => if conduit.next() then comment()

    def keyword(): Text =
      conduit.mark()

      def recur(): Text = conduit.datum match
        case char if char >= 'a' && char <= 'z' => conduit.next() yet recur()
        case other                              => Text(conduit.save()).also(whitespace())

      recur()

    def ident(): Ident =
      conduit.mark()
      def recur(hyphen: Boolean, uppercase: Boolean): Text = conduit.datum match
        case '-' =>
          if hyphen then fail(m"double hyphen")
          else if conduit.next() then recur(true, false) else fail(m"incomplete identifier")

        case char if char >= 'a' && char <= 'z' =>
          if !hyphen && uppercase then fail(m"mixed case")
          if conduit.next() then recur(false, false) else fail(m"incomplete identifier")

        case char if char >= 'A' && char <= 'Z' =>
          if !hyphen && uppercase then fail(m"mixed case")
          if conduit.next() then recur(false, true) else fail(m"incomplete identifier")

        case other =>
          Text(conduit.save()).also(whitespace())

      Ident(recur(true, false).cut(t"-"))

    def open(): Unit = conduit.datum match
      case '{' => if conduit.next() then whitespace() else fail(m"unclosed brace")
      case _   => fail(m"expected open")

    def close(): Unit = conduit.datum match
      case '}' => if conduit.next() then whitespace()
      case _   => fail(m"expected close")

    def world(): World =
      val name = ident()
      open()
      World(name, worldItems()).also(close())

    def interface(): Interface =
      val name = ident()
      open()
      Interface(name, interfaceItems()).also(close())

    def version(): Text =
      conduit.datum match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' =>
          if conduit.next() then version() else fail(m"premature termination")

        case char =>
          Text(conduit.save()).also(whitespace())

    def packageDeclaration(): Package =
      val namespace = ident()
      conduit.datum match
        case ':' =>
          val name = ident()
          conduit.datum match
            case '@' =>
              conduit.mark()
              val v = version()
              Package(namespace, name, v)

            case other =>
              whitespace()
              conduit.datum match
                case ';' => Package(namespace, name, Unset)
                case other => fail(m"unexpected character $other")

        case other =>
          fail(m"expected ':'")

    def interfaceItems(members: List[Primitive | Func] = Nil): List[Primitive | Func] = Nil

    def worldItems(done: List[Import | Export] = Nil): List[Import | Export] = keyword() match
      case t"import" => worldItems(witImport() :: done).also(whitespace())
      case t"export" => worldItems(witExport() :: done).also(whitespace())
      case other     => done

    def witImport(): Import = Import(???)
    def witExport(): Export = Export(???)


    def topLevel(pkg: Optional[Package] = Unset, members: List[World | Interface] = Nil)
    :   List[World | Interface] =

      keyword() match
        case t"package"   => topLevel(packageDeclaration(), members)
        case t"world"     => topLevel(pkg, world() :: members)
        case t"interface" => topLevel(pkg, interface() :: members)
        case _            => members.reverse


    whitespace()
    Wit(topLevel()*)

case class Wit(entries: (World | Interface | Package)*)
enum Primitive:
  case Bool, S8, S16, S32, S64, U8, U16, U32, U64, F32, F64, Char, String
  case List(elements: Primitive)
  case Option(element: Primitive)
  case Result(success: Primitive, failure: Primitive)
  case Tuple(elements: Primitive*)
  case Record(fields: (Ident, Primitive)*)
  case Variant(variants: (Ident, Optional[Primitive])*)
  case Enum(variants: Ident*)
  case Resource(interface: Interface)
  case Alias(name: Ident)

extension (context: StringContext) def w(): Ident = Ident(context.parts.head.tt.cut(t"-"))
