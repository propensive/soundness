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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package acyclicity

import language.dynamics

import anticipation.*
import contextual.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

enum Dot:
  case Graph(id: Option[Dot.Id], strict: Boolean, statements: Dot.Statement*)
  case Digraph(id: Option[Dot.Id], strict: Boolean, statements: Dot.Statement*)

  def serialize: Text = Dot.serialize(Dot.tokenize(this))

  def add(additions: Dot.Statement*): Dot = this match
    case Dot.Graph(id, strict, statements*) =>
      Dot.Graph(id, strict, (statements ++ additions)*)

    case Dot.Digraph(id, strict, statements*) =>
      Dot.Digraph(id, strict, (statements ++ additions)*)

object Dot:
  case class Target(directed: Boolean, dest: Ref | Statement.Subgraph, link: Option[Target])
  case class Property(key: Text, value: Text)

  object Attachment:
    // FIXME: This needs to include the port
    given showable: Attachment is Showable = _.id.key

  case class Attachment(id: Id, compass: Option[CompassPoint] = None)

  case class Ref(id: Id, port: Option[Attachment] = None):
    @targetName("joinTo")
    infix def --(dest: Ref | Statement.Subgraph): Dot.Statement.Edge =
      Dot.Statement.Edge(this, Target(false, dest, None))

    @targetName("mapTo")
    infix def -->(dest: Ref | Statement.Subgraph): Dot.Statement.Edge =
      Dot.Statement.Edge(this, Target(true, dest, None))

  object Ref:
    def apply(key: Text): Ref = Ref(Id(key))

  case class Id(key: Text) extends Dynamic:
    def applyDynamicNamed(method: "apply")(attrs: (String, Text)*) =
      Statement.Node(this, attrs.map { (k, v) => Property(k.show, v) }*)

    @targetName("assign")
    infix def :=(id: Id): Statement.Assignment = Statement.Assignment(this, id)

  enum CompassPoint:
    case North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest

  enum Statement:
    case Node(id: Id, attrs: Property*)
    case Edge(id: Ref, rhs: Target, attrs: Property*)
    case Assignment(id: Id, id2: Id)
    case Subgraph(id: Option[Id], statements: Statement*)

  def serialize(tokens: Stream[Text]): Text = Text.construct:
    var level: Int = 0
    var end: Boolean = true

    def indent(): Unit = level += 1
    def outdent(): Unit = level -= 1
    def newline(): Unit = end = true

    def whitespace(): Unit =
      if end then
        append(t"\n")
        append(t"  "*level)
        end = false
      else append(t" ")

    tokens.each:
      case t""  => ()
      case t"," => append(t",")
      case t"{" => whitespace(); append(t"{"); indent(); newline()
      case t"}" => outdent(); whitespace(); append(t"}"); newline()
      case t"[" => whitespace(); append(t"[")
      case t"]" => whitespace(); append(t"]"); newline()
      case t";" => newline()
      case word => whitespace(); append(word)

  private def tokenize(graph: Ref | Dot | Target | Statement | Property): Stream[Text] = graph match
    case Ref(id, port) =>
      Stream(port.fold(t"\"${id.key}\"") { p => t"\"${id.key}:$p\"" })

    case Property(key, value) =>
      Stream(t"$key=\"$value\"")

    case Target(directed, dest, link) =>
      val op = if directed then t"->" else t"--"
      op #:: tokenize(dest) #::: link.to(Stream).flatMap(tokenize(_)) #::: Stream(t";")

    case Statement.Node(id, attrs*) =>
      t"\"${id.key}\"" #:: (if attrs.isEmpty then Stream() else (Stream(t"[") #:::
          attrs.to(Stream).flatMap(tokenize(_) :+ t",").init #::: Stream(t"]"))) #:::
          Stream(t";")

    case Statement.Edge(id, rhs, attrs*) =>
      tokenize(id) #::: tokenize(rhs)

    case Statement.Assignment(id, id2) =>
      Stream(t"\"${id.key}\"", t"=", t"\"${id2.key}\"", t";")

    case Statement.Subgraph(id, statements*) =>
      t"subgraph" #:: id.to(Stream).map(_.key) #::: t"{" #::
          statements.to(Stream).flatMap(tokenize(_)) #::: Stream(t"}")

    case Dot.Graph(id, strict, statements*) =>
      Stream(
        if strict then Stream(t"strict") else Stream(),
        Stream(t"graph"),
        id.to(Stream).map(_.key), Stream(t"{"),
        statements.flatMap(tokenize(_)), Stream(t"}")
      ).flatten

    case Dot.Digraph(id, strict, statements*) =>
      Stream(
        if strict then Stream(t"strict") else Stream(),
        Stream(t"digraph"),
        id.to(Stream).map(_.key),
        Stream(t"{"),
        statements.flatMap(tokenize(_)),
        Stream(t"}")
      ).flatten
