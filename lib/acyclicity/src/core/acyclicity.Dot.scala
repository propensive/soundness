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
package acyclicity

import anticipation.*
import denominative.*
import gossamer.*
import nomenclature.*
import scala.collection.immutable.List

import rudiments.*
import spectacular.*
import symbolism.*

object Dot:
  case class Target(directed: Boolean, dest: Name[DotId] | Statement.Subgraph, link: Option[Target])
  case class Property(key: Text, value: Text)

  // The DOT graph DSL is built from `Name[DotId]` identifiers. An identifier acts
  // as an edge endpoint (`a -- b`, `a --> b`), an assignment left-hand side
  // (`a := b`) or a node declaration carrying attributes (`a("color" -> "red")`).
  extension (id: Name[DotId])
    @targetName("joinTo")
    infix def -- (dest: Name[DotId] | Statement.Subgraph): Statement.Edge =
      Statement.Edge(id, Target(false, dest, None))

    @targetName("mapTo")
    infix def --> (dest: Name[DotId] | Statement.Subgraph): Statement.Edge =
      Statement.Edge(id, Target(true, dest, None))

    @targetName("assign")
    infix def := (id2: Name[DotId]): Statement.Assignment = Statement.Assignment(id, id2)

    def apply(attributes: (Text, Text)*): Statement.Node =
      Statement.Node(id, attributes.map { (key, value) => Property(key, value) }*)

  enum Statement:
    case Node(id: Name[DotId], attrs: Property*)
    case Edge(id: Name[DotId], rhs: Target, attrs: Property*)
    case Assignment(id: Name[DotId], id2: Name[DotId])
    case Subgraph(id: Option[Name[DotId]], statements: Statement*)

  def serialize(tokens: List[Text]): Text = Text.build:
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
      else
        append(t" ")

    tokens.each:
      case t""  => ()
      case t"," => append(t",")
      case t"{" => whitespace(); append(t"{"); indent(); newline()
      case t"}" => outdent(); whitespace(); append(t"}"); newline()
      case t"[" => whitespace(); append(t"[")
      case t"]" => whitespace(); append(t"]"); newline()
      case t";" => newline()
      case word => whitespace(); append(word)

  private def tokenize(graph: Dot | Target | Statement | Property): List[Text] = graph match
    case Property(key, value) => List(t"$key=\"$value\"")

    case Target(directed, dest, link) =>
      val operator = if directed then t"->" else t"--"

      val destTokens = (dest: @unchecked) match
        case subgraph: Statement.Subgraph => tokenize(subgraph)
        case id: Text                     => List(t"\"$id\"")

      operator :: destTokens ::: link.to(List).flatMap(tokenize(_)) ::: List(t";")

    case Statement.Node(id, attrs*) =>
      t"\"${id: Text}\"" :: (if attrs.isEmpty then List() else (List(t"[") :::
        attrs.to(List).flatMap(tokenize(_) :+ t",").init ::: List(t"]"))) :::
        List(t";")

    case Statement.Edge(id, rhs, attrs*) =>
      List(t"\"${id: Text}\"") ::: tokenize(rhs)

    case Statement.Assignment(id, id2) =>
      List(t"\"${id: Text}\"", t"=", t"\"${id2: Text}\"", t";")

    case Statement.Subgraph(id, statements*) =>
      t"subgraph" :: id.to(List).map { name => name: Text } :::
        t"{" ::
        statements.to(List).flatMap(tokenize(_)) :::
        List(t"}")

    case Dot.Graph(id, strict, statements*) =>
      List(
        if strict then List(t"strict") else List(),
        List(t"graph"),
        id.to(List).map { name => name: Text }, List(t"{"),
        statements.flatMap(tokenize(_)), List(t"}")
      ).flatten

    case Dot.Digraph(id, strict, statements*) =>
      List(
        if strict then List(t"strict") else List(),
        List(t"digraph"),
        id.to(List).map { name => name: Text },
        List(t"{"),
        statements.flatMap(tokenize(_)),
        List(t"}")
      ).flatten

enum Dot:
  case Graph(id: Option[Name[DotId]], strict: Boolean, statements: Dot.Statement*)
  case Digraph(id: Option[Name[DotId]], strict: Boolean, statements: Dot.Statement*)

  def serialize: Text = Dot.serialize(Dot.tokenize(this))

  def add(additions: Dot.Statement*): Dot = this match
    case Dot.Graph(id, strict, statements*)   => Dot.Graph(id, strict, (statements ++ additions)*)
    case Dot.Digraph(id, strict, statements*) => Dot.Digraph(id, strict, (statements ++ additions)*)
