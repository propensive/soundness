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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package decorum

import scala.collection.mutable

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

case class CompanionDecls
  ( typeLines:   Map[(Int, String), Int],
    objectLines: Map[(Int, String), Int] )

object Companions:
  // Collect (scope, name) -> first-line maps for type-like decls
  // (class/trait/enum) and for objects. R28 asks that an object precede its
  // companion type — but a type and an object are companions only when they
  // share an *enclosing scope*. Keying by `(scope, name)`, where `scope` is
  // the span of the nearest enclosing definition (or -1 at the file root),
  // stops e.g. a nested enum case `Hole.Element` (a `TypeDef` named `Element`)
  // from being mistaken for the companion of a top-level `object Element`.
  def extract(tree: untpd.Tree, source: SourceFile): CompanionDecls =
    val types   = mutable.Map[(Int, String), Int]()
    val objects = mutable.Map[(Int, String), Int]()

    def line(t: untpd.Tree): Option[Int] =
      val sp = t.span
      if sp.exists then Some(source.offsetToLine(sp.start) + 1) else None

    def innerScope(t: untpd.Tree, scope: Int): Int =
      if t.span.exists then t.span.start else scope

    def walk(t: untpd.Tree, scope: Int): Unit =
      val childScope =
        t match
          case td: untpd.TypeDef =>
            // A `TypeDef` with a `Template` rhs is a class/trait/enum (or a
            // parameterised enum case); type aliases (`type X = Y`) have a
            // non-Template rhs and are ignored for R28.
            td.rhs match
              case _: untpd.Template =>
                line(td).foreach: l =>
                  val key = (scope, td.name.toString)
                  if !types.contains(key) then types(key) = l

              case _ =>
                ()

            innerScope(td, scope)

          case md: untpd.ModuleDef =>
            line(md).foreach: l =>
              val key = (scope, md.name.toString)
              if !objects.contains(key) then objects(key) = l

            innerScope(md, scope)

          case _: untpd.PackageDef =>
            innerScope(t, scope)

          case _ =>
            scope

      t.productIterator.foreach(descend(_, childScope))

    def descend(x: Any, scope: Int): Unit = x match
      case sub: untpd.Tree  => walk(sub, scope)
      case it:  Iterable[?] => it.foreach(descend(_, scope))
      case _                => ()

    walk(tree, -1)
    CompanionDecls(types.toMap, objects.toMap)
