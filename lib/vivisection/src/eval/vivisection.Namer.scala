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
package vivisection

import scala.collection.concurrent as scc

import anticipation.*
import digression.*
import gossamer.*
import hellenism.*
import hyperbole.*
import proscenium.*
import rudiments.*
import vacuous.*

// Names inline positions from the TASTy the compiler wrote beside the debuggee's classfiles:
// given the `ScalaClass`-stratum class name and an origin position, it answers the definition
// the programmer wrote there — owner and name, demangled as stack traces render them. The
// classloader over the launch classpath fetches `.tasty` resources only; it never defines
// debuggee classes in the debugger's JVM. Lookups are memoized per class, absences included; a
// classpath whose TASTy disagrees with the SMAP-recorded source path yields nothing, and every
// absence degrades to the caller's fallback name.
class Namer(classpath: LocalClasspath):
  private lazy val classloader: Classloader = classpath.classloader()
  private val files: scc.TrieMap[Text, Optional[Tasty.File]] = scc.TrieMap()

  def define(cls: Text, path: Text, line: Int): Optional[Text] =
    load(cls).let: tasty =>
      if !tasty.path.let(_ == path).or(false) then Unset else
        tasty.covering(line).prim.let: definition =>
          val owners: List[Text] = definition.owners.reverse.map(display(_))
          val owner: Text = owners.filter(_ != t"").join(t".")
          val name = display(definition.name)
          if owner == t"" then name else t"$owner.$name"

  private def load(name: Text): Optional[Tasty.File] =
    files.getOrElseUpdate(name, fetch(name.s))

  // The resource path keeps the binary name exactly as the SMAP records it — `$u002E` escapes
  // are part of the file name on the classpath — and a miss walks outwards through `$`-joined
  // enclosing classes, as nested and module classes pickle under their top-level class.
  private def fetch(name: String): Optional[Tasty.File] =
    val resource = (name.replace('.', '/').nn+".tasty").tt
    val loaded: Optional[Tasty.File] = classloader(resource).lay(Unset)(Tasty.parse(_))

    loaded.or:
      name.lastIndexOf('$') match
        case -1 | 0 => Unset
        case index  => fetch(name.substring(0, index).nn)

  // As `hyperbole.StackResolver.display`: drop a module class's trailing `$`, mark an
  // initializer, and demangle compiler-derived names so resolved and raw frames read alike.
  private def display(name: Text): Text =
    val stripped = if name.s.endsWith("$") then name.s.dropRight(1).nn else name.s

    if stripped == "<init>" then t"ⲛ"
    else if stripped.contains("$") then StackTrace.rewrite(stripped)
    else stripped.tt
