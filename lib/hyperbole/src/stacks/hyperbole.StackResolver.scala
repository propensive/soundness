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
package hyperbole

import scala.collection.mutable

import anticipation.*
import contingency.*
import digression.*
import distillate.*
import galilei.*, galilei.Platform.pathReadable
import gossamer.*
import hellenism.*
import hieroglyph.*
import mandible.*
import prepositional.*
import proscenium.compat.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

import StackTrace.Frame.Kind
import charDecoders.utf8Decoder
import textSanitizers.skipSanitizer

object StackResolver:
  // The compiled name cannot say which definition a frame came from, but it can say what kind of
  // thing it is: each of these markers is minted after TASTy is written, by a phase which erases,
  // specializes, lifts or bridges, and each is a reliable sign of that phase's work. A name with
  // no marker says nothing, and the definition the frame resolves to speaks for itself.
  def hint(method: Text): Optional[Kind] =
    val name = method.s

    if name == "<init>" then Kind.Constructor
    else if name == "<clinit>" || name == "$init$" then Kind.Initializer
    else if name.contains("$anonfun$adapted") then Kind.Bridge
    else if name.contains("$anonfun") then Kind.Lambda
    else if name.endsWith("$extension") then Kind.Extension
    else if name.contains("$default$") then Kind.Default
    else if name.contains("$adapted") then Kind.Bridge
    else if name.contains("lzyINIT") || name.contains("$lzy") then Kind.Initializer
    else if name.contains("$mc") && name.endsWith("$sp") then Kind.Specialized
    else Unset

  // Which of the definitions on a line the frame belongs to. Position alone cannot tell a
  // constructor from the first member declared beside it, or a lambda from its enclosing method,
  // but what the compiled name says about the frame's kind settles both; failing that, a name
  // which survived compilation unchanged identifies its definition outright.
  def matches(method: Text, hint: Optional[Kind], definition: Tasty.Definition): Boolean =
    hint match
      case Kind.Constructor => definition.name == t"<init>"
      case Kind.Lambda      => definition.kind == Kind.Lambda
      case _                => definition.name == method

  // Most TASTy names are source names and need no decoding. The exceptions are the ones the
  // compiler derived before pickling—anonymous functions, default getters, package files—and
  // those are exactly what `rewrite` already renders, so a resolved frame reads in the same
  // vocabulary as an unresolved one. A module class's trailing `$` is dropped first: it says
  // nothing the frame does not say already, and it is not a derivation `rewrite` knows.
  def display(name: Text): Text =
    val stripped = if name.s.endsWith("$") then name.s.dropRight(1).nn else name.s

    if stripped == "<init>" then t"ⲛ"
    else if stripped.contains("$") then StackTrace.rewrite(stripped)
    else stripped.tt

// Recovers the source definition behind each frame of a stack trace by reading the TASTy the
// compiler wrote beside the classfile. TASTy records source names, not compiled ones, so the two
// cannot be matched up by name; but it also records the extent of every definition, and a line
// table for the source file, so the line the frame already carries is enough to find it.
class StackResolver(using classloader: Classloader) extends StackTrace.Resolver:
  private val tastyFiles: mutable.HashMap[Text, Optional[Tasty.File]] = mutable.HashMap()
  private val sourceFiles: mutable.HashMap[Text, Optional[List[Text]]] = mutable.HashMap()
  private val smaps: mutable.HashMap[Text, Optional[Smap]] = mutable.HashMap()

  def resolve(frame0: StackTrace.Frame): StackTrace.Frame =
    val frame = expand(frame0)

    tastyFile(frame.jvmClass).lay(frame): tasty =>
      tasty.path.lay(frame): path =>
        val hint = StackResolver.hint(frame.jvmMethod)
        val candidates = frame.line.lay(List[Tasty.Definition]())(tasty.covering(_))

        val definition =
          candidates.find(StackResolver.matches(frame.jvmMethod, hint, _)).optional
          . or(candidates.prim)

        val kind = hint.or(definition.let(_.kind)).or(Kind.Synthetic)
        val code = frame.line.let(sourceLine(path, _))

        val (owner, name) =
          definition.lay((frame.method.className, frame.method.method)): definition =>
            val chain = definition.owners.reverse.map(StackResolver.display)

            // Landing on a class means the line belongs to it but to none of the definitions it
            // records—typically a member the compiler generated whole. The class still names the
            // frame's owner, but only the compiled name can name the frame itself.
            if definition.kind == Kind.Class then
              val name = StackResolver.display(definition.name)
              ((chain :+ name).join(t"."), frame.method.method)
            else
              (chain.join(t"."), StackResolver.display(definition.name))

        frame.copy(source = StackTrace.Frame.Source(path, owner, name, kind, code))

  // The line a frame records may be a synthetic one the compiler allocated for inlined code, in
  // which case the classfile's SMAP says where that code was written and where it was inlined
  // from, level by level; the frame gets its real call-site line back, and the levels of inlining
  // beneath it, each further resolved against TASTy to name the inline method it falls inside.
  // Without an SMAP—or when the line is real—the frame stands as recorded. This runs before TASTy
  // resolution of the frame itself, which needs the real line to find the frame's definition.
  private def expand(frame: StackTrace.Frame): StackTrace.Frame =
    frame.line.lay(frame): line =>
      smap(frame.jvmClass).lay(frame): smap =>
        smap.expand(line).lay(frame): expansion =>
          val inlined = expansion.inlined.map: origin =>
            StackTrace.Frame.Inlined(origin.file, origin.path, origin.line, inlineSource(origin))

          frame.copy(line = expansion.line.or(frame.line), inlined = inlined)

  // Unlike TASTy, a classfile is written per class, so the frame's own class names it directly.
  // Mandible does the reading.
  private def smap(name: Text): Optional[Smap] =
    if name.s.isEmpty then Unset
    else smaps.synchronized(smaps.getOrElseUpdate(name, loadSmap(name)))

  private def loadSmap(name: Text): Optional[Smap] =
    val resource = (name.s.replace('.', '/').nn+".class").tt
    Classfile(resource).let(_.sourceDebugExtension).let(Smap.parse(_))

  // The definition an inline origin falls inside: the innermost definition covering its line in
  // the TASTy of the top-level class the SMAP's `ScalaClass` stratum names for it. The recorded
  // source paths are compared as a guard against a classpath whose classfiles and TASTy come
  // from different compilations. An origin with no named class — an SMAP from another compiler,
  // or an older one of ours — keeps its bare position.
  private def inlineSource(origin: Smap.Origin): Optional[StackTrace.Frame.Source] =
    origin.cls.let: cls =>
      tastyFile(cls).let: tasty =>
        tasty.path.let: path =>
          if path == origin.path then definitionSource(tasty, path, origin.line) else Unset

  private def definitionSource(tasty: Tasty.File, path: Text, line: Int)
  :   Optional[StackTrace.Frame.Source] =

    tasty.covering(line).prim.let: definition =>
      val owner = definition.owners.reverse.map(StackResolver.display).join(t".")
      val name = StackResolver.display(definition.name)

      StackTrace.Frame.Source(path, owner, name, definition.kind, sourceLine(path, line))

  private def tastyFile(name: Text): Optional[Tasty.File] =
    if name.s.isEmpty then Unset
    else tastyFiles.synchronized(tastyFiles.getOrElseUpdate(name, load(name.s)))

  // TASTy is written once per top-level class, so a frame in a nested, anonymous or module class
  // resolves through the outermost class enclosing it, which is what stripping `$`-separated
  // segments from the right arrives at. Top-level definitions live in a `<name>$package` class,
  // whose TASTy is found before any segment is stripped.
  private def load(name: String): Optional[Tasty.File] =
    val resource = (name.replace('.', '/').nn+".tasty").tt
    val loaded: Optional[Tasty.File] = classloader(resource).lay(Unset)(TastyFiles(_))

    loaded.or:
      name.lastIndexOf('$') match
        case -1 | 0 => Unset
        case index  => load(name.substring(0, index).nn)

  private def sourceLine(path: Text, line: Int): Optional[Text] =
    lines(path).let(_.stdlib.lift(line - 1).optional.let(_.trim))

  // The path TASTy records is the path the file was compiled from, which need not exist where the
  // code now runs; when it does not, the frame simply keeps everything but its line of source.
  private def lines(path: Text): Optional[List[Text]] =
    sourceFiles.synchronized:
      sourceFiles.getOrElseUpdate(path, safely(path.as[Path on Linux].read[Text].cut(t"\n")))
