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
package mandible

import java.net as jn

import anticipation.*
import gossamer.*
import vacuous.*

// Resolves a JVM internal class name to its declaration surface, over content held in memory
// plus a classpath of textual entries. Membership keying (LIRA §11.2 requirement 4) has to walk
// the supertype closure of every atomized class, and most of that closure lives in the
// *dependencies*, not in the release being atomized — so the release's own classes are indexed
// eagerly and everything else is resolved lazily through the classpath.
object ClasspathIndex:
  // A directory entry's URL must end in `/` for `URLClassLoader` to treat it as a directory;
  // `File.toURI` appends one for a path that exists on disk, and a classpath entry that does not
  // exist resolves nothing either way.
  private def url(entry: Text): jn.URL = java.io.File(entry.s).toURI.nn.toURL.nn

  def apply(local: Map[Text, ClassSurface], classpath: List[Text]): ClasspathIndex =
    val urls: scala.Array[jn.URL | Null] =
      scala.Array.from(classpath.stdlib.map(url(_)))

    new ClasspathIndex
      ( local,
        jn.URLClassLoader(urls, ClassLoader.getPlatformClassLoader().nn) )

class ClasspathIndex private (local: Map[Text, ClassSurface], classloader: jn.URLClassLoader):
  // Resolution is memoized — and negative results are memoized too. A supertype walk revisits
  // `java/lang/Object` once per atomized class, and a missing supertype is asked for exactly as
  // often, so caching both is what keeps atomization linear in the release's size.
  private val cache = scala.collection.mutable.HashMap[Text, Optional[ClassSurface]]()

  // The classes carried by the release itself, which are the ones that get atomized. Held
  // separately from the cache so `local` is never shadowed by a same-named class that happens to
  // be earlier on the dependency classpath.
  def own: List[Text] = local.stdlib.keys.toList.sortBy(_.s).to(List)

  def apply(name: Text): Optional[ClassSurface] =
    cache.getOrElseUpdate(name, resolve(name))

  private def resolve(name: Text): Optional[ClassSurface] =
    local.stdlib.get(name).map(Optional(_)).getOrElse:
      Optional(classloader.getResourceAsStream(t"$name.class".s)).let: stream =>
        // Same erasure: the bytes are read and never retained, so no freezing capability is
        // needed to hand them to the parser.
        try ClassSurface(stream.readAllBytes().nn.asInstanceOf[scala.IArray[Byte]])
        finally stream.close()

  // The transitive supertype closure of `name`, nearest first, excluding `name` itself. Classes
  // that cannot be resolved are reported rather than skipped: an unresolvable supertype means
  // the member set is *unknown*, and silently atomizing a smaller one would understate the
  // interface.
  def closure(name: Text): (List[Text], List[Text]) =
    val seen = scala.collection.mutable.LinkedHashSet[Text]()
    val missing = scala.collection.mutable.LinkedHashSet[Text]()

    def recur(todo: List[Text]): Unit = todo match
      case Nil => ()

      case next :: rest =>
        if seen.contains(next) || missing.contains(next) then recur(rest) else apply(next) match
          case surface: ClassSurface =>
            seen += next
            recur((rest.stdlib ++ surface.supertypes.stdlib).to(List))

          case _ =>
            missing += next
            recur(rest)

    apply(name).let: surface => recur(surface.supertypes)

    (seen.toList.to(List), missing.toList.to(List))
