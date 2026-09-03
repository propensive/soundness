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
package galilei

import ambience.*
import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

// The joining machinery for searchpath planes (issue #602): an `ambience.Searchpath` plane —
// `Xdg.Data`, `Xdg.Config` — is an ordered list of real directories presented as one
// searchable, navigable tree. Resolution onto the real platform goes through a contextual
// `Stems`, the searchpath analogue of a `Directory.Handle`'s single stem.
object Searchpaths:
  // The ordered real directories a searchpath plane resolves onto: `Searchpaths.Stems on
  // Xdg.Data onto Linux`. `Target` — prepositional's `onto` — is the platform beneath the
  // virtual plane, and the head stem is the writable one: the XDG Base Directory spec directs
  // applications to write only to the user-specific base directory.
  trait Stems extends Planar, Targetable:
    type Plane <: Searchpath
    type Target <: Platform
    def stems: List[Path on Target]

  // Public helpers, not `private`: the transparent-inline operations below would otherwise
  // generate inline-accessor bridges whose fresh capability roots fail capture checking (as
  // documented on `Subtree`).
  def resolve[target <: Platform](stem: Path on target, path: Path on ?): Path on target =
    path.descent.reverse.foldLeft(stem): (parent, name) => parent.child(name)(using Unsafe)

  def locateResolved[target <: Platform: Filesystem]
    ( stems: List[Path on target], path: Path on ? )
    ( using backend: FilesystemBackend on target )
  :   Optional[Path on target] =

    stems.map(resolve(_, path)).seek(galilei.existent(_)())

  def locationsResolved[target <: Platform: Filesystem]
    ( stems: List[Path on target], path: Path on ? )
    ( using backend: FilesystemBackend on target )
  :   List[Path on target] =

    stems.map(resolve(_, path)).filter(galilei.existent(_)())

  // The merged listing's names: the union of children across all stems, deduplicated, with
  // earlier stems shadowing later ones — insertion order preserves precedence.
  def entryNames[target <: Platform: Filesystem]
    ( stems: List[Path on target], path: Path on ? )
    ( using explorable: target is Explorable,
            backend:    FilesystemBackend on target,
            tactic:     Tactic[Io.Error] )
  :   List[Text] =

    val names = scala.collection.mutable.LinkedHashSet[Text]()

    stems.each: stem =>
      val resolved = resolve(stem, path)

      if galilei.existent(resolved)() then resolved.children.each: child =>
        names.add(child.name)

    scala.List.from(names).to(List)

// The operation names are chosen not to collide with anything reachable at the `soundness`
// umbrella or through companion extension scope: zephyrine exports a toplevel `locate`, and
// `Subtree`'s companion supplies `entries` and `extant` for directory-handle paths, which a
// lexical toplevel of the same name would shadow and break (extensions do not fall through on
// failure).
extension [plane <: Searchpath](path: Path on plane)
  // The first stem in which the path exists — first-match-wins, per the XDG Base Directory
  // spec.
  transparent inline def search()
    ( using stems: Searchpaths.Stems { type Plane = plane } )
    ( using filesystem: stems.Target is Filesystem )
    ( using backend: FilesystemBackend on stems.Target )
  :   Optional[Path on stems.Target] =

    Searchpaths.locateResolved[stems.Target](stems.stems, path)

  // Every extant match, in precedence order, for cascade-merging consumers.
  transparent inline def locations()
    ( using stems: Searchpaths.Stems { type Plane = plane } )
    ( using filesystem: stems.Target is Filesystem )
    ( using backend: FilesystemBackend on stems.Target )
  :   List[Path on stems.Target] =

    Searchpaths.locationsResolved[stems.Target](stems.stems, path)

  // The merged directory listing: children across all stems, deduplicated by name, earlier
  // stems shadowing later ones. This is what makes the virtual tree navigable.
  transparent inline def listing()
    ( using stems: Searchpaths.Stems { type Plane = plane } )
    ( using filesystem: stems.Target is Filesystem )
    ( using explorable: stems.Target is Explorable )
    ( using backend: FilesystemBackend on stems.Target, tactic: Tactic[Io.Error] )
  :   List[Path on plane] =

    Searchpaths.entryNames[stems.Target](stems.stems, path).map: name =>
      path.child(name)(using Unsafe)

  // The head-stem realization of the path — where the XDG spec directs writes. Creating any
  // missing intermediate directories is the caller's, e.g. through galilei's
  // `createNonexistentParents` option on the write itself.
  transparent inline def destination()
    ( using stems: Searchpaths.Stems { type Plane = plane } )
    ( using filesystem: stems.Target is Filesystem )
  :   Path on stems.Target =

    // stdlib bridge: the `Stems` contract requires a head stem, but `stems` carries no
    // `Populated` proof, so the total read is unavailable natively.
    Searchpaths.resolve[stems.Target](stems.stems.stdlib.head, path)

// A `Relative` or literal path written against `Linux` conventions may be reinterpreted on a
// searchpath plane; library-qualified and top-level, since neither subject's companion can
// host it (`Searchpath`'s is in ambience, below serpentine's `Compliant`).
inline given searchpathCompliant: [plane <: Searchpath] => Linux is Compliant on plane = !!

extension (xdg: Xdg.type)
  // The XDG data searchpath, in spec precedence order — `dataHome`, then each entry of
  // `dataDirs` — with the environment read once, at construction. An unparseable set of
  // directories falls back to the spec defaults, per `Xdg.dataDirs`.
  def dataSearch()(using Environment, System)
  :   Searchpaths.Stems { type Plane = Xdg.Data; type Target = Linux } =

    import interfaces.paths.pathOnLinux

    new Searchpaths.Stems:
      type Plane = Xdg.Data
      type Target = Linux
      // `unsafely`: the instantiable's decode tactic. `Xdg`'s own accessors guard each
      // variable with `safely`, falling back to the spec defaults, so a malformed entry
      // cannot actually raise here.
      val stems: List[Path on Linux] =
        unsafely(Xdg.dataHome[Path on Linux] :: Xdg.dataDirs[Path on Linux])

  def configSearch()(using Environment, System)
  :   Searchpaths.Stems { type Plane = Xdg.Config; type Target = Linux } =

    import interfaces.paths.pathOnLinux

    new Searchpaths.Stems:
      type Plane = Xdg.Config
      type Target = Linux
      val stems: List[Path on Linux] =
        unsafely(Xdg.configHome[Path on Linux] :: Xdg.configDirs[Path on Linux])
