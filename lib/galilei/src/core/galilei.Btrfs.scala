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

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import serpentine.*
import vacuous.*

// The storage-filesystem axis of a path's type (issue #567): which filesystem an entry is
// stored on determines which metadata it can carry, orthogonally to the plane — `Btrfs` is
// meaningful only under `Linux`, `Apfs` under `MacOs` and `Linux`, `Ntfs` under `Windows` and
// `Linux`. A path cannot know its filesystem from its syntax, so the axis is established by
// runtime probing, through each filesystem's extractor — the axis is `over`'s structural
// `Transport` refinement, deliberately not a declared member of `Path`: refining it in is
// enough for the gated extensions below to dispatch, and declaring it perturbed inference at
// unrelated sites downstream:
//
//     path match
//       case Btrfs(path2) => …   // `path2: Path on Linux over Btrfs`
//       case _            => …
//
// The probe is the same platform query behind `path.volume()`, so no new backend method is
// needed; a probe which fails (an unreadable volume, say) simply does not match. Filesystems
// which record creation times extend `CreationTimed`, which gates a *total* `creation()`
// accessor — unlike `Stat.created`, whose optionality exists because most POSIX filesystems
// record none.
sealed trait CreationTimed

// Filesystems which support extended (user-defined) attributes, gating the typed `attribute`
// accessors (issue #567).
sealed trait Attributed

sealed trait Btrfs extends CreationTimed, Attributed
sealed trait Ext4 extends Attributed
sealed trait Apfs extends CreationTimed, Attributed
sealed trait Ntfs extends CreationTimed, Attributed

private def storageFormat[plane](path: Path on plane)(using backend: FilesystemBackend on plane)
:   Optional[Text] =
  safely(backend.volume(path).volumeType.lower)

object Btrfs:
  def unapply[plane](path: Path on plane)(using FilesystemBackend on plane)
  :   Option[Path on plane over Btrfs] =
    if storageFormat(path) == t"btrfs" then Some(path.asInstanceOf[Path on plane over Btrfs])
    else None

  // A btrfs subvolume, named by the directory it is rooted at (issue #567). Btrfs partitions
  // one filesystem into independently-snapshottable subtrees, so an entry's subvolume — not
  // just its volume — is part of where it is stored, and `Subvolume` is the type in which that
  // is answered.
  //
  // The subvolume's numeric ID, UUID and generation are not carried here: btrfs exposes those
  // only through `BTRFS_IOC_GET_SUBVOL_INFO`, an `ioctl` no filesystem backend can currently
  // issue, whereas the root — all `subvolume()` promises — is recoverable from `stat` alone.
  // They are the natural fields to add once an `ioctl` seam exists.
  case class Subvolume[plane](root: Path on plane over Btrfs)

object Ext4:
  def unapply[plane](path: Path on plane)(using FilesystemBackend on plane)
  :   Option[Path on plane over Ext4] =
    if storageFormat(path) == t"ext4" then Some(path.asInstanceOf[Path on plane over Ext4])
    else None

object Apfs:
  def unapply[plane](path: Path on plane)(using FilesystemBackend on plane)
  :   Option[Path on plane over Apfs] =
    if storageFormat(path) == t"apfs" then Some(path.asInstanceOf[Path on plane over Apfs])
    else None

object Ntfs:
  def unapply[plane](path: Path on plane)(using FilesystemBackend on plane)
  :   Option[Path on plane over Ntfs] =
    if storageFormat(path) == t"ntfs" then Some(path.asInstanceOf[Path on plane over Ntfs])
    else None
