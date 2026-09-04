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
package soundness

export
  galilei
  . { accessed, Apfs, attribute, attributes, Attributed, BlockDevice, Btrfs, C, CharDevice, children, CopyAttributes, copyInto,
      copyTo, created, CreateFlag, CreateNonexistentParents, Creation, creation, CreationTimed,
      D, delete, Ext4,
      DeleteRecursively, DereferenceSymlinks, descendants, dir, Directory, Dos, Drive, Entry,
      entry, entryIdentity, executable, expanse, Explorable, existent, Fifo, file, File,
      FileOpenable,
      FilesystemAttribute, FilesystemBackend,
      glob, Handle, hardLinks, hardLinkTo, hidden, Io, Linux, Local, locations,
      dataSearch, configSearch, destination, listing, search,
      MacOs, modified, MoveAtomically, moveInto, moveTo, Ntfs, OpenFlag,
      OverwritePreexisting, p, Platform, Posix, readable, filesize, Sock, Stat,
      Scratch, Searchpaths, searchpathCompliant, Shared, Slice, Substantiable, Subtree, subvolume,
      subvolumeRoot, Symlink, symlinkInto, symlinkTo, touch,
      TraversalOrder,
      UnixEntry, Volume, volume, Windows, WindowsEntry, wipe, writable, write }

package interfaces.paths:
  export
    anticipation.interfaces.paths
    . { pathOnLinux, pathOnLocal, pathOnMacOs, pathOnPosix, pathOnWindows }

package filesystemOptions:
  export
    galilei.filesystemOptions
    . { copyAttributes, createNonexistentParents, deleteOnlyEmpty, deleteRecursively,
        dereferenceSymlinks, discardAttributes, failOnPreexisting, moveAtomically, moveNonAtomically,
        overwritePreexisting, preserveSymlinks, requireParents }

package filesystemTraversal:
  export galilei.filesystemTraversal.{postOrderTraversal, preOrderTraversal}
