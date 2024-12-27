/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package soundness

export galilei.{CopyAttributes, Makable, CreateNonexistent, CreateNonexistentParents,
    DeleteRecursively, DereferenceSymlinks, Dos, DosDrive, Entry, Filesystem,
    FilesystemAttribute, Handle, IoError, IoEvent, Linux, MacOs, MoveAtomically, Openable,
    OverwritePreexisting, Posix, ReadAccess, Socket, Symlink, TraversalOrder, Volume, Windows,
    WindowsDrive, WriteAccess, WriteSynchronously, C, D, %, $, open, javaPath, javaFile,
    children, descendants, size, delete, wipe, volume, hardLinkTo, entry, copyTo, copyInto,
    renameTo, moveTo, moveInto, symlinkTo, symlinkInto, modified, accessed, readable, writable,
    hidden, touch, make, created, executable, hardLinks}

package pathNavigation:
  export galilei.pathNavigation.{linux, windows, macOs, posix, operatingSystem}

package filesystemOptions:
  export galilei.filesystemOptions.{readAccess, writeAccess, dereferenceSymlinks, moveAtomically,
      copyAttributes, deleteRecursively, overwritePreexisting, createNonexistentParents,
      createNonexistent, writeSynchronously}
