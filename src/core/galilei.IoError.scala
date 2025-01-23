/*
    Galilei, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import fulminate.*
import serpentine.*

object IoError:
  enum Operation:
    case Read, Write, Create, Copy, Move, Delete, Metadata, Open, Access

  enum Reason:
    case PermissionDenied, Nonexistent, AlreadyExists, IsNotDirectory, IsDirectory,
        DirectoryNotEmpty, NotSameVolume, Unsupported, Cycle

  given Reason is Communicable =
    case Reason.PermissionDenied  => m"the user did not have sufficient permissions"
    case Reason.Nonexistent       => m"the entry does not exist"
    case Reason.AlreadyExists     => m"an entry at this path already exists"
    case Reason.IsNotDirectory    => m"the entry is not a directory"
    case Reason.IsDirectory       => m"the entry is a directory"
    case Reason.DirectoryNotEmpty => m"the directory is not empty"
    case Reason.NotSameVolume     => m"the source and destination are on different volumes"
    case Reason.Unsupported       => m"it is not supported by the filesystem"
    case Reason.Cycle             => m"a cycle was detected on the filesystem"

  given Operation is Communicable =
    case Operation.Read     => m"read"
    case Operation.Access   => m"access"
    case Operation.Write    => m"write"
    case Operation.Open     => m"open"
    case Operation.Copy     => m"copy"
    case Operation.Create   => m"create"
    case Operation.Move     => m"move"
    case Operation.Delete   => m"delete"
    case Operation.Metadata => m"metadata"

case class IoError(path: Path, operation: IoError.Operation, reason: IoError.Reason)
   (using Diagnostics)
extends Error(m"the $operation operation on $path failed because $reason")
