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
