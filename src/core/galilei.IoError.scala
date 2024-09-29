package galilei

import fulminate.*
import serpentine.*

object IoError:
  enum Operation:
    case Read, Write, Copy, Move, Delete, Metadata

  enum Reason:
    case PermissionDenied, Nonexistent, WrongType, AlreadyExists, IsNotDirectory, IsDirectory,
        DirectoryNotEmpty, NotSameVolume, Unsupported

  given Reason is Communicable =
    case Reason.PermissionDenied  => m"the user did not have sufficient permissions"
    case Reason.Nonexistent       => m"the entry does not exist"
    case Reason.WrongType         => m"the entry has the wrong type"
    case Reason.AlreadyExists     => m"an entry at this path already exists"
    case Reason.IsNotDirectory    => m"the entry is not a directory"
    case Reason.IsDirectory       => m"the entry is a directory"
    case Reason.DirectoryNotEmpty => m"the directory is not empty"
    case Reason.NotSameVolume     => m"the source and destination are on different volumes"
    case Reason.Unsupported       => m"the operation is not supported by the filesystem"

  given Operation is Communicable =
    case Operation.Read     => m"read"
    case Operation.Write    => m"write"
    case Operation.Copy     => m"copy"
    case Operation.Move     => m"move"
    case Operation.Delete   => m"delete"
    case Operation.Metadata => m"metadata"

case class IoError(path: Path, operation: IoError.Operation, reason: IoError.Reason)
    (using Diagnostics)
extends Error(m"the $operation operation on $path failed because $reason")