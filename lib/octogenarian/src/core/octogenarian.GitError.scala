/*
    Octogenarian, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package octogenarian

import scala.compiletime.*

import fulminate.*

object GitError:
  enum Reason:
    case CannotExecuteGit, CloneFailed, InvalidRepoPath, RepoDoesNotExist, BranchDoesNotExist,
        CommitDoesNotExist, CommitFailed, CannotSwitchBranch, PullFailed, BranchFailed, TagFailed,
        AddFailed, NoWorkTree

  import Reason.*

  given Reason is Communicable =
    case CannotExecuteGit   => m"the `git` command could not be executed"
    case CloneFailed        => m"the repository could not be cloned"
    case InvalidRepoPath    => m"the repository path was not valid"
    case RepoDoesNotExist   => m"the repository does not exist"
    case BranchDoesNotExist => m"the branch does not exist"
    case CommitDoesNotExist => m"the commit does not exist"
    case CommitFailed       => m"the commit could not be created"
    case AddFailed          => m"the path could not be added"
    case PullFailed         => m"the pull operation did not complete"
    case BranchFailed       => m"the new branch could not be created"
    case TagFailed          => m"the new tag could not be created"
    case NoWorkTree         => m"this bare repository does not have a work tree"
    case CannotSwitchBranch => m"the branch could not be changed"

case class GitError(reason: GitError.Reason)(using Diagnostics)
extends Error(m"the Git operation could not be completed because $reason")
