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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package octogenarian

import fulminate.*

object GitError:
  enum Reason(val number: Int) extends Clarification:
    case CannotExecuteGit   extends Reason(1)
    case CloneFailed        extends Reason(2)
    case InvalidRepoPath    extends Reason(3)
    case RepoDoesNotExist   extends Reason(4)
    case BranchDoesNotExist extends Reason(5)
    case CommitDoesNotExist extends Reason(6)
    case CommitFailed       extends Reason(7)
    case CannotSwitchBranch extends Reason(8)
    case PullFailed         extends Reason(9)
    case BranchFailed       extends Reason(10)
    case TagFailed          extends Reason(11)
    case AddFailed          extends Reason(12)
    case NoWorkTree         extends Reason(13)
    case ResetFailed        extends Reason(14)
    case MvFailed           extends Reason(15)
    case ReflogFailed       extends Reason(16)
    case DiffFailed         extends Reason(17)
    case WorktreeFailed     extends Reason(18)

  import Reason.*

  given communicable: Reason is Communicable =
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
    case ResetFailed        => m"the reset operation failed"
    case MvFailed           => m"the move operation failed"
    case ReflogFailed       => m"the reflog could not be read"
    case DiffFailed         => m"the diff could not be computed"
    case WorktreeFailed     => m"the worktree operation failed"

case class GitError(reason: GitError.Reason)(using Diagnostics)
extends Error(realm"oc", 1, reason.number)
  ( m"the Git operation could not be completed because $reason" )
