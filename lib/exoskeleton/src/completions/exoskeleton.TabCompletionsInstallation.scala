/*
    Exoskeleton, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import anticipation.*
import fulminate.*

enum TabCompletionsInstallation:
  case CommandNotOnPath(script: Text)
  case Shells
     (zsh:  TabCompletionsInstallation.InstallResult,
      bash: TabCompletionsInstallation.InstallResult,
      fish: TabCompletionsInstallation.InstallResult)

object TabCompletionsInstallation:
  given TabCompletionsInstallation is Communicable =
    case CommandNotOnPath(script) =>
      m"The ${script} command is not on the PATH, so completions scripts cannot be installed."

    case Shells(zsh, bash, fish) =>
      m"$zsh\n\n$bash\n\n$fish"

  object InstallResult:
    given InstallResult is Communicable =
      case Installed(shell, path) =>
        m"The $shell completion script was installed to $path."

      case AlreadyInstalled(shell, path) =>
        m"A $shell completion script already exists at $path."

      case NoWritableLocation(shell) =>
        m"No writable install location could be found for $shell completions."

      case ShellNotInstalled(shell) =>
        m"The $shell shell is not installed."

  enum InstallResult:
    case Installed(shell: Shell, path: Text)
    case AlreadyInstalled(shell: Shell, path: Text)
    case NoWritableLocation(shell: Shell)
    case ShellNotInstalled(shell: Shell)
