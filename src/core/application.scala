/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import ambience.*
import anticipation.*
import rudiments.*
import profanity.*
import turbulence.*
import perforate.*

abstract class Application:
  protected given environment(using invocation: CliInvocation): Environment = invocation.environment
  protected given workingDirectory(using invocation: CliInvocation): WorkingDirectory = invocation.workingDirectory
  
  def invoke(using Cli): ExitStatus

  def main(textArguments: IArray[Text]): Unit =
    val context: ProcessContext = ProcessContext(Stdio(System.out, System.err, System.in))
    val workingDirectory = unsafely(workingDirectories.default)
    
    val arguments = textArguments.to(List).zipWithIndex.map: (text, index) =>
      Argument(index, text, Unset)

    val invocation = CliInvocation(arguments, environments.jvm, workingDirectory, context)
    
    invoke(using invocation) match
      case ExitStatus.Ok           => System.exit(0)
      case ExitStatus.Fail(status) => System.exit(1)