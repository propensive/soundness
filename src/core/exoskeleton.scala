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

import anticipation.*
import rudiments.*
import perforate.*
import turbulence.*
import spectacular.*
import gossamer.*
import ambience.*

import sun.misc as sm

object Stdin:
  given decoder: Decoder[Stdin] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[Stdin] = _.toString.tt.lower

enum Stdin:
  case Term, Pipe

sealed trait CliContext:
  def args: IArray[Text]
  def environment: Environment
  def workingDirectory: WorkingDirectory

case class CommandLine
    (args: IArray[Text], currentArg: Int, argPosition: Int, environment: Environment,
        workingDirectory: WorkingDirectory)
extends CliContext

case class Invocation
    (args: IArray[Text], environment: Environment, workingDirectory: WorkingDirectory, stdin: LazyList[Bytes],
        stdout: LazyList[Bytes] => Unit, stderr: LazyList[Bytes] => Unit)
extends CliContext:
  def signals(signals: Signal*): LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    
    signals.foreach: signal =>
      sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal))
    
    funnel.stream

abstract class Application:
  protected given environment(using invocation: Invocation): Environment = invocation.environment
  protected given workingDirectory(using invocation: Invocation): WorkingDirectory = invocation.workingDirectory
  
  protected given stdio(using invocation: Invocation): Stdio with
    def putErrBytes(bytes: Bytes): Unit = System.err.nn.write(bytes.mutable(using Unsafe))
    def putOutBytes(bytes: Bytes): Unit = System.out.nn.write(bytes.mutable(using Unsafe))
    def putErrText(text: Text): Unit = System.err.nn.print(text.s)
    def putOutText(text: Text): Unit = System.out.nn.print(text.s)
  
  def invoke(using CliContext): Execution

  def main(args: IArray[Text]): Unit =
    def in: LazyList[Bytes] = safely(System.in.nn.stream[Bytes]).or(LazyList())
    
    def stdout(stream: LazyList[Bytes]): Unit = stream.foreach: bytes =>
      System.out.nn.write(bytes.mutable(using Unsafe))
    
    def stderr(stream: LazyList[Bytes]): Unit = stream.foreach: bytes =>
      System.err.nn.write(bytes.mutable(using Unsafe))

    val invocation = Invocation(args, environments.jvm, workingDirectories.default, in, stdout(_),
        stderr(_))
    
    invoke(using invocation).execute(invocation) match
      case ExitStatus.Ok           => System.exit(0)
      case ExitStatus.Fail(status) => System.exit(1)

case class Execution(execute: Invocation => ExitStatus)

def execute(block: Invocation ?=> ExitStatus): Execution = Execution(block(using _))

