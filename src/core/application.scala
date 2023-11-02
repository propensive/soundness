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