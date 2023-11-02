package exoskeleton

import profanity.*
import rudiments.*
import ambience.*
import anticipation.*
import turbulence.*

trait Executive[ReturnType, CliType <: Cli]:
  def cli
      (fullArguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
          context: ProcessContext)
      : CliType
  
  def process(cli: CliType, result: ReturnType): ExitStatus 

package executives:
  given direct: Executive[ExitStatus, CliInvocation] with
    
    def cli
        (arguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
            context: ProcessContext): CliInvocation =
      
      CliInvocation(Cli.arguments(arguments), environments.jvm, workingDirectories.default, context)

    def process(cli: CliInvocation, exitStatus: ExitStatus): ExitStatus = exitStatus

def application
    [ReturnType, CliType <: Cli]
    (using executive: Executive[ReturnType, CliType])
    (arguments: Iterable[Text])
    (block: Cli ?=> ReturnType)
    : Unit =

  block(using CliInvocation(Cli.arguments(arguments), environments.jvm, workingDirectories.default, ProcessContext(stdioSources.jvm)))
