package superlunary

import guillotine.*
import anthology.*
import eucalyptus.*
import contingency.*
import rudiments.*
import anticipation.*
import ambience.*, systemProperties.virtualMachine

object remote extends Dispatcher:
  type Result[OutputType] = OutputType

  val scalac: Scalac[3.4] = Scalac[3.4](List())

  protected def invoke[OutputType](dispatch: Dispatch[OutputType]): OutputType =
    import workingDirectories.virtualMachine
    import logging.silent
    
    dispatch.remote: input =>
      val cmd = sh"java -classpath ${dispatch.classpath()} ${dispatch.mainClass} $input"
      unsafely(cmd.exec[Text]())
