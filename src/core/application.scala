package exoskeleton

import ambience.*
import anticipation.*
import rudiments.*
import profanity.*
import turbulence.*
import perforate.*

abstract class Application:
  import executives.direct
  def invoke(using Cli): ExitStatus
  def main(textArguments: IArray[Text]): Unit = application(textArguments)(invoke)