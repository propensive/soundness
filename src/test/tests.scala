package turbulence

import probably.*
import eucalyptus.*
import gossamer.*
import stdouts.stdout

import unsafeExceptions.canThrowAny
given Log(Everything |-> SystemOut)

object Tests extends Suite(t"Turbulence tests"):
  def run(using Runner): Unit =
    ()
