package example

import eucalyptus.*
import turbulence.*
import anticipation.*
import parasite.*, threadModels.virtual
import fulminate.*
import escapade.*
import contingency.*
import vacuous.*

import stdioSources.virtualMachine.ansi

given Realm = realm"example"

@main def run(): Unit =
  import logFormats.ansiStandard
  given Message is Loggable = safely(supervise(Log(Out))).or(Log.silent)

  Log.fine(msg"hello")
  Log.info(msg"world")
  Log.warn(msg"!")
  Log.fail(msg"!!!")
