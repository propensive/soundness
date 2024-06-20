package example

import eucalyptus.*
import turbulence.*
import anticipation.*
import parasite.*, threadModels.virtual
import fulminate.*
import contingency.*
import vacuous.*

import stdioSources.virtualMachine.textOnly

given Realm = realm"example"

@main def run(): Unit =
  import logFormats.standard
  given Message is Loggable = safely(supervise(Log[Line](Out))).or(Log.silent)

  Log.info(msg"hello")
  Log.info(msg"!")
