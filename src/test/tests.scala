package eucalyptus

import probably.*
import anticipation.*
import gossamer.*
import spectacular.*
import parasite.*
import perforate.*, errorHandlers.throwUnsafely
import turbulence.*, stdioSources.jvm

object Tests extends Suite(t"Eucalyptus tests"):
  def run(): Unit =
    import Level.*
    supervise:
      given Log = Log:
        case Warn() => Err
        case Fail() => Out
    
      test(t"Log something"):
        given realm: Realm = realm"test"
        Log.fine(t"Fine message")
        Log.info(t"Info message")
        Log.warn(t"Warn message")
        Log.fail(t"Fail message")
      .assert()
      Thread.sleep(1000L)