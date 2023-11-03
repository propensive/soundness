package eucalyptus

import guillotine.*
import rudiments.*
import parasite.*
import anticipation.*
import turbulence.*
import perforate.*
import hieroglyph.*, charEncoders.utf8

case class Syslog(tag: Text)

object Syslog:

  given (using Monitor): Appendable[Syslog, Text] = (syslog, stream) =>
    safely:
      Async:
        import workingDirectories.default
        import logging.silent
        val process: Process["logger", Unit] = sh"logger --tag ${syslog.tag}".fork[Unit]()
        process.stdin(stream)

package logging:
  given syslog(using realm: Realm, monitor: Monitor): Log = Log:
    case _ => Syslog(realm.name).sink