package eucalyptus

import anticipation.*
import prepositional.*
import parasite.*

package logging:
  given [MessageType: Inscribable in Text](using Monitor) => MessageType is Loggable as syslog =
    Log.route[Text](Syslog())
