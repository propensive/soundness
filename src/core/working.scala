package rudiments

import ambience.*
import anticipation.*

package workingDirectory:
  given jvm(using CanThrow[SystemPropertyError], SystemProperties): WorkingDirectory =
    WorkingDirectory(Properties.user.dir[Text]())
