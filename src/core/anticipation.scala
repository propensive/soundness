package anticipation

import aviation.*

package timeApi:
  given aviationApi: (GenericInstant[Timing.Instant] & GenericDuration[Timing.Duration]) =
    new GenericInstant[Timing.Instant] with GenericDuration[Timing.Duration]:
      export Timing.Instant.generic.{makeInstant, readInstant}
      export Timing.Duration.generic.{makeDuration, readDuration}

