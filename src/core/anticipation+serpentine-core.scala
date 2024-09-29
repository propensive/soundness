package anticipation

import serpentine.*
import prepositional.*
import spectacular.*

package filesystemApi:
  given Path is GenericPath = _.text
  
  given [PlatformType: Navigable] => Path on PlatformType is SpecificPath =
    _.decodeAs[Path on PlatformType]