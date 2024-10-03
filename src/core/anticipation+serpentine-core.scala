package anticipation

import serpentine.*
import prepositional.*

package filesystemApi:
  given [PlatformType: Navigable]
      => (Path on PlatformType) is GenericPath & SpecificPath as serpentinePath =
    new GenericPath with SpecificPath:
      type Self = Path on PlatformType
      def pathText(path: Path on PlatformType): Text = path.text
      def path(text: Text): Path on PlatformType = Path.parse[PlatformType](text)