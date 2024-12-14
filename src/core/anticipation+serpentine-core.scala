package anticipation

import prepositional.*
import serpentine.*

package filesystemApi:
  given [PlatformType: {Navigable, Radical}]
      => (Path on PlatformType) is GenericPath & SpecificPath as serpentinePath =
    new GenericPath with SpecificPath:
      type Self = Path on PlatformType
      def pathText(path: Path on PlatformType): Text = path.text
      def path(text: Text): Path on PlatformType = Path.parse[PlatformType](text)
