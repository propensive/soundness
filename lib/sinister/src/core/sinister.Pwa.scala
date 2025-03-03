package sinister

import anticipation.*
import iridescence.*
import gossamer.*
import hallucination.*
import honeycomb.*
import jacinta.*
import prepositional.*
import serpentine.*
import vacuous.*

object Pwa:
  enum Orientation:
    case Any, Natural, Portrait, Landscape

  enum Display:
    case Standalone, MinimalUi, Browser, Fullscreen, WindowControlsOverlay

  object Display:
    given Display is Encodable in Text =
      case Standalone            => t"standalone"
      case MinimalUi             => t"minimal-ui"
      case Browser               => t"browser"
      case Fullscreen            => t"fullscreen"
      case WindowControlsOverlay => t"window-controls-overlay"

  enum Purpose:
    case Any, Monochrome, Maskable

  case class Icon(src: Text, sizes: Text, `type`: Text, purpose: Optional[Pwa.Purpose])

  case class Manifest
     (name:             Text,
      short_name:       Text,
      display:          Display,
      background_color: into Rgb24,
      theme_color:      into Rgb24,
      icons:            List[Icon],
      orientation:      Optional[Orientation],
      scope:            Optional[Path])

case class Pwa
   (name:        Text,
    shortName:   Optional[Text]            = Unset,
    display:     Pwa.Display               = Pwa.Display.Standalone,
    orientation: Optional[Pwa.Orientation] = Unset,
    icon:        Image):

  def manifest: Json =
    Pwa.Manifest
     (name        = name,
      description = description,
      short_name  = shortName.or(name),
      display     = display).json
