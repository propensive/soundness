package cataract

import scintillate.*
import rudiments.*

given SimpleHandler[Stylesheet] = SimpleHandler("text/css; charset=utf-8", _.toString.bytes)