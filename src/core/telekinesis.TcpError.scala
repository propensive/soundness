package telekinesis

import fulminate.*

case class TcpError()(using Diagnostics)
extends Error(m"there was a TCP connection error")
