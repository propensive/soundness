package telekinesis

import anticipation.*
import gossamer.*
import prepositional.*

object TransferEncoding:
  given TransferEncoding is Encodable in Text = _.toString.tt.lower

enum TransferEncoding:
  case Chunked, Compress, Deflate, Gzip
