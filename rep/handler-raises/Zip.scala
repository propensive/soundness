// zeppelin.core analogue — check.sh compiles this pass WITHOUT capture checking by default
// (zeppelin.core is not CC-enabled), or with `cczip` to test the all-CC mix.
package repzip

import repcontingency.*

class ZipError extends Exception

object Zipfile:
  def read(data: String): Int raises ZipError = 42
