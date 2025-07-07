// zeppelin test-suite analogue — compiled WITH capture checking against the two previous passes.
// The reproduction: a raises-method applied inside `capture`'s block, where the tactic given is
// the block's own context-lambda parameter.
package repuse

import language.experimental.captureChecking
import repcontingency.*
import repzip.*

def use(): Exception =
  capture[ZipError]:
    Zipfile.read("not a zip")
