// P8 negative: returning an outer exclusive capability from a method whose result is a
// fresh `any` must be rejected (unless the parameter is consumed) — the rule that confines
// capability streams where LazyList escapes could not be confined under plain CC.
//EXPECT: error
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

trait Source extends Mutable:
  update def next(): Int

def leak(source: Source^): Source^ = source
