package rudiments

import scala.quoted.*

def fail(using q: Quotes)(msg: String, pos: Maybe[q.reflect.Position] = Unset): Nothing =
  import quotes.reflect.*
  
  // Note that this might not be the most reliable way to get the package name, but it appears to
  // work in all the examples that have been tested.
  val pkg = Thread.currentThread.nn.getStackTrace.nn(2).nn.getClassName.nn.split("\\.").nn(0).nn
  pos.mm(report.errorAndAbort(pkg+": "+msg, _)).or(report.errorAndAbort(pkg+": "+msg))
