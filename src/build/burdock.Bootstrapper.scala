package burdock

import anticipation.*
import hellenism.*
import prepositional.*
import turbulence.*
import zeppelin.*

object Bootstrapper:
  def update[SourceType: Readable by Bytes](data: SourceType, classpath: Classpath)
          : LazyList[Bytes] =
    ZipStream(data).map: entry =>
      println(entry)
      entry
