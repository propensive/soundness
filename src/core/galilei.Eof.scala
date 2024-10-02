package galilei

import java.nio.file as jnf

import prepositional.*
import serpentine.*
import contingency.*

case class Eof[PlatformType <: Filesystem](path: Path on PlatformType):
  def open[ResultType](lambda: Handle => ResultType, extraOptions: List[jnf.OpenOption] = Nil)
      (using read:        ReadAccess          = filesystemOptions.readAccess.enabled,
             write:       WriteAccess         = filesystemOptions.writeAccess.enabled,
             dereference: DereferenceSymlinks,
             create:      CreateNonexistent on PlatformType)
          : ResultType raises IoError =
    path.open[ResultType](lambda, jnf.StandardOpenOption.APPEND :: extraOptions)
