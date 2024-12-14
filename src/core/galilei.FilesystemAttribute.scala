package galilei

import java.nio.file as jnf

import prepositional.*
import serpentine.*

object FilesystemAttribute:
  class Readable[PlatformType <: Filesystem](path: Path on PlatformType):
    def apply(): Boolean = jnf.Files.isReadable(path.javaPath)
    def update(value: Boolean): Unit = path.javaFile.setReadable(value)
  
  class Writable[PlatformType <: Filesystem](path: Path on PlatformType):
    def apply(): Boolean = jnf.Files.isWritable(path.javaPath)
    def update(value: Boolean): Unit = path.javaFile.setWritable(value)
  
  class Executable[PlatformType <: Posix](path: Path on PlatformType):
    def apply(): Boolean = jnf.Files.isExecutable(path.javaPath)
    def update(value: Boolean): Unit = path.javaFile.setExecutable(value)
