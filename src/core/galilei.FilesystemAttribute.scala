package galilei

import java.nio.file as jnf

import serpentine.*

object FilesystemAttribute:
  object Readable
  extends FilesystemAttribute
      (path => jnf.Files.isReadable(path.javaPath), _.javaFile.setReadable(_))
  
  object Writable
  extends FilesystemAttribute
      (path => jnf.Files.isWritable(path.javaPath), _.javaFile.setWritable(_))
  
  object Executable
  extends FilesystemAttribute
      (path => jnf.Files.isExecutable(path.javaPath), _.javaFile.setExecutable(_))

case class FilesystemAttribute(get: Path => Boolean, set: (Path, Boolean) => Unit):
  def apply(path: Path): Target = Target(path)
  class Target(path: Path):
    def apply(): Boolean = get(path)
    def update(value: Boolean): Unit = set(path, value)