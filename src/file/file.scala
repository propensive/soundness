package clairvoyant

trait DirectoryProvider[T]:
  def make(path: String): T
  def path(value: T): String

trait FileProvider[T]:
  def make(path: String): T
  def path(value: T): String

object files:
  given javaIo: FileProvider[java.io.File] with DirectoryProvider[java.io.File] with
    def make(path: String): java.io.File = java.io.File(path)
    def path(value: java.io.File): String = value.getAbsolutePath.nn
