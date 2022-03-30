package clairvoyant

import java.nio.file as jnf
import java.io as ji

trait DirectoryProvider[T]:
  def make(path: String, readOnly: Boolean = false): Option[T]
  def path(value: T): String

trait FileProvider[T]:
  def make(path: String, readOnly: Boolean = false): Option[T]
  def path(value: T): String

object files:
  given javaNio: FileProvider[jnf.Path] with DirectoryProvider[jnf.Path] with
    def make(path: String, readOnly: Boolean = false): Option[jnf.Path] =
      try Some(jnf.Paths.get(path).nn)
      catch case err: jnf.InvalidPathException => None
    def path(value: jnf.Path): String = value.toAbsolutePath.nn.toString

  given javaIo: FileProvider[ji.File] with DirectoryProvider[ji.File] with
    def make(path: String, readOnly: Boolean = false): Option[ji.File] = Some(ji.File(path))
    def path(value: ji.File): String = value.getAbsolutePath.nn