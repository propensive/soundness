package diuretic

import anticipation.*

import java.time as jt
import java.io as ji
import java.nio.file as jnf
import java.net as jn
import java.util as ju

object JavaTime extends GenericInstant[jt.Instant], GenericDuration[Long]:
  def readInstant(value: jt.Instant): Long = value.toEpochMilli
  def makeInstant(long: Long): jt.Instant = jt.Instant.ofEpochMilli(long).nn
  def readDuration(long: Long): Long = long
  def makeDuration(value: Long): Long = value

object JavaLongTime extends GenericInstant[Long], GenericDuration[Long]:
  def readInstant(long: Long): Long = long
  def makeInstant(value: Long): Long = value
  def readDuration(long: Long): Long = long
  def makeDuration(value: Long): Long = value

object JavaUtilTime extends GenericInstant[ju.Date], GenericDuration[Long]:
  def makeInstant(long: Long): ju.Date = ju.Date(long)
  def readInstant(value: ju.Date): Long = value.getTime
  def readDuration(long: Long): Long = long
  def makeDuration(value: Long): Long = value

object JavaNioFile extends GenericFileMaker[jnf.Path], GenericDirectoryMaker[jnf.Path],
    GenericDirectoryReader[jnf.Path], GenericFileReader[jnf.Path], GenericPathMaker[jnf.Path],
    GenericPathReader[jnf.Path]:

  def makePath(path: String, readOnly: Boolean = false): Option[jnf.Path] =
    try Some(jnf.Paths.get(path).nn) catch case err: jnf.InvalidPathException => None

  def makeFile(path: String, readOnly: Boolean = false): Option[jnf.Path] = makePath(path, readOnly)
  def makeDirectory(path: String, readOnly: Boolean = false): Option[jnf.Path] = makePath(path, readOnly)

  def getPath(value: jnf.Path): String = value.toAbsolutePath.nn.toString
  def filePath(value: jnf.Path): String = getPath(value)
  def directoryPath(value: jnf.Path): String = getPath(value)

object JavaIoFile extends GenericFileMaker[ji.File], GenericDirectoryMaker[ji.File],
    GenericDirectoryReader[ji.File], GenericFileReader[ji.File], GenericPathMaker[ji.File],
    GenericPathReader[ji.File]:

  def makePath(path: String, readOnly: Boolean = false): Option[ji.File] =
    try Some(ji.File(path).nn) catch case err: jnf.InvalidPathException => None

  def makeFile(path: String, readOnly: Boolean = false): Option[ji.File] = makePath(path, readOnly)
  def makeDirectory(path: String, readOnly: Boolean = false): Option[ji.File] = makePath(path, readOnly)

  def getPath(value: ji.File): String = value.getAbsolutePath.nn.toString
  def filePath(value: ji.File): String = getPath(value)
  def directoryPath(value: ji.File): String = getPath(value)

object JavaNetUrl extends GenericUrl[jn.URL]:
  def readUrl(value: jn.URL): String = value.toString
  def makeUrl(string: String): jn.URL = jn.URI(string).nn.toURL().nn