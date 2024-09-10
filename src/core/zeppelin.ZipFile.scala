/*
    Zeppelin, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package zeppelin

import rudiments.*
import gossamer.*
import vacuous.*
import contingency.*
import serpentine.*
import feudalism.*
import fulminate.*
import anticipation.*
import turbulence.*
import spectacular.*
import ambience.*

import scala.collection.concurrent as scc
import scala.collection.mutable as scm

import java.io as ji
import java.nio.file as jnf
import java.net as jn
import java.util.zip as juz

object ZipFile:
  def apply[FileType: GenericFile](file: FileType): ZipFile raises StreamError =
    val name: Text = file.fileText
    new ZipFile(name)

  def create[PathType: GenericPath](path: PathType): ZipFile raises StreamError =
    val name: Text = path.pathText
    val out: juz.ZipOutputStream = juz.ZipOutputStream(ji.FileOutputStream(ji.File(name.s)))

    out.putNextEntry(juz.ZipEntry("/"))
    out.closeEntry()
    out.close()

    ZipFile(name.show)

  private val cache: scc.TrieMap[Text, Semaphore] = scc.TrieMap()

case class ZipFile(private val filename: Text):
  private lazy val zipFile: juz.ZipFile = juz.ZipFile(ji.File(filename.s)).nn

  private val filesystemUri: jn.URI = jn.URI.create(t"jar:file:$filename".s).nn

  @targetName("child")
  infix def / (name: Name[InvalidZipNames]): ZipPath = ZipPath(this, ZipRef(List(name)))

  private def semaphore: Semaphore = ZipFile.cache.getOrElseUpdate(filename, Semaphore())

  private def withFilesystem[ResultType](lambda: jnf.FileSystem => ResultType): ResultType raises ZipError =
    semaphore.isolate:
      val filesystem =
        try jnf.FileSystems.newFileSystem(filesystemUri, Map("zipinfo-time" -> "false").asJava).nn
        catch case exception: jnf.ProviderNotFoundException => abort(ZipError(filename))

      lambda(filesystem).also(filesystem.close())

  def entry(ref: ZipRef): ZipEntry raises StreamError =
    semaphore.access(ZipEntry(ref, zipFile.getInputStream(zipFile.getEntry(ref.render.s).nn).nn))

  def append[InstantType: GenericInstant](entries: LazyList[ZipEntry], timestamp: Optional[InstantType] = Unset)
      (using Environment)
          : Unit raises ZipError raises StreamError =

    val writeTimestamp: jnf.attribute.FileTime =
      jnf.attribute.FileTime.fromMillis(timestamp.let(_.millisecondsSinceEpoch).or(System.currentTimeMillis)).nn

    withFilesystem: filesystem =>
      val directories: scm.HashSet[ZipRef] = scm.HashSet()

      def addParents(ref: ZipRef): Unit = ref.parent.let: parent =>
        if !directories.contains(parent) then
          addParents(parent)
          val directoryPath = filesystem.getPath(t"${parent.render}/".s).nn
          if jnf.Files.notExists(directoryPath) then
            jnf.Files.createDirectory(directoryPath)
            jnf.Files.setAttribute(directoryPath, "creationTime", writeTimestamp)
            jnf.Files.setAttribute(directoryPath, "lastAccessTime", writeTimestamp)
            jnf.Files.setAttribute(directoryPath, "lastModifiedTime", writeTimestamp)
          directories += parent

      entries.each: entry =>
        addParents(entry.ref)
        val entryPath = filesystem.getPath(entry.ref.render.s).nn
        val in = LazyListInputStream(entry.content())

        jnf.Files.copy(in, entryPath, jnf.StandardCopyOption.REPLACE_EXISTING)
        jnf.Files.setAttribute(entryPath, "creationTime", writeTimestamp)
        jnf.Files.setAttribute(entryPath, "lastAccessTime", writeTimestamp)
        jnf.Files.setAttribute(entryPath, "lastModifiedTime", writeTimestamp)

  def entries(): LazyList[ZipEntry] raises StreamError =
    zipFile.entries.nn.asScala.filter(!_.getName.nn.endsWith("/")).to(LazyList).map: entry =>
      ZipEntry(unsafely(ZipRef(entry.getName.nn.show)), zipFile.getInputStream(entry).nn)
