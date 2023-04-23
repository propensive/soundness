/*
    Zeppelin, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import serpentine.*
import diuretic.*
import anticipation.*, fileApi.javaIo
import imperial.*
import turbulence.*
import ambience.*
import deviation.*

import java.io as ji
import java.nio.file as jnf
import java.util.zip as juz

case class ZipError(filename: Text) extends Error(err"Could not create ZIP file ${filename}")

object ZipFile:
  def apply[FileType]
      (file: FileType)
      (using genericFileReader: /*{*}*/ GenericFileReader[FileType], streamCut: CanThrow[StreamCutError])
      : /*{genericFileReader, streamCut}*/ ZipFile =
    val pathname: String = genericFileReader.filePath(file)
    new ZipFile(pathname.show)

  def create[PathType]
      (path: PathType)
      (using genericPathReader: /*{*}*/ GenericPathReader[PathType], streamCut: CanThrow[StreamCutError])
      : /*{genericPathReader, streamCut}*/ ZipFile =
    val pathname: String = genericPathReader.getPath(path)
    val out: /*{genericPathReader}*/ juz.ZipOutputStream =
      juz.ZipOutputStream(ji.FileOutputStream(ji.File(pathname)))
    
    out.putNextEntry(juz.ZipEntry("/"))
    out.closeEntry()
    out.close()

    ZipFile(pathname.show)

case class ZipFile(private val filename: Text):
  def append
      [InstantType]
      (inputs: LazyList[ZipEntry], prefix: Maybe[Bytes] = Unset, timestamp: Maybe[InstantType] = Unset)
      (using env: Environment, instant: GenericInstant[InstantType] = timeApi.long, hierarchy: Hierarchy[GenericPath])
      : Unit throws ZipError | StreamCutError =
    
    val writeTimestamp: jnf.attribute.FileTime =
      jnf.attribute.FileTime.fromMillis(timestamp.mm(readInstant(_)).or(System.currentTimeMillis)).nn

    val tmpDir: ji.File = Xdg.Var.Tmp()
    val tmpFile: ji.File = ji.File.createTempFile("", ".zip", tmpDir).nn
    val uri: java.net.URI = java.net.URI.create(t"jar:file:${tmpFile.toString}".s).nn
  
    lazy val javaFs =
      try jnf.FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn
      catch case exception: jnf.ProviderNotFoundException => throw ZipError(tmpFile.getName.nn.show)

    val dirs = inputs.map(_.path).to(Set).flatMap: path =>
      try Set(path.parent) catch case err: PathError => Set()

    val dirs2 = dirs.flatMap: dir =>
      (0 to dir.elements.length).map(dir.elements.take(_)).map(GenericPath(_)).to(Set)
    .to(List).map(_.show+t"/").sorted

    dirs2.foreach: dir =>
      val dirPath = javaFs.getPath(dir.s).nn
      
      if jnf.Files.notExists(dirPath) then
        jnf.Files.createDirectory(dirPath)
        jnf.Files.setAttribute(dirPath, "creationTime", writeTimestamp)
        jnf.Files.setAttribute(dirPath, "lastAccessTime", writeTimestamp)
        jnf.Files.setAttribute(dirPath, "lastModifiedTime", writeTimestamp)

    inputs.foreach: entry =>
      val entryPath = javaFs.getPath(entry.path.show.s).nn
      val in = entry.resource().inputStream
      jnf.Files.copy(in, entryPath, jnf.StandardCopyOption.REPLACE_EXISTING)
      jnf.Files.setAttribute(entryPath, "creationTime", writeTimestamp)
      jnf.Files.setAttribute(entryPath, "lastAccessTime", writeTimestamp)
      jnf.Files.setAttribute(entryPath, "lastModifiedTime", writeTimestamp)
      
    javaFs.close()

    val fileOut = ji.BufferedOutputStream(ji.FileOutputStream(ji.File(filename.s)).nn)
    
    prefix.option.foreach: prefix =>
      fileOut.write(prefix.mutable(using Unsafe))
      fileOut.flush()
    
    fileOut.write(jnf.Files.readAllBytes(tmpFile.toPath.nn))
    fileOut.close()
    java.nio.file.Files.delete(tmpFile.toPath.nn)

  def entries(): LazyList[ZipEntry] throws StreamCutError =
    val zipFile = juz.ZipFile(ji.File(filename.s)).nn
    
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map: entry =>
      ZipEntry(unsafely(GenericPath.parse(entry.getName.nn.show)), zipFile.getInputStream(entry).nn)

case class ZipEntry(path: GenericPath, resource: () => LazyList[Bytes])

object ZipEntry:
  def apply
      [ResourceType](path: GenericPath, resource: ResourceType)(using Readable[ResourceType, Bytes])
      : ZipEntry =
    new ZipEntry(path, () => resource.stream[Bytes])

  given Readable[ZipEntry, Bytes] = Readable.lazyList[Bytes].contraMap(_.resource())

  // 00:00:00, 1 January 2000
  val epoch: jnf.attribute.FileTime = jnf.attribute.FileTime.fromMillis(946684800000L).nn
