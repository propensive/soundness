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
import galilei.*
import anticipation.*, fileApi.galileiApi
import imperial.*
import diuretic.*
import turbulence.*
import ambience.*
import deviation.*

import java.io as ji
import java.nio.file as jnf
import java.util.zip as juz

case class ZipError(file: File) extends Error(err"Could not create ZIP file ${file.fullname}")

object ZipFile:
  // FIXME: Remove InvalidPathError
  def apply[FileType: GenericFileReader](file: FileType)(using fs: Filesystem): ZipFile throws IoError | InvalidPathError | StreamCutError =
    new ZipFile(fs.parse(readGenericFile(file).show).file(Ensure))

  def create(path: DiskPath): ZipFile throws IoError | StreamCutError =
    val out = juz.ZipOutputStream(ji.FileOutputStream(path.javaFile))
    out.putNextEntry(juz.ZipEntry("/"))
    out.closeEntry()
    out.close()

    ZipFile(path.file(Expect))

case class ZipFile(file: File):
  def append
      [InstantType]
      (inputs: LazyList[ZipEntry], prefix: Maybe[Bytes] = Unset, timestamp: Maybe[InstantType] = Unset)
      (using env: Environment, instant: GenericInstant[InstantType] = timeApi.long, fs: Filesystem)
      : Unit throws IoError | ZipError | StreamCutError =
    
    val writeTimestamp: jnf.attribute.FileTime =
      jnf.attribute.FileTime.fromMillis(timestamp.mm(readInstant(_)).or(System.currentTimeMillis)).nn

    val tmpDir: Directory = Xdg.Var.Tmp().directory(Ensure)
    val tmpPath = tmpDir.tmpPath()
    val tmpFile: File = file.copyTo(tmpPath)
    val uri: java.net.URI = java.net.URI.create(t"jar:file:${tmpPath.fullname}".s).nn
  
    lazy val javaFs =
      try jnf.FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn
      catch case exception: jnf.ProviderNotFoundException => throw ZipError(tmpFile)

    
    val dirs = unsafely(inputs.map(_.path).map(_.parent)).to(Set).flatMap: dir =>
      (0 to dir.parts.length).map(dir.parts.take(_)).map(Relative(0, _)).to(Set)
    .to(List).map(_.show+t"/").sorted

    dirs.foreach: dir =>
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

    val fileOut = ji.BufferedOutputStream(ji.FileOutputStream(file.javaFile).nn)
    
    prefix.option.foreach: prefix =>
      fileOut.write(prefix.mutable(using Unsafe))
      fileOut.flush()
    
    fileOut.write(jnf.Files.readAllBytes(tmpPath.javaPath))
    fileOut.close()
    java.nio.file.Files.delete(tmpPath.javaPath)

  def entries(): LazyList[ZipEntry] throws StreamCutError =
    val zipFile = juz.ZipFile(file.javaFile).nn
    
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map: entry =>
      ZipEntry(Relative.parse(entry.getName.nn.show), zipFile.getInputStream(entry).nn)

case class ZipEntry(path: Relative, resource: () => LazyList[Bytes])

object ZipEntry:
  def apply
      [ResourceType](path: Relative, resource: ResourceType)(using Readable[ResourceType, Bytes])
      : ZipEntry =
    new ZipEntry(path, () => resource.stream[Bytes])

  given Readable[ZipEntry, Bytes] = Readable.lazyList[Bytes].contraMap(_.resource())

  // 00:00:00, 1 January 2000
  val epoch: jnf.attribute.FileTime = jnf.attribute.FileTime.fromMillis(946684800000L)
