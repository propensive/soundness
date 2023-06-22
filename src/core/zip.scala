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
import spectacular.*
import ambience.*
import digression.*

import scala.collection.mutable as scm

import java.io as ji
import java.nio.file as jnf
import java.util.zip as juz

import scala.language.experimental.pureFunctions


case class ZipError(filename: Text) extends Error(err"Could not create ZIP file ${filename}")

// FIXME: Check this
type InvalidZipNames = ".*'.*" | ".*`.*" | ".*\\/.*" | ".*\\\\.*"

// object ZipPath:
//   given AbsoluteReachable[ZipPath, InvalidZipNames](t"/") with
//     type Root = ZipFile
//     def root(path: ZipPath): ZipFile = path.zipfile
//     def descent(path: ZipPath): List[PathName[InvalidZipNames]] = path.descent
//     def parseRoot(text: Text): Maybe[(ZipFile, Text)] = ???

// case class ZipPath(zipFile: ZipFile, descent: List[PathName[InvalidZipNames]])

object ZipRef:
  def apply(text: Text): ZipRef throws PathError = reachable.parse(text)
  def /(name: PathName[InvalidZipNames]): ZipRef = ZipRef(List(name))
  
  given reachable: ParsableReachable[ZipRef, InvalidZipNames, "/"] with
    type Root = ZipRef.type
    def root(path: ZipRef): ZipRef.type = ZipRef
    def descent(path: ZipRef): List[PathName[InvalidZipNames]] = path.descent
    def parseRoot(text: Text): (ZipRef.type, Text) = (ZipRef, text.drop(1))
    def make(root: ZipRef.type, descent: List[PathName[InvalidZipNames]]) = ZipRef(descent)
    def prefix(ref: ZipRef.type): Text = t"/"

case class ZipRef(descent: List[PathName[InvalidZipNames]])

case class ZipEntry(ref: ZipRef, content: () -> LazyList[Bytes])


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

  private val cache: scm.HashMap[Text, jnf.FileSystem] = scm.HashMap()

case class ZipFile(private val filename: Text):
  private def javaFs(): jnf.FileSystem throws ZipError =
    val uri: java.net.URI = java.net.URI.create(t"jar:file:$filename".s).nn
    
    try jnf.FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn
    catch case exception: jnf.ProviderNotFoundException => throw ZipError(filename)
  
  def filesystem(): jnf.FileSystem throws ZipError =
    ZipFile.cache.getOrElseUpdate(filename, synchronized(javaFs()))

  def append
      [InstantType]
      (entries: LazyList[ZipEntry], /*prefix: Maybe[Bytes] = Unset, */timestamp: Maybe[InstantType] = Unset)
      (using env: Environment, instant: GenericInstant[InstantType] = timeApi.long)
      : Unit throws ZipError | StreamCutError =
    
    val writeTimestamp: jnf.attribute.FileTime =
      jnf.attribute.FileTime.fromMillis(timestamp.mm(readInstant(_)).or(System.currentTimeMillis)).nn
  
    def recur(refs: LazyList[ZipEntry], set: Set[ZipRef]): Set[ZipRef] = refs match
      case head #:: tail => recur(tail, if set.contains(head.ref) then set else set + head.ref)
      case _             => set
      
    val fs: jnf.FileSystem = filesystem()
    
    val dirs = recur(entries, Set()).flatMap(_.descent.tails.map(ZipRef(_)).to(Set)).to(List)
    val dirs2 = dirs.map(_.render+t"/").sorted

    dirs2.foreach: dir =>
      val dirPath = fs.getPath(dir.s).nn
      
      if jnf.Files.notExists(dirPath) then
        jnf.Files.createDirectory(dirPath)
        jnf.Files.setAttribute(dirPath, "creationTime", writeTimestamp)
        jnf.Files.setAttribute(dirPath, "lastAccessTime", writeTimestamp)
        jnf.Files.setAttribute(dirPath, "lastModifiedTime", writeTimestamp)

    entries.foreach: entry =>
      val entryPath = fs.getPath(entry.ref.render.s).nn
      val in = entry.content().inputStream
      jnf.Files.copy(in, entryPath, jnf.StandardCopyOption.REPLACE_EXISTING)
      jnf.Files.setAttribute(entryPath, "creationTime", writeTimestamp)
      jnf.Files.setAttribute(entryPath, "lastAccessTime", writeTimestamp)
      jnf.Files.setAttribute(entryPath, "lastModifiedTime", writeTimestamp)
      
    fs.close()

    //val fileOut = ji.BufferedOutputStream(ji.FileOutputStream(ji.File(filename.s)).nn)
    
    // prefix.option.foreach: prefix =>
    //   fileOut.write(prefix.mutable(using Unsafe))
    //   fileOut.flush()
    
    // val tmpDir: ji.File = Xdg.Var.Tmp()
    // val tmpFile: ji.File = ji.File.createTempFile("tmp", ".zip", tmpDir).nn
    
    //fileOut.write(jnf.Files.readAllBytes(tmpFile.toPath.nn))
    //fileOut.close()
    //java.nio.file.Files.delete(tmpFile.toPath.nn)

  def entries(): LazyList[ZipEntry] throws StreamCutError =
    val zipFile = juz.ZipFile(ji.File(filename.s)).nn
    
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map: entry =>
      ZipEntry(unsafely(ZipRef(entry.getName.nn.show)), zipFile.getInputStream(entry).nn)

object ZipEntry:
  def apply
      [ResourceType]
      (path: ZipRef, resource: ResourceType)
      (using Readable[ResourceType, Bytes])
      : ZipEntry =
    new ZipEntry(path, () => resource.stream[Bytes])

  given Readable[ZipEntry, Bytes] = Readable.lazyList[Bytes].contraMap(_.content())

  // 00:00:00, 1 January 2000
  val epoch: jnf.attribute.FileTime = jnf.attribute.FileTime.fromMillis(946684800000L).nn
