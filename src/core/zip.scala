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
import perforate.*
import serpentine.*
import diuretic.*
import fulminate.*
import anticipation.*
import turbulence.*
import spectacular.*
import ambience.*

import scala.collection.mutable as scm

import java.io as ji
import java.nio.file as jnf
import java.util.zip as juz

//import scala.language.experimental.captureChecking

case class ZipError(filename: Text) extends Error(msg"could not create ZIP file ${filename}")

// FIXME: Check this
type InvalidZipNames = ".*'.*" | ".*`.*" | ".*\\/.*" | ".*\\\\.*"

object ZipPath:
  given reachable: Reachable[ZipPath, InvalidZipNames, ZipFile] with
    def root(path: ZipPath): ZipFile = path.zipFile
    def descent(path: ZipPath): List[PathName[InvalidZipNames]] = path.descent
    def prefix(path: ZipFile): Text = t"/"
    def separator(path: ZipPath): Text = t"/"
    
  given creator: PathCreator[ZipPath, InvalidZipNames, ZipFile] = (root, descent) =>
    ZipPath(root, ZipRef(descent))

  given readable(using CanThrow[StreamError]): Readable[ZipPath, Bytes] =
    Readable.lazyList[Bytes].contramap(_.entry().content())

case class ZipPath(zipFile: ZipFile, ref: ZipRef):
  def entry()(using streamCut: CanThrow[StreamError]): ZipEntry = zipFile.entry(ref)

object ZipRef:
  def apply
      (text: Text)
      (using pathError: Raises[PathError], reachable: Reachable[ZipRef, InvalidZipNames, Unset.type],
          rootParser: RootParser[ZipRef, Unset.type], creator: PathCreator[ZipRef, InvalidZipNames, Unset.type])
      : ZipRef =
    Reachable.decode[ZipRef](text)
  
  @targetName("child")
  def /(name: PathName[InvalidZipNames]): ZipRef = ZipRef(List(name))
  
  given reachable: Reachable[ZipRef, InvalidZipNames, Unset.type] with
    def root(path: ZipRef): Unset.type = Unset
    def descent(path: ZipRef): List[PathName[InvalidZipNames]] = path.descent
    def prefix(ref: Unset.type): Text = t""
    def separator(path: ZipRef): Text = t"/"

  given rootParser: RootParser[ZipRef, Unset.type] with
    def parse(text: Text): (Unset.type, Text) = (Unset, text.drop(1))

  given creator: PathCreator[ZipRef, InvalidZipNames, Unset.type] = (root, descent) => ZipRef(descent)

case class ZipRef(descent: List[PathName[InvalidZipNames]])

object ZipEntry:
  def apply
      [ResourceType]
      (path: ZipRef, resource: ResourceType)
      (using readable: Readable[ResourceType, Bytes])
      : ZipEntry =
    new ZipEntry(path, () => resource.stream[Bytes])

  given Readable[ZipEntry, Bytes] = Readable.lazyList[Bytes].contramap(_.content())

  // 00:00:00, 1 January 2000
  val epoch: jnf.attribute.FileTime = jnf.attribute.FileTime.fromMillis(946684800000L).nn

case class ZipEntry(ref: ZipRef, content: () => LazyList[Bytes])

object ZipFile:
  def apply[FileType]
      (file: FileType)
      (using genericFile: /*{*}*/ GenericFile[FileType], streamCut: CanThrow[StreamError])
      : /*{genericFile, streamCut}*/ ZipFile =
    val pathname: Text = file.fileText
    new ZipFile(pathname)

  def create[PathType]
      (path: PathType)
      (using genericPath: GenericPath[PathType], streamCut: CanThrow[StreamError])
      : ZipFile =
    val pathname: Text = path.pathText
    val out: juz.ZipOutputStream = juz.ZipOutputStream(ji.FileOutputStream(ji.File(pathname.s)))
    
    out.putNextEntry(juz.ZipEntry("/"))
    out.closeEntry()
    out.close()

    ZipFile(pathname.show)

  private val cache: scm.HashMap[Text, jnf.FileSystem] = scm.HashMap()

case class ZipFile(private val filename: Text):
  private lazy val zipFile: juz.ZipFile = juz.ZipFile(ji.File(filename.s)).nn
  
  private def javaFs(): jnf.FileSystem throws ZipError =
    val uri: java.net.URI = java.net.URI.create(t"jar:file:$filename".s).nn
    
    try jnf.FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn
    catch case exception: jnf.ProviderNotFoundException => throw ZipError(filename)
  
  @targetName("child")
  def /(name: PathName[InvalidZipNames]): ZipPath = ZipPath(this, ZipRef(List(name)))

  def filesystem(): jnf.FileSystem throws ZipError =
    ZipFile.cache.getOrElseUpdate(filename, synchronized(javaFs()))

  def entry(ref: ZipRef)(using streamCut: CanThrow[StreamError]): ZipEntry =
    ZipEntry(ref, zipFile.getInputStream(zipFile.getEntry(ref.render.s).nn).nn)

  def append
      [InstantType: GenericInstant]
      (entries: LazyList[ZipEntry], /*prefix: Optional[Bytes] = Unset, */timestamp: Optional[InstantType] = Unset)
      (using env: Environment)
      : Unit throws ZipError | StreamError =
    
    val writeTimestamp: jnf.attribute.FileTime =
      jnf.attribute.FileTime.fromMillis(timestamp.let(_.millisecondsSinceEpoch).or(System.currentTimeMillis)).nn
  
    def recur(refs: LazyList[ZipEntry], set: Set[ZipRef]): Set[ZipRef] = refs match
      case head #:: tail => recur(tail, if set.contains(head.ref) then set else set + head.ref)
      case _             => set
      
    val fs: jnf.FileSystem = filesystem()
    
    val directories = recur(entries, Set()).flatMap(_.descent.tails.map(ZipRef(_)).to(Set)).to(List)
    val directories2 = directories.map(_.render+t"/").sorted

    directories2.each: directory =>
      val directoryPath = fs.getPath(directory.s).nn
      
      if jnf.Files.notExists(directoryPath) then
        jnf.Files.createDirectory(directoryPath)
        jnf.Files.setAttribute(directoryPath, "creationTime", writeTimestamp)
        jnf.Files.setAttribute(directoryPath, "lastAccessTime", writeTimestamp)
        jnf.Files.setAttribute(directoryPath, "lastModifiedTime", writeTimestamp)

    entries.each: entry =>
      val entryPath = fs.getPath(entry.ref.render.s).nn
      val in = entry.content().inputStream
      jnf.Files.copy(in, entryPath, jnf.StandardCopyOption.REPLACE_EXISTING)
      jnf.Files.setAttribute(entryPath, "creationTime", writeTimestamp)
      jnf.Files.setAttribute(entryPath, "lastAccessTime", writeTimestamp)
      jnf.Files.setAttribute(entryPath, "lastModifiedTime", writeTimestamp)
      
    fs.close()

    //val fileOut = ji.BufferedOutputStream(ji.FileOutputStream(ji.File(filename.s)).nn)
    
    // prefix.option.each: prefix =>
    //   fileOut.write(prefix.mutable(using Unsafe))
    //   fileOut.flush()
    
    // val tmpDir: ji.File = Xdg.Var.Tmp()
    // val tmpFile: ji.File = ji.File.createTempFile("tmp", ".zip", tmpDir).nn
    
    //fileOut.write(jnf.Files.readAllBytes(tmpFile.toPath.nn))
    //fileOut.close()
    //java.nio.file.Files.delete(tmpFile.toPath.nn)

  def entries(): LazyList[ZipEntry] throws StreamError =
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map: entry =>
      ZipEntry(unsafely(ZipRef(entry.getName.nn.show)), zipFile.getInputStream(entry).nn)
