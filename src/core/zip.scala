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
import java.util.zip as juz

//import scala.language.experimental.captureChecking

case class ZipError(filename: Text) extends Error(msg"could not create ZIP file ${filename}")

// FIXME: Check this
type InvalidZipNames = ".*'.*" | ".*`.*" | ".*\\/.*" | ".*\\\\.*"

object ZipPath:
  given navigable: Navigable[ZipPath, InvalidZipNames, ZipFile] with
    def root(path: ZipPath): ZipFile = path.zipFile
    def descent(path: ZipPath): List[PathName[InvalidZipNames]] = path.descent
    def prefix(path: ZipFile): Text = t"/"
    def separator(path: ZipPath): Text = t"/"
    
  given creator: PathCreator[ZipPath, InvalidZipNames, ZipFile] = (root, descent) =>
    ZipPath(root, ZipRef(descent))

  given readable(using Raises[StreamError]): Readable[ZipPath, Bytes] =
    Readable.lazyList[Bytes].contramap(_.entry().content())

case class ZipPath(zipFile: ZipFile, ref: ZipRef):
  def entry()(using streamCut: Raises[StreamError]): ZipEntry = zipFile.entry(ref)

object ZipRef:
  def apply(text: Text)
      (using pathError:  Raises[PathError],
             navigable:  Navigable[ZipRef, InvalidZipNames, Unset.type],
             rootParser: RootParser[ZipRef, Unset.type],
             creator:    PathCreator[ZipRef, InvalidZipNames, Unset.type])
          : ZipRef =

    Navigable.decode[ZipRef](text)
  
  @targetName("child")
  infix def / (name: PathName[InvalidZipNames]): ZipRef = ZipRef(List(name))
  
  given navigable: Navigable[ZipRef, InvalidZipNames, Unset.type] with
    def root(path: ZipRef): Unset.type = Unset
    def descent(path: ZipRef): List[PathName[InvalidZipNames]] = path.descent
    def prefix(ref: Unset.type): Text = t""
    def separator(path: ZipRef): Text = t"/"

  given rootParser: RootParser[ZipRef, Unset.type] with
    def parse(text: Text): (Unset.type, Text) =
      (Unset, if text.length > 0 && text.at(0) == '/' then text.drop(1) else text)

  given creator: PathCreator[ZipRef, InvalidZipNames, Unset.type] = (root, descent) => ZipRef(descent)
  given show: Show[ZipRef] = _.descent.reverse.map(_.render).join(t"/", t"/", t"")

case class ZipRef(descent: List[PathName[InvalidZipNames]]):
  def parent: Optional[ZipRef] = descent match
    case Nil       => Unset
    case _ :: tail => ZipRef(tail)

object ZipEntry:
  def apply[ResourceType](path: ZipRef, resource: ResourceType)(using readable: Readable[ResourceType, Bytes])
          : ZipEntry =

    new ZipEntry(path, () => resource.stream[Bytes])

  given Readable[ZipEntry, Bytes] = Readable.lazyList[Bytes].contramap(_.content())

  // 00:00:00, 1 January 2000
  val epoch: jnf.attribute.FileTime = jnf.attribute.FileTime.fromMillis(946684800000L).nn

case class ZipEntry(ref: ZipRef, content: () => LazyList[Bytes])

object ZipFile:
  def apply[FileType](file: FileType)
      (using genericFile: /*{*}*/ GenericFile[FileType], stream: Raises[StreamError])
          : ZipFile =

    val pathname: Text = file.fileText
    new ZipFile(pathname)

  def create[PathType](path: PathType)(using genericPath: GenericPath[PathType], streamCut: Raises[StreamError])
          : ZipFile =

    val pathname: Text = path.pathText
    val out: juz.ZipOutputStream = juz.ZipOutputStream(ji.FileOutputStream(ji.File(pathname.s)))
    
    out.putNextEntry(juz.ZipEntry("/"))
    out.closeEntry()
    out.close()

    ZipFile(pathname.show)

  private val cache: scc.TrieMap[Text, Semaphore] = scc.TrieMap()

case class ZipFile(private val filename: Text):
  private lazy val zipFile: juz.ZipFile = juz.ZipFile(ji.File(filename.s)).nn
  
  private val filesystemUri: java.net.URI = java.net.URI.create(t"jar:file:$filename".s).nn
    
  @targetName("child")
  infix def / (name: PathName[InvalidZipNames]): ZipPath = ZipPath(this, ZipRef(List(name)))

  private def semaphore: Semaphore = ZipFile.cache.getOrElseUpdate(filename, Semaphore())

  private def withFilesystem[ResultType](lambda: jnf.FileSystem => ResultType): ResultType raises ZipError =
    semaphore.isolate:
      val filesystem =
        try jnf.FileSystems.newFileSystem(filesystemUri, Map("zipinfo-time" -> "false").asJava).nn
        catch case exception: jnf.ProviderNotFoundException => abort(ZipError(filename))

      lambda(filesystem).also(filesystem.close())
    
  def entry(ref: ZipRef)(using streamCut: Raises[StreamError]): ZipEntry =
    semaphore.attend(ZipEntry(ref, zipFile.getInputStream(zipFile.getEntry(ref.render.s).nn).nn))

  def append[InstantType: GenericInstant](entries: LazyList[ZipEntry], timestamp: Optional[InstantType] = Unset)
      (using Environment)
          : Unit raises ZipError raises StreamError =
    
    val writeTimestamp: jnf.attribute.FileTime =
      jnf.attribute.FileTime.fromMillis(timestamp.let(_.millisecondsSinceEpoch).or(System.currentTimeMillis)).nn
    
    withFilesystem: filesystem =>
      val directories: scm.HashSet[ZipRef] = scm.HashSet()

      def addParents(ref: ZipRef): Unit = ref.parent.let: parent =>
        if !directories.has(parent) then
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

