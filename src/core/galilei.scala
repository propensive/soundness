/*
    Galilei, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import rudiments.*
import digression.*
import gossamer.*
import turbulence.*
import serpentine.*
import spectacular.*
import eucalyptus.*
import anticipation.*

import java.io as ji
import java.nio.file as jnf

import jnf.{Files, Paths, StandardCopyOption, DirectoryNotEmptyException}, jnf.StandardCopyOption.*
import ji.{File as JavaFile}

// extension (inodes: Seq[Inode])
//   transparent inline def files: Seq[File] = inodes.collect:
//     case file: File => file
  
//   transparent inline def directories: Seq[Directory] = inodes.collect:
//     case dir: Directory => dir

// sealed trait Inode(val path: DiskPath):
//   lazy val javaFile: ji.File = ji.File(fullname.s)
//   lazy val javaPath: jnf.Path = javaFile.toPath.nn

//   def name: Text = path.parts.lastOption.getOrElse(path.root.prefix)
//   def fullname: Text = path.javaFile.getAbsolutePath.nn.show
//   def uriString: Text = Showable(javaFile.toURI).show
  
//   def directory: Maybe[Directory]
//   def file: Maybe[File]
//   def symlink: Maybe[Symlink]
//   def modified[InstantType: GenericInstant]: InstantType = makeInstant(javaFile.lastModified)
//   def exists(): Boolean = javaFile.exists()
//   def delete(): Unit throws IoError

//   def readable: Boolean = Files.isReadable(javaPath)
//   def writable: Boolean = Files.isWritable(javaPath)
//   def setPermissions(readable: Maybe[Boolean] = Unset, writable: Maybe[Boolean] = Unset,
//                          executable: Maybe[Boolean]): Unit throws IoError =
//     if !readable.option.fold(true)(javaFile.setReadable(_)) |
//         !writable.option.fold(true)(javaFile.setWritable(_)) |
//         !executable.option.fold(true)(javaFile.setExecutable(_))
//     then throw IoError(IoError.Op.Permissions, IoError.Reason.AccessDenied, path)
  
//   def touch(): Unit throws IoError =
//     try
//       if !exists() then ji.FileOutputStream(javaFile).close()
//       else javaFile.setLastModified(System.currentTimeMillis())
//     catch case e => throw IoError(IoError.Op.Write, IoError.Reason.NotSupported, path)

// object File:
//   given provider(using fs: Filesystem): (GenericFileMaker[File] & GenericFileReader[File]) =
//     new GenericFileMaker[File] with GenericFileReader[File]:
//       def makeFile(str: String, readOnly: Boolean = false): Option[File] =
//         safely(fs.parse(Text(str)).file(Expect)).option
      
//       def filePath(file: File): String = file.path.fullname.toString

//   given writable
//       (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
//       : (/*{io, streamCut}*/ Writable[File, Bytes]) =
//     Appendable.outputStreamBytes.asWritable.contraMap: file =>
//       if !file.javaFile.canWrite()
//       then throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, file.path)
      
//       ji.BufferedOutputStream(ji.FileOutputStream(file.javaFile, false))

//   given appendable
//       (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
//       : (/*{io, streamCut}*/ Appendable[File, Bytes]) =
//     Appendable.outputStreamBytes.contraMap: file =>
//       if !file.javaFile.canWrite()
//       then throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, file.path)
      
//       ji.BufferedOutputStream(ji.FileOutputStream(file.javaFile, true))

//   given readableBytes
//       (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
//       : (/*{io, streamCut}*/ Readable[File, Bytes]) =
//     Readable.inputStream.contraMap: file =>
//       if !file.javaFile.canRead() then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.path)
//       else ji.BufferedInputStream(ji.FileInputStream(file.javaFile))
  
//   given readableLine
//       (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
//       : (/*{io, streamCut}*/ Readable[File, Line]) =
//     Readable.bufferedReader.contraMap: file =>
//       if !file.javaFile.canRead() then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.path)
//       else ji.BufferedReader(ji.FileReader(file.javaFile))
    
// case class File(filePath: DiskPath) extends Inode(filePath), Shown[File]:
//   def hardLinkCount(): Int throws IoError =
//     try Files.getAttribute(javaPath, "unix:nlink") match
//       case i: Int => i
//       case _      => throw Mistake("Should never match")
//     catch e => throw IoError(IoError.Op.Read, IoError.Reason.NotSupported, path)
  

// object Fifo:
//   given Show[Fifo] = t"ˢ｢"+_.path.fullname+t"｣"
  
//   given appendable[ChunkType](using io: CanThrow[IoError],
//                                   appendable: /*{*}*/ Appendable[ji.OutputStream, ChunkType])
//         : (/*{io, appendable}*/ Appendable[Fifo, ChunkType]) =
//     appendable.contraMap: fifo =>
//       if !fifo.writable()
//       then throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, fifo.path)
      
//       fifo.out

//   given readable[ChunkType](using io: CanThrow[IoError], readable: /*{*}*/ Readable[ji.InputStream, ChunkType])
//                      : (/*{io, readable}*/ Readable[Fifo, ChunkType]) =
//     readable.contraMap: fifo =>
//       if !fifo.readable() then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, fifo.path)
//       else fifo.in
  
    
// case class Fifo(path: DiskPath) extends Shown[Fifo]:
//   def writable(): Boolean = path.javaFile.canWrite()
//   def readable(): Boolean = path.javaFile.canRead()
//   lazy val out = ji.FileOutputStream(path.javaFile, false)
//   lazy val in = ji.FileInputStream(path.javaFile)
//   def close(): Unit = out.close()

// object Symlink:
//   given Show[Symlink] = t"ˢʸᵐ｢"+_.path.fullname+t"｣"

// object Directory:
//   given GenericWatchService[Directory] = () => Unix.javaFilesystem.newWatchService().nn

//   given provider(using fs: Filesystem)
//                 : (GenericDirectoryMaker[Directory] & GenericDirectoryReader[Directory]) =
//     new GenericDirectoryMaker[Directory] with GenericDirectoryReader[Directory]:
//       def makeDirectory(str: String, readOnly: Boolean = false): Option[Directory] =
//         safely(fs.parse(Text(str)).directory(Expect)).option
      
//       def directoryPath(dir: Directory): String = dir.path.fullname.s
  
//   given pathReader(using fs: Filesystem): GenericPathReader[Directory] =
//     new GenericPathReader[Directory]:
//       def getPath(dir: Directory): String = dir.path.fullname.s
  
//   given pathMaker(using fs: Filesystem): GenericPathMaker[Directory] =
//     new GenericPathMaker[Directory]:
//       def makePath(str: String, readOnly: Boolean = false): Option[Directory] =
//         safely(fs.parse(Text(str)).directory(Expect)).option

// case class Directory(directoryPath: DiskPath)
// extends Inode(directoryPath), Shown[Directory]:
//   def directory: Directory = this
//   def file: Unset.type = Unset
//   def symlink: Unset.type = Unset
  
//   def tmpPath(suffix: Maybe[Text] = Unset): DiskPath =
//     val part = unsafely(PathElement(t"${Uuid().show}${suffix.or(t"")}"))
//     path.root.make(path.parts :+ part.value)
  
//   def tmpFile(suffix: Maybe[Text] = Unset): File throws IoError =
//     val file = tmpPath(suffix).file(Create)
//     file.javaFile.deleteOnExit()
//     file

// object DiskPath:
//   given provider(using fs: Filesystem): (GenericPathMaker[DiskPath] & GenericPathReader[DiskPath]) =
//     new GenericPathMaker[DiskPath] with GenericPathReader[DiskPath]:
//       def makePath(str: String, readOnly: Boolean = false): Option[DiskPath] =
//         safely(fs.parse(Text(str))).option
    
//       def getPath(path: DiskPath): String = path.fullname.s

// case class DiskPath(filesystem: Filesystem, elements: List[Text])
// extends Absolute(elements), Shown[DiskPath]:
//   type RootType = Filesystem
//   val root: Filesystem = filesystem
//   lazy val javaFile: ji.File = ji.File(fullname.s)
//   lazy val javaPath: jnf.Path = javaFile.toPath.nn
  
//   def rename(fn: Text => Text): DiskPath = DiskPath(root, elements.init :+ fn(name))

// object Filesystem:

//   given Show[Filesystem] = fs => t"ᶠˢ｢${fs.name}:${fs.prefix}...${fs.separator}｣"

//   lazy val roots: Set[Filesystem] =
//     Option(ji.File.listRoots).fold(Set())(_.nn.immutable(using Unsafe).to(Set)).map(_.nn.getAbsolutePath.nn)
//         .collect:
//       case "/" =>
//         Unix
      
//       case s"""$drive:\""" if drive.length == 1 =>
//         unsafely:
//           drive.charAt(0).toUpper match
//             case ch: Majuscule => WindowsRoot(ch)
//             case _             => throw Mistake("Filesystem must always start with a letter")
//     .to(Set)
 
// object Classpath:
//   given Show[Classpath] = cp =>
//     def recur(cp: ClassLoader): Text =
//       val start = Option(cp.getParent).map(_.nn).map(recur(_)).getOrElse(t"")
//       t"$start/${Option(cp.getName).fold(t"-")(_.nn.show)}"
    
//     recur(cp.classLoader)

// open class Classpath(val classLoader: ClassLoader = getClass.nn.getClassLoader.nn)
// extends Root(t"/", t""), Shown[Classpath]:
//   type PathType = ClasspathRef
//   protected inline def classpath: this.type = this
//   def make(parts: List[Text]): ClasspathRef = ClasspathRef(this, parts)

// object ClasspathRef:
//   given Show[ClasspathRef] = t"ᶜᵖ｢"+_.fullname+t"｣"

//   given readable(using classpathRef: CanThrow[ClasspathRefError], streamCut: CanThrow[StreamCutError])
//                 : (/*{classpathRef, streamCut}*/ Readable[ClasspathRef, Bytes]) =
//     Readable.inputStream.contraMap: ref =>
//       ref.classpath.classLoader.getResourceAsStream(ref.fullname.drop(1).s) match
//         case null => throw ClasspathRefError(ref)
//         case in   => in.nn

// case class ClasspathRef(classpath: Classpath, elements: List[Text])
// extends Absolute(elements), Shown[ClasspathRef]:
//   type RootType = Classpath
//   val root: Classpath = classpath
//   def fullname = parts.join(t"/", t"/", t"")

// case class ClasspathRefError(path: ClasspathRef)
// extends Error(msg"the resource $path is not on the claspath")
