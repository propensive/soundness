/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

// sealed trait Inode(val path: DiskPath):
//   def setPermissions(readable: Optional[Boolean] = Unset, writable: Optional[Boolean] = Unset,
//                          executable: Optional[Boolean]): Unit throws IoError =
//     if !readable.option.fold(true)(javaFile.setReadable(_)) |
//         !writable.option.fold(true)(javaFile.setWritable(_)) |
//         !executable.option.fold(true)(javaFile.setExecutable(_))
//     then throw IoError(IoError.Op.Permissions, IoError.Reason.AccessDenied, path)
  
//   def touch(): Unit throws IoError =
//     try
//       if !exists() then ji.FileOutputStream(javaFile).close()
//       else javaFile.setLastModified(System.currentTimeMillis())
//     catch case e => throw IoError(IoError.Op.Write, IoError.Reason.NotSupported, path)

    
// object Directory:
// case class Directory(directoryPath: DiskPath)
// extends Inode(directoryPath), Shown[Directory]:
//   def tmpPath(suffix: Optional[Text] = Unset): DiskPath =
//     val part = unsafely(PathElement(t"${Uuid().show}${suffix.or(t"")}"))
//     path.root.make(path.parts :+ part.value)
  
//   def tmpFile(suffix: Optional[Text] = Unset): File throws IoError =
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
//   def rename(fn: Text => Text): DiskPath = DiskPath(root, elements.init :+ fn(name))

// object Filesystem:

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

//   given readable(using classpathRef: CanThrow[ClasspathRefError], streamCut: CanThrow[StreamError])
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
