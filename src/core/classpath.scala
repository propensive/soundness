/*
    Hellenism, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hellenism

import rudiments.*
import serpentine.*
import fulminate.*
import spectacular.*
import gossamer.*
import perforate.*
import anticipation.*
import turbulence.*

import scala.quoted.*

import java.net as jn
import java.io as ji

sealed trait ClasspathEntry

object ClasspathEntry:
  case class Directory(path: Text) extends ClasspathEntry:
    def apply[DirectoryType: SpecificDirectory](): DirectoryType = SpecificDirectory(path)

  case class Jarfile(path: Text) extends ClasspathEntry:
    def apply[FileType: SpecificFile](): FileType = SpecificFile(path)

  case class Url(url: Text) extends ClasspathEntry:
    def apply[UrlType: SpecificUrl](): UrlType = SpecificUrl(url)

  case object JavaRuntime extends ClasspathEntry

  def apply(url: jn.URL): ClasspathEntry = url.getProtocol.nn.tt match
    case t"jrt" =>
      ClasspathEntry.JavaRuntime
    
    case t"file" =>
      val path: Text = url.nn.getPath.nn.tt
      if path.ends(t"/") then ClasspathEntry.Directory(path) else ClasspathEntry.Jarfile(path)
    
    case t"http" | t"https" =>
      ClasspathEntry.Url(url.toString.tt)
    
object Classloader:
  def threadContext: Classloader = new Classloader(Thread.currentThread.nn.getContextClassLoader.nn)
  inline def apply[ClassType <: AnyKind]: Classloader = ClassRef[ClassType].classloader
  
class Classloader(val java: ClassLoader):

  def parent: Maybe[Classloader] = Maybe(java.getParent).mm(new Classloader(_))

  protected def urlClassloader: Maybe[jn.URLClassLoader] = java match
    case java: jn.URLClassLoader => java
    case _                       => parent.mm(_.urlClassloader)
  
  def classpath: Maybe[Classpath] = urlClassloader.mm(Classpath(_))
  private[hellenism] def inputStream(path: Text)(using notFound: Raises[ClasspathError]): ji.InputStream =
    Maybe(java.getResourceAsStream(path.s)).or(abort(ClasspathError(path)))

object Classpath:
  @targetName("child")
  def /(child: PathName[ClasspathRef.Forbidden]): ClasspathRef = ClasspathRef(List(child))
 
class Classpath(urlClassloader: jn.URLClassLoader):
  def entries: List[ClasspathEntry] =
    urlClassloader.mm(_.getURLs.nn.to(List)).or(Nil).map(_.nn).map(ClasspathEntry(_))
  

object ClasspathRef:
  type Forbidden = "" | ".*\\/.*"

  inline given decoder(using Raises[PathError]): Decoder[ClasspathRef] = new Decoder[ClasspathRef]:
    def decode(text: Text): ClasspathRef = Reachable.decode[ClasspathRef](text)

  given reachable: Reachable[ClasspathRef, Forbidden, Classpath.type] with
    def root(ref: ClasspathRef): Classpath.type = Classpath
    def prefix(classpathCompanion: Classpath.type): Text = t""
    def descent(ref: ClasspathRef): List[PathName[Forbidden]] = ref.descent
    def separator(ref: ClasspathRef): Text = t"/"
  
  given creator: PathCreator[ClasspathRef, Forbidden, Classpath.type] = (_, descent) => ClasspathRef(descent)

  given rootParser: RootParser[ClasspathRef, Classpath.type] = (Classpath, _)

  given show: Show[ClasspathRef] = _.text

case class ClasspathRef(descent: List[PathName[ClasspathRef.Forbidden]]):
  def text: Text = descent.reverse.map(_.render).join(t"/")

  def apply()(using classloader: Classloader): Resource = Resource(classloader, this)

object Resource:
  given readableBytes(using Raises[ClasspathError]): Readable[Resource, Bytes] =
    Readable.reliableInputStream.contraMap: resource =>
      resource.classloader.inputStream(resource.ref.text)

case class Resource(classloader: Classloader, ref: ClasspathRef)

object Hellenism extends Hellenism2:

  opaque type ClassRef = Class[?]

  object ClassRef:
    def apply(javaClass: Class[?]): ClassRef = javaClass
    inline def apply[ClassType <: AnyKind]: ClassRef = ${Hellenism.makeClass[ClassType]}

  extension (classRef: ClassRef)
    def classloader: Classloader = new Classloader(classRef.getClassLoader().nn)
    
    def classpathEntry: ClasspathEntry =
      ClasspathEntry(classRef.getProtectionDomain.nn.getCodeSource.nn.getLocation.nn)

export Hellenism.ClassRef

trait Hellenism2:
  def makeClass[ClassType <: AnyKind: Type](using Quotes): Expr[ClassRef] =
    import quotes.reflect.*
    '{ClassRef(Class.forName(${Expr(TypeRepr.of[ClassType].classSymbol.get.fullName)}).nn)}

case class ClasspathError(resource: Text)
extends Error(msg"the resource $resource was not on the classpath")