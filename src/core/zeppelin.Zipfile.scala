package zeppelin

import java.io as ji
import java.nio.file as jnf
import java.net as jn
import java.util.zip as juz
import scala.collection.concurrent as scc
import scala.collection.mutable as scm

import anticipation.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import contingency.*
import nomenclature.*
import fulminate.*
import feudalism.*
import vacuous.*

import juz.ZipFile

object Zip:
  type Rules = MustNotContain["\\"] & MustNotContain["\""] & MustNotContain["/"] &
      MustNotContain[":"] & MustNotContain["*"] & MustNotContain["?"] & MustNotContain["<"] &
      MustNotContain[">"] & MustNotContain["|"]

  class ZipRoot(private val filesystem: Optional[jnf.FileSystem] = Unset) extends Root(t"", t"/", Case.Sensitive):
    type Platform = Zip
  
  given (using Tactic[NameError]) => Zip is Radical from Zip.ZipRoot = new Radical:
    type Self = Zip
    type Source = ZipRoot

    def rootLength(path: Text): Int = 0
    def rootText(root: Source): Text = t""
    def root(path: Text): Source = ZipRoot()

  given (using Tactic[NameError]) => Zip is Navigable by Name[Zip] under Rules = new Navigable:
    type Self = Zip
    type Operand = Name[Zip]
    type Constraint = Rules

    val separator: Text = t"/"
    val parentElement: Text = t".."
    val selfText: Text = t"."

    def element(element: Text): Name[Zip] = Name(element)
    def elementText(element: Name[Zip]): Text = element.text
    def caseSensitivity: Case = Case.Sensitive

erased trait Zip

object Zipfile:
  given Zipfile is Openable over jnf.FileSystem = new Openable:
    type Self = Zipfile
    type Operand = Unit
    type Result = Root
    protected type Carrier = jnf.FileSystem
    
    def init(value: Zipfile, options: List[Operand]): Carrier =
      try jnf.FileSystems.newFileSystem(value.uri, Map("zipinfo-time" -> "false").asJava).nn
      catch case exception: jnf.ProviderNotFoundException =>
        throw Panic(m"There was unexpectedly no filesystem provider for ZIP files")
      
    def handle(carrier: jnf.FileSystem): Zip.ZipRoot = Zip.ZipRoot(carrier: jnf.FileSystem)
    def close(carrier: Carrier): Unit = carrier.close()

  private val cache: scc.TrieMap[Text, Semaphore] = scc.TrieMap()

  def write[PathType: GenericPath](path: PathType)(stream: LazyList[ZipEntry]): Unit =
    val filename = path.pathText
    val out: juz.ZipOutputStream = juz.ZipOutputStream(ji.FileOutputStream(ji.File(filename.s)))
    val directories: scm.HashSet[Path on Zip] = scm.HashSet()

    def addEntry(path: Path on Zip): Boolean = directories(path) || {
      directories += path
      out.putNextEntry(juz.ZipEntry(path.text.s))
      out.closeEntry()
      false
    }

    for entry <- stream do
      entry.ref.ancestors.reverse.exists: path =>
        directories(path) || addEntry(path)
      
      out.putNextEntry(juz.ZipEntry(entry.ref.text.s))
      
      entry.content().each: bytes =>
        out.write(bytes.mutable(using Unsafe))

      out.closeEntry()

    out.close()
      
      
  

case class Zipfile(path: Text):
  protected lazy val zipFile: juz.ZipFile = juz.ZipFile(ji.File(path.s)).nn
  protected lazy val uri: jn.URI = jn.URI.create(t"jar:file:$path".s).nn
  private def semaphore = Zipfile.cache.getOrElseUpdate(path, Semaphore())
