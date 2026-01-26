package jacinta

import scala.collection.mutable as scm
import scala.annotation.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

object JsonPointer:
  trait Registry:
    private val documents: scm.HashMap[HttpUrl, Json] = scm.HashMap()
    def update(url: HttpUrl, document: Json): Unit = documents(url) = document
    def apply(url: HttpUrl): Optional[Json] = documents.at(url).or(lookup(url))
    protected def lookup(url: HttpUrl): Optional[Json]

  given navigable: [ordinal <: Ordinal] => ordinal is Navigable on JsonPointer = _.n0.toString.tt
  given admissible: [ordinal <: Ordinal] => ordinal is Admissible on JsonPointer = _ => ()
  given admissible2: [text <: Text] => text is Admissible on JsonPointer = _ => ()

  given filesystem: JsonPointer is Filesystem:
    override def escape(text: Text): Text = text.sub("~", "~0").sub("/", "~1")
    override def unescape(text: Text): Text = text.sub("~1", "/").sub("~0", "~")

    val parent: Text = ".."
    val self: Text = "#"
    val separator: Text = "/"

  given JsonPointer is Encodable in Text = pointer =>
    t"${pointer.url.let(_.encode).or(t"")}#${pointer.path}"

case class JsonPointer(url: Optional[HttpUrl], path: Path on JsonPointer):
  def apply(using registry: JsonPointer.Registry)(document: Json): Json raises JsonPointerError =
    url.let(registry(_).lest(JsonPointerError())).or(document)

  def apply(ordinal: Ordinal): JsonPointer = JsonPointer(url, path / ordinal)
  def apply(text: Text): JsonPointer = JsonPointer(url, path / text)
