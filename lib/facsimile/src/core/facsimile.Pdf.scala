                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package facsimile

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*

object Pdf:
  case class Version(major: Int, minor: Int)

  // An embedded file from the `/EmbeddedFiles` name tree. Its metadata is materialized, but
  // `data` still reads through the document, so an `Attachment` is confined to the scope;
  // call `data` inside and keep the result.
  class Attachment private[facsimile]
    ( pdf:             Pdf,
      val name:        Text,
      val filename:    Optional[Text],
      val description: Optional[Text],
      val mediaType:   Optional[Text],
      body:            Optional[Cos.Body] ):

    def data: Data raises PdfError =
      body.let(pdf.payload(_)).or(abort(PdfError(PdfError.Reason.MissingEntry(t"EF"))))

  // Until the encryption milestone lands, an encrypted file is refused outright rather
  // than misread: strings and streams would otherwise surface as ciphertext.
  private[facsimile] def validate(pdf: Pdf^, password: Optional[Text]): Unit raises PdfError =
    pdf.trailer.at(t"Encrypt").let: encrypt =>
      val revision = pdf.resolved(encrypt)(t"V").let(_.long).or(0L).toInt
      abort(PdfError(PdfError.Reason.UnsupportedEncryption(revision)))

  // The header comment is nominally at offset 0, but tolerated anywhere in the first 1KiB,
  // matching widespread reader behaviour for files with prepended junk.
  private[facsimile] def readVersion(source: ByteSource): Version raises PdfError =
    val window = source.read(0L, source.size.min(1024L).toInt)
    val marker = t"%PDF-"

    def digit(byte: Int): Boolean = byte >= '0' && byte <= '9'

    var found: Optional[Version] = Unset
    var i = 0

    while found.absent && i <= window.length - 8 do
      var j = 0
      while j < marker.length && (window(i + j) & 0xff) == marker.s.charAt(j).toInt do j += 1

      if j == marker.length && digit(window(i + 5) & 0xff) &&
        (window(i + 6) & 0xff) == '.' && digit(window(i + 7) & 0xff)
      then found = Version((window(i + 5) & 0xff) - '0', (window(i + 7) & 0xff) - '0')
      else i += 1

    found.or(abort(PdfError(PdfError.Reason.NotPdf)))

// An open PDF document: the scoped capability at the heart of the module. It owns the byte
// source and a cache of resolved objects, so anything that can still touch the file — the
// `Pdf` itself, and any lazily-resolving value that captures it — is confined to the `open`
// scope by capture checking, while decoded `Data` and parsed `Cos` values are pure and
// escape freely. No method anywhere dereferences a `Cos.Body` except through this class.
class Pdf private[facsimile]
  ( private[facsimile] val source: ByteSource,
    private[facsimile] val xref: Xref,
    val version: Pdf.Version )
extends caps.ExclusiveCapability:

  private val cache: scala.collection.mutable.HashMap[Int, Cos] =
    scala.collection.mutable.HashMap()

  private val loading: scala.collection.mutable.HashSet[Int] =
    scala.collection.mutable.HashSet()

  private val containers: scala.collection.mutable.HashMap[Int, ObjectStream] =
    scala.collection.mutable.HashMap()

  def trailer: Map[Text, Cos] = xref.trailer

  def catalog: Map[Text, Cos] raises PdfError =
    resolved(trailer.at(t"Root").or(Cos.Nil)).dictionary
    . or(abort(PdfError(PdfError.Reason.MissingEntry(t"Root"))))

  // The page tree flattened into reading order, with the inheritable attributes accumulated
  // along each path; the object number of each leaf is kept so that destinations can refer
  // back to a page by reference.
  private[facsimile] def pageEntries
  :   Vector[(Optional[Int], Map[Text, Cos], Page.Inherited)] raises PdfError =

    var visited = Set[Int]()

    def recur(node: Cos, number: Optional[Int], inherited: Page.Inherited)
    :   Vector[(Optional[Int], Map[Text, Cos], Page.Inherited)] =

      node match
        case Cos.Ref(reference, _) =>
          if visited.contains(reference)
          then abort(PdfError(PdfError.Reason.CircularPageTree))

          visited += reference
          recur(resolved(node), reference, inherited)

        case Cos.Dictionary(entries) => entries.at(t"Type").let(_.name) match
          case t"Pages" =>
            val updated = inherited.update(entries)

            resolved(entries.at(t"Kids").or(Cos.Nil)).elements.lay(Vector()): kids =>
              kids.to(Vector).flatMap(recur(_, Unset, updated))

          case _ =>
            Vector((number, entries, inherited))

        case _ =>
          Vector()

    recur(catalog.at(t"Pages").or(Cos.Nil), Unset, Page.Inherited())

  def pages: Vector[Page^{this}] raises PdfError =
    pageEntries.zipWithIndex.map: (entry, index) =>
      Page(this, index.z, entry(1), entry(2))

  // Leaf object numbers mapped to positions in the flattened page sequence, for resolving
  // destinations that refer to pages by reference.
  private[facsimile] def pageNumbers: Map[Int, Ordinal] raises PdfError =
    pageEntries.zipWithIndex.flatMap: (entry, index) =>
      entry(0).lay(List()): number =>
        List(number -> index.z)

    . to(Map)

  // Named destinations from both homes: the old-style `/Dests` dictionary and the
  // `/Names /Dests` name tree, still as raw COS values.
  private[facsimile] def rawDestinations: Map[Text, Cos] raises PdfError =
    val old = resolved(catalog.at(t"Dests").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())

    val tree = resolved(catalog.at(t"Names").or(Cos.Nil))(t"Dests")
      . let(Trees.names(_)(using this).to(Map)).or(Map[Text, Cos]())

    old ++ tree

  def destinations: Map[Text, Destination] raises PdfError =
    val pages = pageNumbers
    val raw = rawDestinations

    raw.to(List).flatMap: (name, value) =>
      Destination.read(value, pages, raw.at(_))(using this)
      . lay(List()): destination =>
          List(name -> destination)

    . to(Map)

  def bookmarks: List[Bookmark] raises PdfError =
    val pages = pageNumbers
    val raw = rawDestinations
    var visited = Set[Int]()

    // `/Dest` directly, or the `/D` of a `/GoTo` action.
    def target(entries: Map[Text, Cos]): Optional[Cos] raises PdfError =
      entries.at(t"Dest").or:
        val action = resolved(entries.at(t"A").or(Cos.Nil))

        if action(t"S").let(_.name).or(t"") == t"GoTo" then action(t"D") else Unset

    def item(value: Cos): List[Bookmark] raises PdfError = value match
      case Cos.Ref(number, _) =>
        if visited.contains(number) then List() else
          visited += number
          item(resolved(value))

      case Cos.Dictionary(entries) =>
        val title = entries.at(t"Title").let(resolved(_).text).or(t"")

        val destination =
          target(entries).let(Destination.read(_, pages, raw.at(_))(using this))

        Bookmark(title, destination, chain(entries.at(t"First"))) ::
          chain(entries.at(t"Next"))

      case _ =>
        List()

    def chain(first: Optional[Cos]): List[Bookmark] raises PdfError =
      first.lay(List())(item(_))

    chain(resolved(catalog.at(t"Outlines").or(Cos.Nil))(t"First"))

  def attachments: List[Pdf.Attachment^{this}] raises PdfError =
    resolved(catalog.at(t"Names").or(Cos.Nil))(t"EmbeddedFiles").lay(List()): tree =>
      Trees.names(tree)(using this).map: (name, value) =>
        val spec = resolved(value).dictionary.or(Map[Text, Cos]())
        val filename = spec.at(t"UF").or(spec.at(t"F")).let(resolved(_).text)
        val description = spec.at(t"Desc").let(resolved(_).text)
        val files = resolved(spec.at(t"EF").or(Cos.Nil))

        val body: Optional[Cos.Body] =
          resolved(files(t"UF").or(files(t"F")).or(Cos.Nil)) match
            case body: Cos.Body => body
            case _              => Unset

        val mediaType = body.let(_.entries.at(t"Subtype")).let(_.name)
        Pdf.Attachment(this, name, filename, description, mediaType, body)

  // The label a viewer displays for a page (ISO 32000-2 §12.4.2): styled and prefixed by
  // the `/PageLabels` number tree, or the plain one-based page number when absent.
  def pageLabel(index: Ordinal): Text raises PdfError =
    catalog.at(t"PageLabels").lay(index.n1.toString.tt): tree =>
      val ranges = Trees.numbers(tree)(using this).filter(_(0) <= index.n0)

      if ranges.isEmpty then index.n1.toString.tt else
        val (start, value) = ranges.maxBy(_(0))
        val entries = resolved(value).dictionary.or(Map[Text, Cos]())
        val prefix = entries.at(t"P").let(resolved(_).text).or(t"")
        val first = entries.at(t"St").let(resolved(_).long).or(1L)
        val number = first + (index.n0 - start)

        val formatted = entries.at(t"S").let(resolved(_).name).lay(t""):
          case t"D" => number.toString.tt
          case t"R" => roman(number)
          case t"r" => roman(number).s.toLowerCase.nn.tt
          case t"A" => alphabetic(number)
          case t"a" => alphabetic(number).s.toLowerCase.nn.tt
          case _    => t""

        t"$prefix$formatted"

  private def roman(number: Long): Text =
    val numerals =
      List
        ( 1000L -> "M", 900L -> "CM", 500L -> "D", 400L -> "CD", 100L -> "C", 90L -> "XC",
          50L -> "L", 40L -> "XL", 10L -> "X", 9L -> "IX", 5L -> "V", 4L -> "IV", 1L -> "I" )

    def recur(remaining: Long, numerals: List[(Long, String)], result: String): String =
      numerals match
        case (value, numeral) :: rest =>
          if remaining >= value then recur(remaining - value, numerals, result + numeral)
          else recur(remaining, rest, result)

        case _ =>
          result

    if number <= 0 then t"" else recur(number, numerals, "").tt

  // A, B, ..., Z, AA, BB, ..., ZZ, AAA, ... — the same letter repeated, per the spec.
  private def alphabetic(number: Long): Text =
    if number <= 0 then t"" else
      val letter = ('A' + ((number - 1)%26)).toChar.toString
      letter.repeat((((number - 1)/26) + 1).toInt).nn.tt

  // The document-level XMP packet, undecoded: XML parsing belongs downstream.
  def xmp: Optional[Data] raises PdfError =
    resolved(catalog.at(t"Metadata").or(Cos.Nil)) match
      case body: Cos.Body => payload(body)
      case _              => Unset

  def info: PdfInfo raises PdfError =
    val entries = resolved(trailer.at(t"Info").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())
    def field(key: Text): Optional[Text] = entries.at(key).let(resolved(_).text)

    PdfInfo
      ( field(t"Title"), field(t"Author"), field(t"Subject"), field(t"Keywords"),
        field(t"Creator"), field(t"Producer"),
        field(t"CreationDate").let(PdfInfo.parseDate(_)),
        field(t"ModDate").let(PdfInfo.parseDate(_)) )

  def apply(ref: Cos.Ref): Cos raises PdfError = apply(ref.number, ref.generation)

  // Resolves an object by number and generation: from the cache, from its recorded file
  // offset, or by extraction from a containing object stream. A missing or free entry, or a
  // generation mismatch, is `null` per ISO 32000-2 §7.3.10.
  def apply(number: Int, generation: Int = 0): Cos raises PdfError =
    cache.at(number).or:
      if !loading.add(number) then abort(PdfError(PdfError.Reason.CircularReference(number)))

      try
        val resolution = xref.entries.at(number) match
          case Xref.Entry.Direct(offset, expected) =>
            if expected != generation then Cos.Nil else
              val parser = CosParser(CosLexer(new Scan(source, offset)))
              val (foundNumber, foundGeneration, content) = parser.indirect()

              if foundNumber != number || foundGeneration != generation
              then abort(PdfError(PdfError.Reason.MissingObject(number, generation)))

              content

          case Xref.Entry.Compressed(container, index) =>
            if generation != 0 then Cos.Nil else
              containerStream(container)(number)
              . or(abort(PdfError(PdfError.Reason.MissingObject(number, generation))))

          case _ =>
            Cos.Nil

        cache(number) = resolution
        resolution
      finally loading.remove(number)

  def resolved(value: Cos): Cos raises PdfError = value match
    case ref: Cos.Ref => apply(ref)
    case other        => other

  // The decoded content of a stream, decrypted (in a later milestone) and passed through its
  // filter chain, which stops at terminal image codecs. `/Length` may be indirect; filters in
  // a general stream may be too, so the chain inputs are resolved through this document.
  def payload(body: Cos.Body): Data raises PdfError =
    val length = resolved(body.entries.at(t"Length").or(Cos.Nil)).long
      . or(abort(PdfError(PdfError.Reason.MissingEntry(t"Length")))).toInt

    val raw = source.read(body.start, length)
    if raw.length < length then abort(PdfError(PdfError.Reason.Truncated))

    val chain =
      Filter.chain
        ( body.entries.at(t"Filter").let(deepResolved(_)),
          body.entries.at(t"DecodeParms").let(deepResolved(_)) )

    Filter.decode(raw, chain)

  // Resolves a value and, one level down, the elements of an array or the values of a
  // dictionary: sufficient for `/Filter` and `/DecodeParms` shapes.
  private def deepResolved(value: Cos): Cos raises PdfError = resolved(value) match
    case Cos.Sequence(elements)  => Cos.Sequence(elements.map(resolved(_)))
    case Cos.Dictionary(entries) => Cos.Dictionary(entries.view.mapValues(resolved(_)).toMap)
    case other                   => other

  private def containerStream(container: Int): ObjectStream raises PdfError =
    containers.at(container).or:
      val stream = apply(container) match
        case body @ Cos.Body(entries, _) =>
          val data = payload(body)

          val first = entries.at(t"First").let(_.long)
            . or(abort(PdfError(PdfError.Reason.MissingEntry(t"First")))).toInt

          val count = entries.at(t"N").let(_.long)
            . or(abort(PdfError(PdfError.Reason.MissingEntry(t"N")))).toInt

          ObjectStream(data, first, count)

        case _ =>
          abort(PdfError(PdfError.Reason.MissingObject(container, 0)))

      containers(container) = stream
      stream
