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

import scala.collection.immutable.Vector

import scala.caps

import proscenium.compat.*

import anticipation.*
import contingency.*
import denominative.*
import enigmatic.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*
import pneumatic.*

object Pdf:
  // A fresh, empty document: a catalog and an empty page tree, over which a creation scope's
  // edits accumulate before a full write. Built in memory so the write extensions — which
  // resolve through a `Pdf` — work identically to editing an existing file.
  private[facsimile] def blank(): Pdf raises PdfError =
    val catalog = t"1 0 obj\n<< /Type /Catalog /Pages 2 0 R >>\nendobj\n"
    val pages = t"2 0 obj\n<< /Type /Pages /Kids [] /Count 0 >>\nendobj\n"
    val body = t"%PDF-1.7\n$catalog$pages"

    val offset1 = body.s.indexOf("1 0 obj")
    val offset2 = body.s.indexOf("2 0 obj")
    val xrefOffset = body.length

    def pad(value: Int): Text =
      val digits = value.toString
      ("0".repeat(10 - digits.length).nn + digits).tt

    val table =
      t"xref\n0 3\n0000000000 65535 f \n${pad(offset1)} 00000 n \n${pad(offset2)} 00000 n \n"

    val trailer = t"trailer\n<< /Size 3 /Root 1 0 R >>\nstartxref\n$xrefOffset\n%%EOF"
    val bytes = charEncoders.iso88591Encoder.encoded(t"$body$table$trailer")
    val source = DataSource(bytes)
    Pdf(source, Xref.load(source), Version(1, 7))

  // Anchored here — the form's companion — so `path.open[Pdf]()` and `data.open[Pdf]()`
  // resolve with no import.
  given pathOpenable: [path: Abstractable across Paths to Text]
  =>  Tactic[PdfError]
  =>  ( PdfFile.PdfPathOpenable[path]^ ) =
    PdfFile.PdfPathOpenable[path]

  given dataOpenable: Tactic[PdfError] => ( PdfFile.PdfDataOpenable^ ) =
    PdfFile.PdfDataOpenable()

  // Anchored here so `path.create[Pdf](): doc ?=> …` resolves the `Pdf` form with no import.
  given creatable: [path: Abstractable across Paths to Text]
  =>  Tactic[PdfError]
  =>  ( PdfFile.PdfCreatable[path]^ ) =
    PdfFile.PdfCreatable[path]

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

  // Builds the security handler, if the file is encrypted, and installs it on the document.
  // The `/Encrypt` dictionary and the trailer `/ID` are read before the guard exists — and
  // so are never themselves decrypted — and a wrong password fails here, at open, rather
  // than at first string or stream access. The password's cleartext is read only within
  // `uncloak`, so it is confined to this call; the empty password covers unprotected files.
  private[facsimile] def unlock(pdf: Pdf^, password: Optional[Password]): Unit raises PdfError =
    pdf.trailer.at(t"Encrypt").let: encryptRef =>
      val encrypt = pdf.resolved(encryptRef).dictionary
        . or(abort(PdfError(PdfError.Reason.UnsupportedEncryption(0))))

      val id = pdf.trailer.at(t"ID") match
        case Cos.Sequence(first :: _) => first.chars.or(IArray.empty[Byte])
        case _                        => IArray.empty[Byte]

      pdf.guard = password.lay(Guard(encrypt, id, Array.empty[Char])(using pdf)): password =>
        password.uncloak(Guard(encrypt, id, cleartext.chars)(using pdf))

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

  // The stream at each recorded payload offset belongs to this indirect object, so its bytes
  // can be decrypted with the right per-object key. Populated as `Direct` objects load.
  private val streamOwners: scala.collection.mutable.HashMap[Long, (Int, Int)] =
    scala.collection.mutable.HashMap()

  // The security handler, installed by `Pdf.unlock` after the document exists (it must read
  // the unencrypted `/Encrypt` dictionary through this same document first).
  private[facsimile] var guard: Optional[Guard] = Unset

  // The write overlay: an in-memory incremental update layered over the immutable read model.
  // New and replaced objects live in `overlay`, deleted objects in `freed`; `apply` consults
  // them first, so every existing read view reflects pending edits with no change of its own.
  // Empty in a read-only session; serialised and appended to the file when the write scope
  // closes.
  private[facsimile] val overlay: scala.collection.mutable.HashMap[Int, Cos] =
    scala.collection.mutable.HashMap()

  private[facsimile] val freed: scala.collection.mutable.HashSet[Int] =
    scala.collection.mutable.HashSet()

  // The next free object number, one past the largest the original file used.
  private[facsimile] var nextNumber: Int =
    (xref.entries.keys.maxOption.getOrElse(0).max(trailer.at(t"Size").let(_.long).or(0L).toInt - 1)) + 1

  // Payloads for streams created during the write scope. A `Cos.Body` locates its bytes by a
  // file offset; a new stream has none, so it is given a negative sentinel `start` that keys
  // its bytes here — inert to a reader, resolved to these bytes by `raw` and the writer.
  private[facsimile] val newStreams: scala.collection.mutable.HashMap[Long, Data] =
    scala.collection.mutable.HashMap()

  private var nextStreamId: Long = -1L

  // Trailer entries set or overridden during the write scope — e.g. a newly-created `/Info`
  // reference. Merged over the original trailer's carried-forward entries by the writer.
  private[facsimile] val trailerOverrides: scala.collection.mutable.HashMap[Text, Cos] =
    scala.collection.mutable.HashMap()

  // A new stream object carrying inline bytes, for content the write scope produces.
  private[facsimile] def newBody(entries: Map[Text, Cos], data: Data): Cos.Body =
    val id = nextStreamId
    nextStreamId -= 1
    newStreams(id) = data
    Cos.Body(entries.updated(t"Length", Cos.Integral(data.length.toLong)), id)

  private[facsimile] def dirty: Boolean =
    overlay.nonEmpty || freed.nonEmpty || trailerOverrides.nonEmpty

  // Records a new value for an existing or new object number; un-frees it if it was deleted.
  private[facsimile] def put(number: Int, value: Cos): Unit =
    overlay(number) = value
    freed.remove(number)

  // Allocates a fresh object number for a new value, returning a reference to it.
  private[facsimile] def allocate(value: Cos): Cos.Ref =
    val number = nextNumber
    nextNumber += 1
    overlay(number) = value
    Cos.Ref(number, 0)

  // Marks an object deleted: dropped from the overlay, and — if it exists in the base file —
  // recorded as freed so the incremental update writes a free entry for it.
  private[facsimile] def remove(number: Int): Unit =
    overlay.remove(number)
    if xref.entries.defines(number) then freed += number

  // Rewrites an object's dictionary in place, reading its current value (overlay-aware) so
  // successive edits within a scope compose.
  private[facsimile] def editDictionary(number: Int)(transform: Map[Text, Cos] => Map[Text, Cos])
  :   Unit raises PdfError =

    put(number, Cos.Dictionary(transform(apply(number).dictionary.or(Map[Text, Cos]()))))

  // Rewrites the catalog (the `/Root` object) in place.
  private[facsimile] def editCatalog(transform: Map[Text, Cos] => Map[Text, Cos])
  :   Unit raises PdfError =

    trailer.at(t"Root") match
      case ref: Cos.Ref => editDictionary(ref.number)(transform)
      case _            => ()

  // A reference to the page at a position in the flattened page sequence, for destinations.
  private[facsimile] def pageReference(ordinal: Ordinal): Optional[Cos.Ref] raises PdfError =
    val entries = pageEntries
    if ordinal.n0 < 0 || ordinal.n0 >= entries.length then Unset
    else entries(ordinal.n0)(0).let(Cos.Ref(_, 0))

  def trailer: Map[Text, Cos] = xref.trailer

  def encrypted: Boolean = trailer.defines(t"Encrypt")

  def catalog: Map[Text, Cos] raises PdfError =
    resolved(trailer.at(t"Root").or(Cos.Nil)).dictionary
    . or(abort(PdfError(PdfError.Reason.MissingEntry(t"Root"))))

  // The page tree flattened into reading order, with the inheritable attributes accumulated
  // along each path; the object number of each leaf is kept so that destinations can refer
  // back to a page by reference.
  private[facsimile] def pageEntries
  :   Vector[(Optional[Int], Map[Text, Cos], Page.Inherited)] raises PdfError =

    var visited = scala.collection.immutable.Set[Int]()

    def recur(node: Cos, number: Optional[Int], inherited: Page.Inherited)
    :   Vector[(Optional[Int], Map[Text, Cos], Page.Inherited)] =

      node match
        case Cos.Ref(reference, _) =>
          if visited.has(reference)
          then abort(PdfError(PdfError.Reason.CircularPageTree))

          visited += reference
          recur(resolved(node), reference, inherited)

        case Cos.Dictionary(entries) => entries.at(t"Type").let(_.name) match
          case t"Pages" =>
            val updated = inherited.update(entries)

            resolved(entries.at(t"Kids").or(Cos.Nil)).elements.lay(Vector()): kids =>
              kids.stdlib.toVector.flatMap(recur(_, Unset, updated))

          case _ =>
            Vector((number, entries, inherited))

        case _ =>
          Vector()

    recur(catalog.at(t"Pages").or(Cos.Nil), Unset, Page.Inherited())

  def pages: Vector[Page^{this}] raises PdfError =
    pageEntries.zipWithIndex.map: (entry, index) =>
      Page(this, index.z, entry(0), entry(1), entry(2))

  // Leaf object numbers mapped to positions in the flattened page sequence, for resolving
  // destinations that refer to pages by reference.
  private[facsimile] def pageNumbers: Map[Int, Ordinal] raises PdfError =
    pageEntries.zipWithIndex.flatMap: (entry, index) =>
      entry(0).lay(scala.collection.immutable.List()): number =>
        scala.collection.immutable.List(number -> index.z)

    . pipe(Map.from(_))

  // Named destinations from both homes: the old-style `/Dests` dictionary and the
  // `/Names /Dests` name tree, still as raw COS values.
  private[facsimile] def rawDestinations: Map[Text, Cos] raises PdfError =
    val old = resolved(catalog.at(t"Dests").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())

    val tree = resolved(catalog.at(t"Names").or(Cos.Nil))(t"Dests")
      . let(Trees.names(_)(using this).stdlib.pipe(Map.from(_))).or(Map[Text, Cos]())

    Map.of(old.stdlib ++ (tree: Map[Text, Cos]).stdlib)

  def destinations: Map[Text, Destination] raises PdfError =
    val pages = pageNumbers
    val raw = rawDestinations

    raw.stdlib.toList.flatMap: (name, value) =>
      Destination.read(value, pages, raw.at(_))(using this)
      . lay(scala.collection.immutable.List()): destination =>
          scala.collection.immutable.List(name -> destination)

    . pipe(Map.from(_))

  def bookmarks: List[Bookmark] raises PdfError =
    val pages = pageNumbers
    val raw = rawDestinations
    var visited = scala.collection.immutable.Set[Int]()

    // `/Dest` directly, or the `/D` of a `/GoTo` action.
    def target(entries: Map[Text, Cos]): Optional[Cos] raises PdfError =
      entries.at(t"Dest").or:
        val action = resolved(entries.at(t"A").or(Cos.Nil))

        if action(t"S").let(_.name).or(t"") == t"GoTo" then action(t"D") else Unset

    def item(value: Cos): List[Bookmark] raises PdfError = value match
      case Cos.Ref(number, _) =>
        if visited.has(number) then List() else
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

  // Resolves an object by number and generation: from the write overlay, the cache, its
  // recorded file offset, or a containing object stream. A freed, missing or invalid entry,
  // or a generation mismatch, is `null` per ISO 32000-2 §7.3.10.
  def apply(number: Int, generation: Int = 0): Cos raises PdfError =
    if freed.contains(number) then Cos.Nil
    else overlay.at(number).or(cache.at(number).or(load(number, generation)))

  private def load(number: Int, generation: Int): Cos raises PdfError =
    if !loading.add(number) then abort(PdfError(PdfError.Reason.CircularReference(number)))

    try
      val resolution = xref.entries.at(number) match
        case Xref.Entry.Direct(offset, expected) =>
          if expected != generation then Cos.Nil else
            // If the recorded offset does not hold this object — a corrupt or shifted
            // cross-reference table — fall back to the offset found by a full-file scan.
            val content = atOffset(number, generation, offset).or:
              recoveredOffset(number).let(atOffset(number, generation, _)).or(Cos.Nil)

            // A top-level indirect stream: record its owner so its payload can be
            // decrypted with the right key.
            content match
              case Cos.Body(_, start) => streamOwners(start) = (number, generation)
              case _                  => ()

            // Strings in a directly-stored object are encrypted individually; those inside
            // an object stream travel in its already-decrypted payload, so are skipped.
            guard.lay(content)(decryptStrings(content, number, generation, _))

        case Xref.Entry.Compressed(container, index) =>
          if generation != 0 then Cos.Nil else
            containerStream(container)(number)
            . or(abort(PdfError(PdfError.Reason.MissingObject(number, generation))))

        case _ =>
          Cos.Nil

      cache(number) = resolution
      resolution
    finally loading.remove(number)

  // Parses the object at an offset, returning its content only if the header matches the
  // number and generation asked for; a mismatch (a lie in the cross-reference table) is
  // `Unset`, so the caller can try a recovered offset instead.
  private def atOffset(number: Int, generation: Int, offset: Long): Optional[Cos] raises PdfError =
    if offset < 0 || offset >= source.size then Unset else
      safely(CosParser(CosLexer(new Scan(source, offset))).indirect()).let: (found, gen, content) =>
        if found == number && gen == generation then content else Unset

  // A cross-reference table rebuilt by scanning the whole file for objects, computed once and
  // only when the recorded table is found to be lying.
  private lazy val recovered: Xref = safely(Xref.rebuild(source)).or(Xref(Map(), Map()))

  private def recoveredOffset(number: Int): Optional[Long] = recovered.entries.at(number).let:
    case Xref.Entry.Direct(offset, _) => offset
    case _                            => Unset

  def resolved(value: Cos): Cos raises PdfError = value match
    case ref: Cos.Ref => apply(ref)
    case other        => other

  // Rewrites every string in an object with its decrypted bytes. A stream body's own
  // dictionary is decrypted, but its payload is left to `raw`; nothing is done for `Ref`s,
  // which resolve to their own separately-decrypted objects.
  private def decryptStrings(value: Cos, number: Int, generation: Int, guard: Guard): Cos =
    value match
      case Cos.Chars(bytes) =>
        Cos.Chars(guard.string(bytes, number, generation))

      case Cos.Sequence(elements) =>
        Cos.Sequence(elements.map(decryptStrings(_, number, generation, guard)))

      case Cos.Dictionary(entries) =>
        Cos.Dictionary(Map.of(entries.stdlib.view.mapValues(decryptStrings(_, number, generation, guard)).toMap))

      case Cos.Body(entries, start) =>
        val decrypted = Map.of(entries.stdlib.view.mapValues(decryptStrings(_, number, generation, guard)).toMap)
        Cos.Body(decrypted, start)

      case other =>
        other

  // The decoded content of a stream, decrypted (in a later milestone) and passed through its
  // filter chain, which stops at terminal image codecs. `/Length` may be indirect; filters in
  // a general stream may be too, so the chain inputs are resolved through this document.
  def payload(body: Cos.Body): Data raises PdfError =
    val chain =
      Filter.chain
        ( body.entries.at(t"Filter").let(deepResolved(_)),
          body.entries.at(t"DecodeParms").let(deepResolved(_)) )

    Filter.decode(raw(body), chain)

  // A re-materializable streaming view of the decoded payload: each `apply()` mints a fresh
  // pull endpoint reading the raw range in chunks and decoding through the filter chain, so
  // a large image or embedded file is never materialized whole. The endpoint reads through
  // this document, so — like everything that does — it cannot outlive the `open` scope.
  def spring(body: Cos.Body)(using tactic: Tactic[PdfError]): Spring[Data]^{this, tactic} =
    val chain =
      Filter.chain
        ( body.entries.at(t"Filter").let(deepResolved(_)),
          body.entries.at(t"DecodeParms").let(deepResolved(_)) )

    val steps = Filter.steps(chain)
    val start = body.start
    val end = payloadEnd(body)

    // An encrypted stream is decrypted whole before filtering (a cipher spans the payload),
    // so it starts the pipeline as one materialized chunk; a plain stream reads in chunks.
    val decrypted: Optional[Data] = if encryptedStream(body) then raw(body) else Unset

    new Spring[Data]:
      def apply(): (Stream[Data] over Credit)^ =
        decrypted.lay(pipeline(steps, Stream(ranges(start, end)))): data =>
          pipeline(steps, Stream(List(data).iterator))

  // Interprets a streaming plan, minting each duct at its `via` call site.
  private def pipeline(steps: List[Filter.Step^], consume stream: (Stream[Data] over Credit)^)
  :   (Stream[Data] over Credit)^ =

    steps match
      case Filter.Step.Inflate :: rest =>
        pipeline(rest, stream.via(pneumatic.Zlib.compression.decompressor()))

      case Filter.Step.Unlzw(earlyChange) :: rest =>
        pipeline(rest, stream.via(pneumatic.Lzw.decompressor(earlyChange)))

      case Filter.Step.Gather(transform) :: rest =>
        pipeline(rest, stream.via(Gathering(transform)))

      case _ =>
        stream

  // Chunked positional reads over a raw range: the pull side of `spring`.
  private def ranges(start: Long, end: Long): Iterator[Data]^{this} = new Iterator[Data]:
    private var position: Long = start

    def hasNext: Boolean = position < end

    def next(): Data =
      val length = (end - position).min(65536L).toInt
      val chunk = source.read(position, length)
      position = if chunk.length == 0 then end else position + chunk.length
      chunk

  // The raw payload, decrypted if the document is encrypted and this stream is not exempt. A
  // stream created in this scope (negative sentinel start) yields its inline bytes directly.
  private[facsimile] def raw(body: Cos.Body): Data raises PdfError =
    if body.start < 0 then newStreams.at(body.start).or(IArray.empty[Byte]) else
      val bytes = source.read(body.start, (payloadEnd(body) - body.start).toInt)

      if !encryptedStream(body) then bytes else
        guard.lay(bytes): guard =>
          streamOwners.at(body.start).lay(bytes): (number, generation) =>
            guard.stream(bytes, number, generation, Unset)

  // Whether a stream's raw bytes need decrypting: the document is encrypted and the stream is
  // not exempt — cross-reference streams (never encrypted), metadata under `/EncryptMetadata
  // false`, and streams marked with the `Identity` crypt filter.
  private def encryptedStream(body: Cos.Body): Boolean raises PdfError = guard.lay(false): guard =>
    val kind = body.entries.at(t"Type").let(_.name).or(t"")

    val exempt =
      kind == t"XRef"
      || (kind == t"Metadata" && !guard.encryptMetadata)
      || cryptMethod(body) == Guard.Method.Identity

    !exempt && streamOwners.contains(body.start)

  // A `/Crypt` filter in the stream's filter chain selects a crypt method by name; `Identity`
  // (the default) means the stream is stored in the clear.
  private def cryptMethod(body: Cos.Body): Optional[Guard.Method] raises PdfError =
    val filters = deepResolved(body.entries.at(t"Filter").or(Cos.Nil))

    val hasCrypt = filters match
      case Cos.Name(t"Crypt")     => true
      case Cos.Sequence(elements) => elements.exists(_.name == t"Crypt")
      case _                      => false

    if !hasCrypt then Unset else
      val parms = deepResolved(body.entries.at(t"DecodeParms").or(Cos.Nil))

      val name = parms match
        case Cos.Dictionary(entries) => entries.at(t"Name").let(_.name)
        case Cos.Sequence(elements)  =>
          elements.flatMap(_.dictionary.let(_.at(t"Name")).let(_.name).lay(List())(List(_))).headOption
            . getOrElse(Unset)
        case _                       => Unset

      if name == t"Identity" || name.absent then Guard.Method.Identity else Unset

  // The exclusive end of the payload: `/Length` bytes when the declared length checks out —
  // the `endstream` keyword must follow it — and otherwise, since wrong lengths abound in
  // real files, the nearest `endstream`, less the end-of-line before it.
  private def payloadEnd(body: Cos.Body): Long raises PdfError =
    resolved(body.entries.at(t"Length").or(Cos.Nil)).long.let: length =>
      val end = body.start + length
      if length >= 0 && end <= source.size && endstreamFollows(end) then end else Unset

    . or:
        val marker = t"endstream"
        val chunkSize = 65536
        var offset = body.start
        var found: Optional[Long] = Unset

        while found.absent && offset < source.size do
          val chunk = source.read(offset, chunkSize + marker.length - 1)
          var i = 0

          while found.absent && i <= chunk.length - marker.length do
            var j = 0
            while j < marker.length && (chunk(i + j) & 0xff) == marker.s.charAt(j).toInt do j += 1
            if j == marker.length then found = offset + i else i += 1

          offset += chunkSize

        found.let: position =>
          // The end-of-line before `endstream` belongs to the syntax, not the payload.
          val windowStart = (position - 2).max(body.start)
          val window = source.read(windowStart, (position - windowStart).toInt)
          val last = if window.length >= 1 then window(window.length - 1) & 0xff else -1
          val prior = if window.length >= 2 then window(window.length - 2) & 0xff else -1

          if prior == 0x0d && last == 0x0a then position - 2
          else if last == 0x0a || last == 0x0d then position - 1
          else position

        . or(abort(PdfError(PdfError.Reason.Truncated)))

  private def endstreamFollows(position: Long): Boolean =
    val marker = t"endstream"
    val window = source.read(position, 24)
    var i = 0

    while i < window.length && CosLexer.whitespace(window(i) & 0xff) do i += 1

    if i + marker.length > window.length then false else
      var j = 0
      while j < marker.length && (window(i + j) & 0xff) == marker.s.charAt(j).toInt do j += 1
      j == marker.length

  // Resolves a value and, one level down, the elements of an array or the values of a
  // dictionary: sufficient for `/Filter` and `/DecodeParms` shapes.
  private def deepResolved(value: Cos): Cos raises PdfError = resolved(value) match
    case Cos.Sequence(elements)  => Cos.Sequence(elements.map(resolved(_)))
    case Cos.Dictionary(entries) => Cos.Dictionary(Map.of(entries.stdlib.view.mapValues(resolved(_)).toMap))
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
