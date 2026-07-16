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
import gossamer.*
import rudiments.*
import vacuous.*

object Pdf:
  case class Version(major: Int, minor: Int)

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

      if j == marker.length
         && digit(window(i + 5) & 0xff)
         && (window(i + 6) & 0xff) == '.'
         && digit(window(i + 7) & 0xff)
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

    val chain = Filter.chain
      ( body.entries.at(t"Filter").let(deepResolved(_)),
        body.entries.at(t"DecodeParms").let(deepResolved(_)) )

    Filter.decode(raw, chain)

  // Resolves a value and, one level down, the elements of an array or the values of a
  // dictionary: sufficient for `/Filter` and `/DecodeParms` shapes.
  private def deepResolved(value: Cos): Cos raises PdfError = resolved(value) match
    case Cos.Sequence(elements) => Cos.Sequence(elements.map(resolved(_)))
    case Cos.Dictionary(entries) => Cos.Dictionary(entries.view.mapValues(resolved(_)).toMap)
    case other => other

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
