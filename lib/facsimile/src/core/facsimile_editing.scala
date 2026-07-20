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

import proscenium.compat.*

import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import gossamer.*
import phoenicia.*
import quantitative.*
import rudiments.*
import vacuous.*

// The low-level write surface. These operations are extension methods on the write-granted
// handle — `(Pdf & Granting[Grant.Write])^`, as galilei gates its `write` — so they exist
// only inside a scope opened with `Write`; reading is available in every mode on bare `Pdf`.
// The document's `apply` consults the overlay first, so a value written here is visible to
// every read view immediately; the overlay is serialised and appended when the scope closes.
extension (pdf: (Pdf & Granting[Grant.Write])^)
  // Replaces (or defines) an object by reference. (Not overloaded — an overloaded extension
  // method fails to reduce aperture's abstract `Result`/`Grants` at the use site — and named
  // `set` rather than `update`, whose assignment sugar collides with other `update`s.)
  def set(ref: Cos.Ref, value: Cos): Unit = pdf.put(ref.number, value)

  // Adds a new object, returning a reference to it.
  def allocate(value: Cos): Cos.Ref = pdf.allocate(value)

  // A new stream object carrying the given bytes (uncompressed), to be `allocate`d or
  // referenced. Its `/Length` is set automatically.
  def newStream(data: Data, entries: Map[Text, Cos] = Map()): Cos.Body = pdf.newBody(entries, data)

  // Deletes an object: in PDF terms, its number becomes a free cross-reference entry. (Named
  // `free` rather than `delete` to avoid colliding with galilei's `delete` in `soundness`.)
  def free(ref: Cos.Ref): Unit = pdf.remove(ref.number)

  // Replaces a page's content with the given operators: they are serialised to a fresh
  // content stream, and the page's `/Contents` is pointed at it.
  def setContents(page: Page^, operators: List[PdfOperator]): Unit raises PdfError =
    val stream = pdf.allocate(pdf.newBody(Map(), ContentWriter.write(operators)))

    editPage(pdf, page): entries =>
      entries.updated(t"Contents", stream)

  // Sets a page's rotation.
  def setRotation(page: Page^, rotation: Page.Rotation): Unit raises PdfError =
    val degrees = rotation match
      case Page.Rotation.None          => 0L
      case Page.Rotation.Quarter       => 90L
      case Page.Rotation.Half          => 180L
      case Page.Rotation.ThreeQuarters => 270L

    editPage(pdf, page)(_.updated(t"Rotate", Cos.Integral(degrees)))

  // Sets a page box (`/MediaBox`, `/CropBox`, …) to a rectangle in points.
  def setBox(page: Page^, box: Text, rect: PdfRect): Unit raises PdfError =
    val array =
      Cos.Sequence:
        List(rect.left, rect.bottom, rect.right, rect.top).map(_.value).map(number(_))

    editPage(pdf, page)(_.updated(box, array))

  // Sets an arbitrary entry in a page's dictionary.
  def setPageEntry(page: Page^, key: Text, value: Cos): Unit raises PdfError =
    editPage(pdf, page)(_.updated(key, value))

  // Replaces the document information dictionary. The existing `/Info` object is edited if
  // there is one, else a fresh object is created and linked from the trailer.
  def setInfo(info: PdfInfo): Unit raises PdfError =
    val dict = Cos.Dictionary(PdfInfo.dictionary(info))

    pdf.trailer.at(t"Info") match
      case ref: Cos.Ref =>
        pdf.put(ref.number, dict)

      case _ =>
        val ref = pdf.allocate(dict)
        pdf.trailerOverrides(t"Info") = ref

  // Embeds a font program as a simple WinAnsi TrueType font, returning a reference to the
  // `/Font` dictionary. The name defaults to the font's own PostScript name; with `subset`,
  // only the glyphs the given text needs are embedded, under a tagged name per the PDF
  // convention.
  def embedFont(font: Ttf, name: Optional[Text] = Unset, subset: Optional[Text] = Unset)
  :   Cos.Ref raises PdfError =

    FontEmbedder.embed(pdf, font, name, subset)

  // Registers a resource (a font, XObject, …) in a page's `/Resources` under a category and
  // name — the name content operators refer to, e.g. `/F1` for a font used by `Tf`.
  def addResource(page: Page^, category: Text, name: Text, resource: Cos.Ref)
  :   Unit raises PdfError =

    editPage(pdf, page): entries =>
      val resources = pdf.resolved(entries.at(t"Resources").or(Cos.Nil)).dictionary
        . or(Map[Text, Cos]())

      val existing = pdf.resolved(resources.at(category).or(Cos.Nil)).dictionary
        . or(Map[Text, Cos]())

      val category0 = Cos.Dictionary(existing.updated(name, resource))
      entries.updated(t"Resources", Cos.Dictionary(resources.updated(category, category0)))

  // Sets a page's annotations from raw annotation dictionaries: each becomes an indirect
  // object, and the page's `/Annots` is set to the array of references.
  def setAnnotations(page: Page^, annotations: List[Cos]): Unit raises PdfError =
    val refs = annotations.map(pdf.allocate(_))
    editPage(pdf, page)(_.updated(t"Annots", Cos.Sequence(refs)))

  // Adds a link annotation over a rectangle, targeting either a URI or an in-document
  // destination, appending it to the page's existing annotations.
  def addLink
    ( page: Page^, rect: PdfRect, uri: Optional[Text] = Unset,
      destination: Optional[Destination] = Unset )
  :   Unit raises PdfError =

    val box =
      Cos.Sequence:
        List(rect.left, rect.bottom, rect.right, rect.top).map(_.value).map(number(_))

    var dict: Map[Text, Cos] =
      Map(t"Type" -> Cos.Name(t"Annot"), t"Subtype" -> Cos.Name(t"Link"), t"Rect" -> box)

    uri.let: target =>
      val action =
        Map[Text, Cos](t"S" -> Cos.Name(t"URI"), t"URI" -> Cos.Chars(Cos.encodeText(target)))

      dict = dict.updated(t"A", Cos.Dictionary(action))

    destination.let: dest =>
      dict = dict.updated(t"Dest", destinationArray(pdf, dest))

    val ref = pdf.allocate(Cos.Dictionary(dict))

    editPage(pdf, page): entries =>
      val existing = entries.at(t"Annots").let(pdf.resolved(_).elements).or(Nil)
      entries.updated(t"Annots", Cos.Sequence(existing :+ ref))

  // Replaces the document outline (bookmarks). The tree is rebuilt as fresh objects with the
  // full `/First`/`/Last`/`/Next`/`/Prev`/`/Parent`/`/Count` linkage, and the catalog's
  // `/Outlines` is pointed at the new root.
  def setBookmarks(bookmarks: List[Bookmark]): Unit raises PdfError =
    val rootRef = pdf.allocate(Cos.Nil)
    val (first, last, total) = buildOutline(pdf, bookmarks, rootRef)

    var root: Map[Text, Cos] =
      Map(t"Type" -> Cos.Name(t"Outlines"), t"Count" -> Cos.Integral(total.toLong))

    first.let: ref =>
      root = root.updated(t"First", ref)

    last.let: ref =>
      root = root.updated(t"Last", ref)

    pdf.put(rootRef.number, Cos.Dictionary(root))

    pdf.editCatalog(_.updated(t"Outlines", rootRef))

  // Appends a new page of the given size to the end of the page tree, with optional content,
  // returning a reference to it. The page tree's single-level `/Kids` grows and `/Count`
  // increments; deeper trees are handled by appending to the root's kids.
  def appendPage
    ( mediaBox: PdfRect, operators: List[PdfOperator] = Nil,
      resources: Optional[Cos] = Unset )
  :   Cos.Ref raises PdfError =

    val root = pdf.catalog.at(t"Pages").or(abort(PdfError(PdfError.Reason.MissingEntry(t"Pages"))))

    val rootRef = root match
      case ref: Cos.Ref => ref
      case _            => abort(PdfError(PdfError.Reason.TypeMismatch(t"Pages", t"a reference")))

    val box =
      Cos.Sequence:
        List(mediaBox.left, mediaBox.bottom, mediaBox.right, mediaBox.top).map(_.value)
        . map(number(_))

    var entries = Map(t"Type" -> Cos.Name(t"Page"), t"Parent" -> rootRef, t"MediaBox" -> box)

    resources.let: value =>
      entries = entries.updated(t"Resources", value)

    if operators.nonEmpty then
      val stream = pdf.allocate(pdf.newBody(Map(), ContentWriter.write(operators)))
      entries = entries.updated(t"Contents", stream)

    val pageRef = pdf.allocate(Cos.Dictionary(entries))

    pdf.editDictionary(rootRef.number): tree =>
      val kids = tree.at(t"Kids").let(pdf.resolved(_).elements).or(Nil)
      val count = tree.at(t"Count").let(_.long).or(kids.length.toLong)

      tree.updated(t"Kids", Cos.Sequence(kids :+ pageRef))
        . updated(t"Count", Cos.Integral(count + 1))

    pageRef

  // Removes a page: unlinked from its parent's `/Kids`, `/Count` decremented, and its object
  // freed. Only single-level page trees are handled (the common case); a page nested deeper
  // is left in place.
  def removePage(page: Page^): Unit raises PdfError =
    page.number.let: pageNumber =>
      val parent = page.entries.at(t"Parent")

      parent.let: parentRef =>
        parentRef match
          case ref: Cos.Ref =>
            pdf.editDictionary(ref.number): tree =>
              val kids = tree.at(t"Kids").let(pdf.resolved(_).elements).or(Nil)

              val remaining = kids.filter:
                case Cos.Ref(number, _) => number != pageNumber
                case _                  => true

              tree.updated(t"Kids", Cos.Sequence(remaining))
                . updated(t"Count", Cos.Integral(remaining.length.toLong))

            pdf.remove(pageNumber)

          case _ =>
            ()

private def editPage
  ( pdf: (Pdf & Granting[Grant.Write])^, page: Page^ )
  ( transform: Map[Text, Cos] => Map[Text, Cos] )
:   Unit raises PdfError =

  page.number.let(pdf.editDictionary(_)(transform))

private def number(value: Double): Cos =
  if value == value.toLong.toDouble then Cos.Integral(value.toLong) else Cos.Real(value)

// Encodes text for a simple WinAnsi font, for a `ShowText` operand:
// `ShowText(winAnsi(t"Hello"))`.
def winAnsi(text: Text): Data = PdfEncoding.winAnsiEncode(text)

// Builds one level of the outline tree — allocating an object per bookmark, linking siblings,
// recursing into children — and returns the first and last child references and the total
// descendant count (used for `/Count`).
private def buildOutline
  ( pdf: (Pdf & Granting[Grant.Write])^, items: List[Bookmark], parent: Cos.Ref )
:   (Optional[Cos.Ref], Optional[Cos.Ref], Int) raises PdfError =

  if items.isEmpty then (Unset, Unset, 0) else
    val refs = items.map { _ => pdf.allocate(Cos.Nil) }
    var total = items.length

    items.zip(refs).zipWithIndex.each: (pair, index) =>
      val (bookmark, ref) = pair
      val (childFirst, childLast, childCount) = buildOutline(pdf, bookmark.children, ref)
      total += childCount

      var dict: Map[Text, Cos] =
        Map(t"Title" -> Cos.Chars(Cos.encodeText(bookmark.title)), t"Parent" -> parent)

      if index > 0 then dict = dict.updated(t"Prev", refs(index - 1))
      if index < refs.length - 1 then dict = dict.updated(t"Next", refs(index + 1))

      childFirst.let: first =>
        dict = dict.updated(t"First", first)

      childLast.let: last =>
        dict = dict.updated(t"Last", last)

      if childCount > 0 then dict = dict.updated(t"Count", Cos.Integral(childCount.toLong))

      bookmark.destination.let: dest =>
        dict = dict.updated(t"Dest", destinationArray(pdf, dest))

      pdf.put(ref.number, Cos.Dictionary(dict))

    (refs.headOption.getOrElse(Unset), refs.lastOption.getOrElse(Unset), total)

// A destination as its explicit `[page /Mode …]` array.
private def destinationArray(pdf: (Pdf & Granting[Grant.Write])^, dest: Destination)
:   Cos raises PdfError =

  val page: Cos = pdf.pageReference(dest.page).or(Cos.Nil)
  def opt(value: Optional[Double]): Cos = value.lay(Cos.Nil)(number(_))

  dest match
    case Destination.Fit(_)             => Cos.Sequence(List(page, Cos.Name(t"Fit")))
    case Destination.FitBox(_)          => Cos.Sequence(List(page, Cos.Name(t"FitB")))
    case Destination.FitWidth(_, t)     => Cos.Sequence(List(page, Cos.Name(t"FitH"), opt(t)))
    case Destination.FitHeight(_, l)    => Cos.Sequence(List(page, Cos.Name(t"FitV"), opt(l)))
    case Destination.FitBoxWidth(_, t)  => Cos.Sequence(List(page, Cos.Name(t"FitBH"), opt(t)))
    case Destination.FitBoxHeight(_, l) => Cos.Sequence(List(page, Cos.Name(t"FitBV"), opt(l)))

    case Destination.Xyz(_, l, t, z) =>
      Cos.Sequence(List(page, Cos.Name(t"XYZ"), opt(l), opt(t), opt(z)))

    case Destination.FitRect(_, rect) =>
      Cos.Sequence(List(page, Cos.Name(t"FitR"), number(rect.left.value), number(rect.bottom.value),
          number(rect.right.value), number(rect.top.value)))
