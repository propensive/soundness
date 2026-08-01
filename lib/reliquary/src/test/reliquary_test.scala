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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package reliquary

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import probably.*
import rudiments.*
import stratiform.*
import turbulence.*
import vacuous.*

import monotonous.*
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import alphabets.hexLowerCase
import Tel.given

object Tests extends Suite(m"Reliquary Tests"):
  // The pinned serialization of the two-blob stream `["a", "bc"]` (golden bytes of §8.2):
  // `uvarint(1) ++ "a" ++ uvarint(2) ++ "bc"`, records in ascending blob-hash order.
  val goldenStream: Text = t"0161026263"

  def encode(text: Text): Data = charEncoders.utf8Encoder.encoded(text)

  def resource(name: Text): Data =
    val stream = getClass.getResourceAsStream(s"/reliquary/${name}.tel").nn
    val bytes = stream.readAllBytes().nn
    stream.close()
    Array.unsafeFrozen(bytes)

  def run(): Unit =
    val schemas = List(
      (t"lira",       LiraSchemas.lira,  LiraSchemas.liraSignature),
      (t"lira-tree",  LiraSchemas.tree,  LiraSchemas.treeSignature),
      (t"lira-atoms", LiraSchemas.atoms, LiraSchemas.atomsSignature),
      (t"lira-uses",  LiraSchemas.uses,  LiraSchemas.usesSignature),
      (t"lira-delta", LiraSchemas.delta, LiraSchemas.deltaSignature))

    suite(m"Schema documents"):
      for (name, tels, signature) <- schemas do
        test(m"canonical $name.tel parses"):
          resource(name).read[Tel].childCompounds.readable.length
        . assert(_ > 0)

        test(m"canonical $name.tel type-assigns against the tel-schema axiom"):
          val doc = resource(name).read[Tel]

          try
            Tel.Type.assign(doc, Tels.Axiom.tels)
            t"ok"
          catch case error: TelError => t"failed with ${error.reason}"
        . assert(_ == t"ok")

        test(m"$name.tel reconstructs equal to the hand-encoded value"):
          Tels.Reconstructor.equivalent(Tels.Reconstructor.fromTel(resource(name).read[Tel]), tels)
        . assert(identity)

        test(m"$name schema signature matches its pinned value"):
          Base256.encode(SchemaSignature.fromDocument(resource(name).read[Tel], Tels.Axiom.tels))
        . assert(_ == signature)

    suite(m"Domain-separated hashing"):
      val sample: Data = encode(t"sample content")

      test(m"a domain-separated hash is 32 bytes"):
        LiraHash(LiraHash.Domain.Blob, sample).length
      . assert(_ == 32)

      test(m"distinct domains separate hashes of equal content"):
        val domains = List(LiraHash.Domain.Blob, LiraHash.Domain.Snapshot, LiraHash.Domain.Manifest,
          LiraHash.Domain.Key, LiraHash.Domain.Derivative, LiraHash.Domain.Atom(t"opaque/1"))

        val hashes = domains.map { domain => LiraHash.text(LiraHash(domain, sample)) }.stdlib
        (hashes.toSet.size, hashes.size)
      . assert { sizes => sizes(0) == sizes(1) }

      test(m"distinct disciplines separate atom hashes of equal content"):
        val one = LiraHash.text(LiraHash(LiraHash.Domain.Atom(t"scala-tasty/1"), sample))
        val two = LiraHash.text(LiraHash(LiraHash.Domain.Atom(t"scala-tasty/2"), sample))
        one != two
      . assert(identity)

      test(m"the separator byte disambiguates domain from content"):
        val one = LiraHash.text(LiraHash(LiraHash.Domain.Atom(t"x"), encode(t"yz")))
        val two = LiraHash.text(LiraHash(LiraHash.Domain.Atom(t"xy"), encode(t"z")))
        one != two
      . assert(identity)

      test(m"the empty blob hash matches its pinned value"):
        LiraHash.text(LiraHash(LiraHash.Domain.Blob, Array.freeze(Array[Byte](0))))
      . assert(_ == LiraHash.emptyBlob)

    suite(m"Validators"):
      def valid(method: Text, value: Text): Boolean =
        LiraValidators.registry(Tel.Validator.Request.Scalar(method, value)) match
          case Tel.Validator.Response.Valid => true
          case _                            => false

      val goodHash: Text = LiraHash.text(LiraHash(LiraHash.Domain.Blob, encode(t"x")))

      test(m"a 32-character BASE-256 string is a valid hash"):
        valid(t"base-256-hash", goodHash)
      . assert(identity)

      test(m"a 31-character hash is invalid"):
        valid(t"base-256-hash", goodHash.skip(1))
      . assert(!_)

      test(m"a hash containing a non-alphabet character is invalid"):
        valid(t"base-256-hash", t"±${goodHash.skip(1)}")
      . assert(!_)

      test(m"kebab-case module names are valid"):
        List(t"gossamer-core", t"soundness.dev/gossamer-core", t"a.b/c-d").map: name =>
          valid(t"module-name", name)
      . assert(_ == List(true, true, true))

      for name <- List(t"", t"Gossamer", t"a--b", t"a/", t"-a") do
        test(m"the malformed module name '$name' is invalid"):
          valid(t"module-name", name)
        . assert(!_)

      test(m"package-style namespaces are valid"):
        List(t"gossamer", t"scala.quoted", t"a_b.c1").map { name => valid(t"namespace", name) }
      . assert(_ == List(true, true, true))

      test(m"malformed namespaces are invalid"):
        List(t"", t"9bad", t"a..b", t"a b").map { name => valid(t"namespace", name) }
      . assert(_ == List(false, false, false, false))

      test(m"numeric-only semantic versions are valid"):
        List(t"0.1.0", t"12.0.3").map { version => valid(t"semver", version) }
      . assert(_ == List(true, true))

      test(m"suffixed or partial versions are invalid"):
        List(t"1.2", t"1.2.3-RC1", t"1.2.3+build", t"01.2.3", t"1.2.3.4").map: version =>
          valid(t"semver", version)
      . assert(_ == List(false, false, false, false, false))

      test(m"naturals reject leading zeros"):
        List(t"0", t"42", t"007", t"-1", t"").map { value => valid(t"natural", value) }
      . assert(_ == List(true, true, false, false, false))

      test(m"discipline identifiers require a positive version"):
        List(t"scala-tasty/1", t"opaque/1", t"scala-tasty", t"Scala/1", t"scala-tasty/0").map:
          id => valid(t"discipline-id", id)
      . assert(_ == List(true, true, false, false, false))

      test(m"tree paths must be relative and traversal-free"):
        List(t"a/b.class", t"gossamer/Text.tasty", t"../x", t"a//b", t"/a", t"a/./b").map: path =>
          valid(t"tree-path", path)
      . assert(_ == List(true, true, false, false, false, false))

      test(m"atom classes are rigid or replaceable"):
        List(t"rigid", t"replaceable", t"other").map { value => valid(t"atom-class", value) }
      . assert(_ == List(true, true, false))

    suite(m"Blob stream"):
      def concat(left: Data, right: Data): Data =
        val buffer = Array[Byte](left.length + right.length)
        System.arraycopy(Array.unsafeJvm(left), 0, buffer.raw, 0, left.length)
        System.arraycopy(Array.unsafeJvm(right), 0, buffer.raw, left.length, right.length)
        Array.freeze(buffer)

      def blobHash(data: Data): Text = LiraHash.text(LiraHash(LiraHash.Domain.Blob, data))

      val blobA = encode(t"alpha content")
      val blobB = encode(t"beta")
      val blobC = encode(t"gamma payload bytes")

      test(m"a written stream reads back and resolves every blob"):
        val store = BlobStream.read(BlobStream.write(List(blobA, blobB, blobC)))

        List(blobA, blobB, blobC).map: blob =>
          val hash = LiraHash(LiraHash.Domain.Blob, blob)
          blobHash(store.resolve(hash)) == blobHash(blob)
      . assert(_ == List(true, true, true))

      test(m"any permutation of the same blobs serializes identically"):
        val one = BlobStream.write(List(blobA, blobB, blobC)).serialize[Hex]
        val two = BlobStream.write(List(blobC, blobA, blobB)).serialize[Hex]
        one == two
      . assert(identity)

      test(m"duplicate blobs are stored once"):
        val once = BlobStream.write(List(blobA, blobB)).serialize[Hex]
        val twice = BlobStream.write(List(blobA, blobB, blobA)).serialize[Hex]
        once == twice
      . assert(identity)

      test(m"a two-blob stream matches its pinned serialization"):
        BlobStream.write(List(encode(t"a"), encode(t"bc"))).serialize[Hex]
      . assert(_ == Tests.goldenStream)

      test(m"records out of hash order are rejected"):
        val hashA = LiraHash(LiraHash.Domain.Blob, blobA)
        val hashB = LiraHash(LiraHash.Domain.Blob, blobB)
        val (low, high) = if Blob.compare(hashA, hashB) < 0 then (blobA, blobB) else (blobB, blobA)
        val stream = concat(BlobStream.write(List(high)), BlobStream.write(List(low)))

        capture[LiraError](BlobStream.read(stream)).reason match
          case LiraError.Reason.InvalidBlobStream(_) => true
          case _                                     => false
      . assert(identity)

      test(m"duplicate records in a stream are rejected"):
        val single = BlobStream.write(List(blobA))

        capture[LiraError](BlobStream.read(concat(single, single))).reason match
          case LiraError.Reason.InvalidBlobStream(_) => true
          case _                                     => false
      . assert(identity)

      test(m"a truncated stream is rejected"):
        val stream = BlobStream.write(List(blobA, blobB))
        val short = Array[Byte](stream.length - 1)
        System.arraycopy(Array.unsafeJvm(stream), 0, short.raw, 0, stream.length - 1)

        capture[LiraError](BlobStream.read(Array.freeze(short))).reason match
          case LiraError.Reason.InvalidBlobStream(_) => true
          case _                                     => false
      . assert(identity)

      test(m"resolving an absent hash is an L104 error"):
        val store = BlobStream.read(BlobStream.write(List(blobA)))

        capture[LiraError](store.resolve(LiraHash(LiraHash.Domain.Blob, blobB))).reason match
          case LiraError.Reason.MissingBlob(_) => true
          case _                               => false
      . assert(identity)

      test(m"unreferenced blobs are reported"):
        val store = BlobStream.read(BlobStream.write(List(blobA, blobB)))
        store.unreferenced(Set(blobHash(blobA)))
      . assert(_ == List(blobHash(blobB)))

    suite(m"Compression envelope"):
      val stream = BlobStream.write(List(encode(t"payload blob one"), encode(t"and blob two")))

      test(m"a compressed payload decompresses to the original stream"):
        val compressed = LiraPayload.compress(stream)
        val result = LiraPayload.decompress(compressed, stream.length.toLong, LiraPayload.hash(stream))
        result.serialize[Hex] == stream.serialize[Hex]
      . assert(identity)

      test(m"a payload longer than declared is an L102 error"):
        val compressed = LiraPayload.compress(stream)

        capture[LiraError]
         (LiraPayload.decompress(compressed, stream.length.toLong - 1, LiraPayload.hash(stream)))
        . reason match
            case LiraError.Reason.PayloadLength(_) => true
            case _                                 => false
      . assert(identity)

      test(m"a payload with the wrong declared hash is an L105 error"):
        val compressed = LiraPayload.compress(stream)
        val wrong = LiraHash(LiraHash.Domain.Blob, encode(t"something else"))

        capture[LiraError](LiraPayload.decompress(compressed, stream.length.toLong, wrong)).reason match
          case LiraError.Reason.PayloadHash => true
          case _                            => false
      . assert(identity)

    suite(m"Trees and overlays"):
      def entry(path: Text, content: Text): TreeEntry =
        TreeEntry(TreePath(path), LiraHash(LiraHash.Domain.Blob, encode(content)))

      def same(left: LiraTree, right: LiraTree): Boolean =
        left.encode.serialize[Hex] == right.encode.serialize[Hex]

      val root = LiraTree.of(List(
        entry(t"a/One.class", t"one"),
        entry(t"a/One.tasty", t"one-tasty"),
        entry(t"b/Two.class", t"two")))

      test(m"a traversal path is rejected"):
        capture[LiraError](TreePath(t"../evil")).reason match
          case LiraError.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"tree entries sort identically regardless of input order"):
        val reordered = LiraTree.of(List(
          entry(t"b/Two.class", t"two"),
          entry(t"a/One.tasty", t"one-tasty"),
          entry(t"a/One.class", t"one")))

        same(reordered, root)
      . assert(identity)

      test(m"a duplicate path is rejected"):
        capture[LiraError](LiraTree.of(List(entry(t"a/x", t"1"), entry(t"a/x", t"2")))).reason match
          case LiraError.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"a tree round-trips through its canonical encoding"):
        same(LiraTree.decode(root.encode), root)
      . assert(identity)

      test(m"an empty tree round-trips"):
        same(LiraTree.decode(LiraTree.empty.encode), LiraTree.empty)
      . assert(identity)

      test(m"rows out of ascending path order are rejected on decode"):
        val hash = LiraHash.text(LiraHash(LiraHash.Domain.Blob, encode(t"x")))
        val doc = t"tel 1.0 ${LiraSchemas.treeSignature}\n\nentry b/x  $hash\nentry a/x  $hash\n"

        capture[LiraError](LiraTree.decode(encode(doc))).reason match
          case LiraError.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"a traversal path in a document is rejected on decode"):
        val hash = LiraHash.text(LiraHash(LiraHash.Domain.Blob, encode(t"x")))
        val doc = t"tel 1.0 ${LiraSchemas.treeSignature}\n\nentry ../evil  $hash\n"

        capture[LiraError](LiraTree.decode(encode(doc))).reason match
          case LiraError.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"materialization applies deletions, replacements and additions"):
        val overlay = LiraTree.of(List(
          entry(t"a/One.class", t"one-sjsir"),
          entry(t"c/Three.sjsir", t"three")))

        val delete = List(TreePath(t"b/Two.class"))

        val expected = LiraTree.of(List(
          entry(t"a/One.class", t"one-sjsir"),
          entry(t"a/One.tasty", t"one-tasty"),
          entry(t"c/Three.sjsir", t"three")))

        same(Overlay.materialize(root, delete, overlay), expected)
      . assert(identity)

      test(m"diff produces the minimal overlay that materializes back"):
        val target = LiraTree.of(List(
          entry(t"a/One.class", t"one-sjsir"),
          entry(t"a/One.tasty", t"one-tasty"),
          entry(t"c/Three.sjsir", t"three")))

        val (overlay, delete) = Overlay.diff(root, target)

        val counts = (overlay.entries.stdlib.size, delete.stdlib.size)
        val back = same(Overlay.materialize(root, delete, overlay), target)
        (counts, back)
      . assert(_ == ((2, 1), true))

      test(m"diffing a tree against itself is empty"):
        val (overlay, delete) = Overlay.diff(root, root)
        (overlay.entries.stdlib.size, delete.stdlib.size)
      . assert(_ == (0, 0))

      test(m"a delete of an absent root path is L107"):
        capture[LiraError](Overlay.materialize(root, List(TreePath(t"absent")), LiraTree.empty))
        . reason match
            case LiraError.Reason.OverlayNotMinimal(_) => true
            case _                                     => false
      . assert(identity)

      test(m"an overlay entry identical to the root is L107"):
        val overlay = LiraTree.of(List(entry(t"a/One.class", t"one")))

        capture[LiraError](Overlay.materialize(root, List(), overlay)).reason match
          case LiraError.Reason.OverlayNotMinimal(_) => true
          case _                                     => false
      . assert(identity)

    suite(m"Atoms and snapshots"):
      def item(path: Text, content: Text): (TreePath, Data) = (TreePath(path), encode(content))

      def hex(data: Data): Text = data.serialize[Hex]

      object Special extends Discipline:
        def id: Text = t"special/1"
        def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".special")

        def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
        :   Atomization raises DisciplineError =

          val atoms = content.map: (path, data) =>
            Atom(path.text, AtomClass.Replaceable, LiraHash(LiraHash.Domain.Atom(id), data))

          Atomization.of(id, atoms)

      val context = Discipline.Context(t"jvm")
      val content = List(item(t"a/One.class", t"one"), item(t"b/Two.class", t"two"))

      test(m"opaque atomization is order-insensitive"):
        val one = OpaqueDiscipline.atomize(content, context)
        val two = OpaqueDiscipline.atomize(content.reverse, context)
        hex(AtomsBlob.encode(one)) == hex(AtomsBlob.encode(two))
      . assert(identity)

      test(m"opaque atoms are rigid and keyed by path"):
        val atoms = OpaqueDiscipline.atomize(content, context).atoms

        atoms.map { atom => (atom.key, atom.atomClass) }.stdlib.toSet
        == scala.collection.immutable.Set
            ((t"a/One.class", AtomClass.Rigid), (t"b/Two.class", AtomClass.Rigid))
      . assert(identity)

      test(m"opaque atom hashes are domain-separated from blob hashes"):
        val atom = OpaqueDiscipline.atomize(List(item(t"x", t"content")), context).atoms.stdlib.head
        LiraHash.text(atom.valueHash) != LiraHash.text(LiraHash(LiraHash.Domain.Blob, encode(t"content")))
      . assert(identity)

      test(m"the registry partitions content between disciplines"):
        val mixed = List.from(content.stdlib :+ item(t"c/Three.special", t"three"))
        val results = Discipline.Registry(List(Special)).atomize(mixed, context)

        results.map { atomization => (atomization.discipline, atomization.atoms.stdlib.size) }
        . stdlib
      . assert(_ == scala.List((t"special/1", 1), (t"opaque/1", 2)))

      test(m"an atoms blob round-trips through its canonical encoding"):
        val atomization = OpaqueDiscipline.atomize(content, context)
        hex(AtomsBlob.encode(AtomsBlob.decode(AtomsBlob.encode(atomization))))
        == hex(AtomsBlob.encode(atomization))
      . assert(identity)

      test(m"an empty atoms listing round-trips"):
        val atomization = OpaqueDiscipline.atomize(List(), context)
        AtomsBlob.decode(AtomsBlob.encode(atomization)).atoms.stdlib.size
      . assert(_ == 0)

      test(m"atoms blob rows out of hash order are rejected"):
        val one = LiraHash(LiraHash.Domain.Atom(t"opaque/1"), encode(t"1"))
        val two = LiraHash(LiraHash.Domain.Atom(t"opaque/1"), encode(t"2"))
        val (low, high) = if Blob.compare(one, two) < 0 then (one, two) else (two, one)

        val rowOne = t"atom rigid  ${LiraHash.text(high)}  key-one"
        val rowTwo = t"atom rigid  ${LiraHash.text(low)}  key-two"
        val doc =
          t"tel 1.0 ${LiraSchemas.atomsSignature}\n\ndiscipline opaque/1\n\n$rowOne\n$rowTwo\n"

        capture[LiraError](AtomsBlob.decode(encode(doc))).reason match
          case LiraError.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)

      test(m"a malformed atom class is rejected"):
        val hash = LiraHash.text(LiraHash(LiraHash.Domain.Atom(t"opaque/1"), encode(t"1")))

        val row = t"atom solid  $hash  key-one"
        val doc = t"tel 1.0 ${LiraSchemas.atomsSignature}\n\ndiscipline opaque/1\n\n$row\n"

        capture[LiraError](AtomsBlob.decode(encode(doc))).reason match
          case LiraError.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)

      test(m"duplicate keys within a discipline are rejected"):
        val atom = Atom(t"same", AtomClass.Rigid, LiraHash(LiraHash.Domain.Atom(t"x/1"), encode(t"1")))
        val other = Atom(t"same", AtomClass.Rigid, LiraHash(LiraHash.Domain.Atom(t"x/1"), encode(t"2")))

        capture[DisciplineError](Atomization.of(t"x/1", List(atom, other))).reason match
          case DisciplineError.Reason.Duplicate(_) => true
          case _                                   => false
      . assert(identity)

      test(m"snapshots are permutation-invariant"):
        val one = Snapshot(List(OpaqueDiscipline.atomize(content, context)))
        val two = Snapshot(List(OpaqueDiscipline.atomize(content.reverse, context)))
        LiraHash.text(one) == LiraHash.text(two)
      . assert(identity)

      test(m"snapshots deduplicate value hashes across atomizations"):
        val atomization = OpaqueDiscipline.atomize(content, context)
        val once = Snapshot(List(atomization))
        val twice = Snapshot(List(atomization, atomization))
        LiraHash.text(once) == LiraHash.text(twice)
      . assert(identity)

      test(m"a changed atom value changes the snapshot"):
        val one = Snapshot(List(OpaqueDiscipline.atomize(content, context)))

        val changed = List(item(t"a/One.class", t"one-changed"), item(t"b/Two.class", t"two"))
        val two = Snapshot(List(OpaqueDiscipline.atomize(changed, context)))
        LiraHash.text(one) != LiraHash.text(two)
      . assert(identity)

    suite(m"Grades, lineage and versioning"):
      import revolution.Semver

      def atom(key: Text, atomClass: AtomClass, content: Text): Atom =
        Atom(key, atomClass, LiraHash(LiraHash.Domain.Atom(t"x/1"), encode(content)))

      def release(atoms: Atom*): List[Atomization] =
        List(Atomization.of(t"x/1", List.from(atoms)))

      val base = release(atom(t"a", AtomClass.Rigid, t"1"), atom(t"b", AtomClass.Replaceable, t"2"))

      test(m"an identical atom set grades as a patch"):
        Grade.between(base, base)
      . assert(_ == Grade.Patch)

      test(m"a pure rigid addition grades as minor"):
        val next = release(
          atom(t"a", AtomClass.Rigid, t"1"),
          atom(t"b", AtomClass.Replaceable, t"2"),
          atom(t"c", AtomClass.Rigid, t"3"))

        Grade.between(base, next)
      . assert(_ == Grade.Minor)

      test(m"a replaceable value change with a surviving key grades as minor"):
        val next = release(
          atom(t"a", AtomClass.Rigid, t"1"),
          atom(t"b", AtomClass.Replaceable, t"2-changed"))

        Grade.between(base, next)
      . assert(_ == Grade.Minor)

      test(m"a rigid removal grades as major"):
        Grade.between(base, release(atom(t"b", AtomClass.Replaceable, t"2")))
      . assert(_ == Grade.Major)

      test(m"a rigid value change grades as major"):
        val next = release(
          atom(t"a", AtomClass.Rigid, t"1-changed"),
          atom(t"b", AtomClass.Replaceable, t"2"))

        Grade.between(base, next)
      . assert(_ == Grade.Major)

      test(m"a replaceable removal grades as major"):
        Grade.between(base, release(atom(t"a", AtomClass.Rigid, t"1")))
      . assert(_ == Grade.Major)

      val snapshot = Snapshot(base)
      val older = Snapshot(release(atom(t"a", AtomClass.Rigid, t"1")))

      test(m"a lineage ending in the release's snapshot passes L109"):
        Lineage.check(List(older, snapshot), snapshot)
        true
      . assert(identity)

      test(m"a lineage not ending in the release's snapshot fails L109"):
        capture[LiraError](Lineage.check(List(snapshot, older), snapshot)).reason
      . assert(_ == LiraError.Reason.LineageMismatch)

      test(m"an empty lineage fails L109"):
        capture[LiraError](Lineage.check(List(), snapshot)).reason
      . assert(_ == LiraError.Reason.LineageMismatch)

      test(m"lineage membership decides satisfaction"):
        val absent = Snapshot(release(atom(t"z", AtomClass.Rigid, t"9")))

        (Lineage.contains(List(older, snapshot), older),
         Lineage.contains(List(older, snapshot), absent))
      . assert(_ == (true, false))

      test(m"a minor step appends its snapshot to the lineage"):
        Versioning.extendLineage(List(older), snapshot, Grade.Minor).stdlib
        . map { hash => LiraHash.text(hash) }
      . assert(_ == scala.List(LiraHash.text(older), LiraHash.text(snapshot)))

      test(m"a patch step leaves the lineage unchanged"):
        Versioning.extendLineage(List(older), older, Grade.Patch).stdlib.size
      . assert(_ == 1)

      test(m"a major step without explicit request is refused (L110)"):
        capture[LiraError](Versioning.extendLineage(List(older), snapshot, Grade.Major)).reason
      . assert(_ == LiraError.Reason.UngradedSuccessor)

      test(m"a requested major step begins a fresh lineage"):
        Versioning.extendLineage(List(older), snapshot, Grade.Major, forceMajor = true).stdlib
        . map { hash => LiraHash.text(hash) }
      . assert(_ == scala.List(LiraHash.text(snapshot)))

      test(m"a delta records additions and replacements"):
        val next = release(
          atom(t"a", AtomClass.Rigid, t"1"),
          atom(t"b", AtomClass.Replaceable, t"2-changed"),
          atom(t"c", AtomClass.Rigid, t"3"))

        val delta = LiraDelta.compute(base, next)
        (delta.add.stdlib.size, delta.replace.stdlib.size)
      . assert(_ == (2, 1))

      test(m"a delta round-trips through its canonical encoding"):
        val next = release(
          atom(t"a", AtomClass.Rigid, t"1"),
          atom(t"b", AtomClass.Replaceable, t"2-changed"),
          atom(t"c", AtomClass.Rigid, t"3"))

        val delta = LiraDelta.compute(base, next)
        val back = LiraDelta.decode(delta.encode)
        back.encode.serialize[Hex] == delta.encode.serialize[Hex]
      . assert(identity)

      test(m"an empty delta round-trips"):
        val delta = LiraDelta.compute(base, base)
        val back = LiraDelta.decode(delta.encode)
        (back.add.stdlib.size, back.replace.stdlib.size)
      . assert(_ == (0, 0))

      test(m"the algebra assigns patch, minor and major versions"):
        val version = Semver(1, 2, 3)

        (Versioning.expected(version, Grade.Patch),
         Versioning.expected(version, Grade.Minor),
         Versioning.expected(version, Grade.Major))
      . assert(_ == (Semver(1, 2, 4), Semver(1, 3, 0), Semver(2, 0, 0)))

      test(m"suffixed versions are not numeric"):
        (Versioning.numeric(Semver(1, 2, 3)),
         Versioning.numeric(Semver(1, 2, 3, prerelease = List(t"RC1"))))
      . assert(_ == (true, false))

      test(m"a version matching the projection raises no advisory"):
        Versioning.advisories(Semver(1, 3, 0), Semver(1, 2, 3), Grade.Minor).stdlib.size
      . assert(_ == 0)

      test(m"a version defying the projection raises an advisory"):
        Versioning.advisories(Semver(1, 2, 4), Semver(1, 2, 3), Grade.Minor).stdlib
      . assert(_ == scala.List(LiraAdvisory.VersionMismatch(Semver(1, 2, 4), Semver(1, 3, 0))))

      test(m"a suffixed version raises a not-numeric advisory"):
        val suffixed = Semver(1, 2, 3, prerelease = List(t"RC1"))
        Versioning.advisories(suffixed, Unset, Grade.Patch).stdlib
      . assert(_ == scala.List(LiraAdvisory.NotNumeric(Semver(1, 2, 3, prerelease = List(t"RC1")))))
