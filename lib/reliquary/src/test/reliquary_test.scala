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

  // The pinned `lira/1:derivative` hash of the two-entry canonical jar built by the derivative
  // suite: a golden value locking the Stored zip profile byte-for-byte.
  val goldenDerivative: Text = t"ωӟMſÔǽƒJMôҷЖǣΞYǣЭOЫǿ3ωḡm3ќḞήUШďå"

  def encode(text: Text): Data = charEncoders.utf8Encoder.encoded(text)

  def resource(name: Text): Data =
    val stream = getClass.getResourceAsStream(s"/reliquary/${name}.tel").nn
    val bytes = stream.readAllBytes().nn
    stream.close()
    Array.unsafeFrozen(bytes)

  def run(): Unit =
    val classA = encode(t"class A bytecode")
    val tastyA = encode(t"class A tasty")
    val sjsirA = encode(t"class A sjsir")

    def blob(data: Data): Data = LiraHash(LiraHash.Domain.Blob, data)

    def makeLira(): Data =
      val context = Discipline.Context(t"jvm")
      val registry = Discipline.Registry(List())

      val rootTree = LiraTree.of(List(
        TreeEntry(TreePath(t"a/A.class"), blob(classA)),
        TreeEntry(TreePath(t"a/A.tasty"), blob(tastyA))))

      val overlayTree = LiraTree.of(List(TreeEntry(TreePath(t"a/A.sjsir"), blob(sjsirA))))

      val atomizations = registry.atomize(
        List((TreePath(t"a/A.class"), classA), (TreePath(t"a/A.tasty"), tastyA)), context)

      val atomsData = AtomsBlob.encode(atomizations.stdlib.head)
      val snapshot = Snapshot(atomizations)

      val manifest = LiraManifest(
        module    = t"example-core",
        version   = revolution.Semver(0, 1, 0),
        lineage   = List(snapshot),
        toolchain = List(LiraManifest.Tool(t"scala", t"3.9.0")),
        owns      = List(t"example"),
        api       = List(LiraManifest.Api(t"opaque/1", blob(atomsData))),
        section   = List(
          Section(t"jvm", tree = blob(rootTree.encode)),
          Section(t"sjsir", tree = blob(overlayTree.encode),
            delete = List(TreePath(t"a/A.class")))),
        payload   = LiraManifest.Payload(t"brotli", 0L, blob(encode(t""))))

      Lira.assemble(manifest,
        List(classA, tastyA, sjsirA, rootTree.encode, overlayTree.encode, atomsData))


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

      test(m"a deleted-and-re-added path is L107"):
        val overlay = LiraTree.of(List(entry(t"a/One.class", t"one-other")))
        val delete = List(TreePath(t"a/One.class"))

        capture[LiraError](Overlay.materialize(root, delete, overlay)).reason match
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

    suite(m"Container and verification"):
      test(m"an assembled lira reads back and verifies"):
        val report = Verification.install(Lira.read(makeLira()))
        report.materialized.stdlib.map { pair => pair(0) }
      . assert(_ == scala.List(t"jvm", t"sjsir"))

      test(m"assembly is byte-deterministic"):
        makeLira().serialize[Hex] == makeLira().serialize[Hex]
      . assert(identity)

      test(m"the manifest round-trips through its rendering"):
        val lira = Lira.read(makeLira())
        val rendered = encode(lira.manifest.render)
        val tail = encode(t"##\n")
        val buffer = Array[Byte](rendered.length + tail.length + lira.compressed.length)
        System.arraycopy(Array.unsafeJvm(rendered), 0, buffer.raw, 0, rendered.length)
        System.arraycopy(Array.unsafeJvm(tail), 0, buffer.raw, rendered.length, tail.length)

        System.arraycopy(Array.unsafeJvm(lira.compressed), 0, buffer.raw,
          rendered.length + tail.length, lira.compressed.length)

        Lira.read(Array.freeze(buffer)).manifest.render == lira.manifest.render
      . assert(identity)

      test(m"the sjsir overlay materializes without the deleted classfile"):
        val report = Verification.install(Lira.read(makeLira()))
        val sjsir = report.materialized.stdlib.find { pair => pair(0) == t"sjsir" }

        sjsir.map { pair => pair(1).entries.map(_.path.text).stdlib }
      . assert(_ == scala.Some(scala.List(t"a/A.sjsir", t"a/A.tasty")))

      test(m"a corrupted directive is L115"):
        val data = makeLira().mutable(using Unsafe)
        data(0) = '?'.toByte

        capture[LiraError](Lira.read(Array.unsafeFrozen(data))).reason
      . assert(_ == LiraError.Reason.BadDirective)

      test(m"a manifest with a sigil in its pragma is L116"):
        val body = t"#!/usr/bin/env lira\ntel 1.0 ${LiraSchemas.liraSignature} !\n\nmodule x\n##\n"

        capture[LiraError](Lira.read(encode(body))).reason match
          case LiraError.Reason.SigilSpecified      => true
          case LiraError.Reason.InvalidManifest(_)  => false
          case _                                    => false
      . assert(identity)

      test(m"a missing separator is rejected"):
        // truncate the file to just the directive line, which contains no separator
        val data = makeLira()
        val short = Array[Byte](20)
        System.arraycopy(Array.unsafeJvm(data), 0, short.raw, 0, 20)

        capture[LiraError](Lira.read(Array.freeze(short))).reason match
          case LiraError.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)

      test(m"a wrong declared payload hash is caught at verification"):
        val lira = Lira.read(makeLira())
        val wrong = lira.manifest.payload.copy(hash = blob(encode(t"wrong")))
        val tampered = lira.copy(manifest = lira.manifest.copy(payload = wrong))

        capture[LiraError](Verification.install(tampered)).reason
      . assert(_ == LiraError.Reason.PayloadHash)

      test(m"a wrong declared payload length is caught at verification"):
        val lira = Lira.read(makeLira())
        val payload = lira.manifest.payload
        val wrong = payload.copy(length = payload.length + 1)
        val tampered = lira.copy(manifest = lira.manifest.copy(payload = wrong))

        capture[LiraError](Verification.install(tampered)).reason match
          case LiraError.Reason.PayloadLength(_) => true
          case _                                 => false
      . assert(identity)

      test(m"a dangling atoms reference is L104"):
        val lira = Lira.read(makeLira())
        val wrong = List(LiraManifest.Api(t"opaque/1", blob(encode(t"absent"))))
        val tampered = lira.copy(manifest = lira.manifest.copy(api = wrong))

        capture[LiraError](Verification.install(tampered)).reason match
          case LiraError.Reason.MissingBlob(_) => true
          case _                               => false
      . assert(identity)

      test(m"a lineage not ending in the snapshot is L109"):
        val lira = Lira.read(makeLira())
        val tampered = lira.copy(manifest = lira.manifest.copy(lineage = List(blob(encode(t"x")))))

        capture[LiraError](Verification.install(tampered)).reason
      . assert(_ == LiraError.Reason.LineageMismatch)

      test(m"a corrupted compressed payload is rejected"):
        val data = makeLira().mutable(using Unsafe)
        data(data.length - 1) = (data(data.length - 1) ^ 0x55).toByte

        capture[LiraError](Verification.install(Lira.read(Array.unsafeFrozen(data)))).reason match
          case LiraError.Reason.InvalidBlobStream(_) => true
          case LiraError.Reason.PayloadHash          => true
          case LiraError.Reason.PayloadLength(_)     => true
          case _                                     => false
      . assert(identity)

    suite(m"Manifest signing"):
      import enigmatic.{MlDsa, Signing}
      import gastronomy.providers.javaStdlibProvider

      val mlDsa65: MlDsa[65] = summon[MlDsa[65]]
      val privateKey = mlDsa65.genKey()
      val publicKey = mlDsa65.privateToPublic(privateKey)
      val otherPrivate = mlDsa65.genKey()
      val otherPublic = mlDsa65.privateToPublic(otherPrivate)

      def schemes(algorithm: Text): Optional[Signing] =
        if algorithm == t"ml-dsa-65" then mlDsa65 else Unset

      def signed(): LiraManifest =
        val manifest = Lira.read(makeLira()).manifest

        ManifestSigning.sign
          (manifest, t"jon.pretty@propensive.com", t"ml-dsa-65", mlDsa65, privateKey, publicKey)

      test(m"a signed manifest verifies against the signer's key"):
        ManifestSigning.verify(signed(), ManifestSigning.Keyring(List(publicKey)), schemes)
        true
      . assert(identity)

      test(m"the signing input is unchanged by signing"):
        val manifest = Lira.read(makeLira()).manifest
        val one = LiraHash.text(ManifestSigning.input(manifest))
        val two = LiraHash.text(ManifestSigning.input(signed()))
        one == two
      . assert(identity)

      test(m"a counter-signed manifest verifies both signatures"):
        val twice = ManifestSigning.sign
          (signed(), t"co@example.com", t"ml-dsa-65", mlDsa65, otherPrivate, otherPublic)

        ManifestSigning.verify
          (twice, ManifestSigning.Keyring(List(publicKey, otherPublic)), schemes)

        twice.signature.stdlib.size
      . assert(_ == 2)

      test(m"a tampered manifest fails signature verification"):
        val tampered = signed().copy(module = t"impostor-core")
        val keyring = ManifestSigning.Keyring(List(publicKey))

        capture[LiraError](ManifestSigning.verify(tampered, keyring, schemes)).reason match
          case LiraError.Reason.BadSignature(_) => true
          case _                                => false
      . assert(identity)

      test(m"an unknown algorithm is rejected, never ignored"):
        val record = signed().signature.stdlib.head.copy(algorithm = t"quantum-magic")
        val manifest = signed().copy(signature = List(record))
        val keyring = ManifestSigning.Keyring(List(publicKey))

        capture[LiraError](ManifestSigning.verify(manifest, keyring, schemes)).reason match
          case LiraError.Reason.UnknownAlgorithm(_) => true
          case _                                    => false
      . assert(identity)

      test(m"an unknown key fingerprint is rejected"):
        val keyring = ManifestSigning.Keyring(List(otherPublic))

        capture[LiraError](ManifestSigning.verify(signed(), keyring, schemes)).reason match
          case LiraError.Reason.UnknownKey(_) => true
          case _                              => false
      . assert(identity)

      test(m"a signed lira survives assembly, reading and verification"):
        val lira = Lira.read(makeLira())
        val stream = LiraPayload.decompress
          (lira.compressed, lira.manifest.payload.length, lira.manifest.payload.hash)

        val store = BlobStream.read(stream)
        val blobs = store.blobs.map(_.data)

        val resigned = ManifestSigning.sign
          (lira.manifest, t"jon.pretty@propensive.com", t"ml-dsa-65", mlDsa65, privateKey,
           publicKey)

        val bytes = Lira.assemble(resigned, blobs)
        val back = Lira.read(bytes)
        ManifestSigning.verify(back.manifest, ManifestSigning.Keyring(List(publicKey)), schemes)
        Verification.install(back)
        back.manifest.signature.stdlib.size
      . assert(_ == 1)

    suite(m"Buildpath and publication"):
      import revolution.Semver

      def payloadStub(seed: Text): LiraManifest.Payload =
        LiraManifest.Payload(t"brotli", 1L, blob(encode(seed)))

      def stub
        ( module:  Text,
          lineage: List[Data],
          owns:    List[Text]                    = List(),
          deps:    List[LiraManifest.Dependency] = List(),
          version: Optional[Semver]              = Unset,
          section: List[Section]                 = List() )
      :   LiraManifest =

        LiraManifest
          ( module     = module,
            version    = version,
            lineage    = lineage,
            owns       = owns,
            api        = List(),
            dependency = deps,
            section    = section,
            payload    = payloadStub(module) )

      val snapOne = LiraHash(LiraHash.Domain.Snapshot, encode(t"one"))
      val snapTwo = LiraHash(LiraHash.Domain.Snapshot, encode(t"two"))

      test(m"two releases of one module are L111"):
        val path = Buildpath(List(stub(t"alpha", List(snapOne)), stub(t"alpha", List(snapTwo))))

        capture[LiraError](path.validate(t"jvm")).reason match
          case LiraError.Reason.DuplicateModule(_) => true
          case _                                   => false
      . assert(identity)

      test(m"nested namespace claims are L112"):
        val path = Buildpath(List(
          stub(t"alpha", List(snapOne), owns = List(t"gossamer")),
          stub(t"beta", List(snapTwo), owns = List(t"gossamer.text"))))

        capture[LiraError](path.validate(t"jvm")).reason match
          case LiraError.Reason.NamespaceClash(_) => true
          case _                                  => false
      . assert(identity)

      test(m"disjoint namespace claims pass"):
        val path = Buildpath(List(
          stub(t"alpha", List(snapOne), owns = List(t"gossamer")),
          stub(t"beta", List(snapTwo), owns = List(t"gossamers"))))

        path.validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"an absent dependency is L113"):
        val needy = stub(t"alpha", List(snapOne),
          deps = List(LiraManifest.Dependency(t"missing", snapTwo)))

        capture[LiraError](Buildpath(List(needy)).validate(t"jvm")).reason match
          case LiraError.Reason.AbsentDependency(_) => true
          case _                                    => false
      . assert(identity)

      test(m"a universe-scoped dependency binds only its universes"):
        val needy = stub(t"alpha", List(snapOne),
          deps = List(LiraManifest.Dependency(t"missing", snapTwo, universe = List(t"nir"))))

        val path = Buildpath(List(needy))
        val jvm = path.validate(t"jvm").stdlib.size

        val nir = capture[LiraError](path.validate(t"nir")).reason match
          case LiraError.Reason.AbsentDependency(_) => true
          case _                                    => false

        (jvm, nir)
      . assert(_ == (0, true))

      test(m"lineage membership satisfies a requirement"):
        val provider = stub(t"beta", List(snapOne, snapTwo))
        val needy = stub(t"alpha", List(snapOne),
          deps = List(LiraManifest.Dependency(t"beta", snapOne)))

        Buildpath(List(needy, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"a requirement outside the lineage is L114"):
        val provider = stub(t"beta", List(snapTwo))
        val needy = stub(t"alpha", List(snapOne),
          deps = List(LiraManifest.Dependency(t"beta", snapOne)))

        capture[LiraError](Buildpath(List(needy, provider)).validate(t"jvm")).reason match
          case LiraError.Reason.Unsatisfiable(_) => true
          case _                                 => false
      . assert(identity)

      test(m"a diamond resolves iff one lineage contains both snapshots"):
        val provider = stub(t"omega", List(snapOne, snapTwo))
        val left = stub(t"alpha", List(blob(encode(t"al"))),
          deps = List(LiraManifest.Dependency(t"omega", snapOne)))

        val right = stub(t"beta", List(blob(encode(t"be"))),
          deps = List(LiraManifest.Dependency(t"omega", snapTwo)))

        Buildpath(List(left, right, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"a build pin must match the implementation identity"):
        val provider = stub(t"beta", List(snapOne))
        val pinned = stub(t"alpha", List(snapTwo), deps = List(
          LiraManifest.Dependency(t"beta", snapOne, build = blob(encode(t"other")))))

        capture[LiraError](Buildpath(List(pinned, provider)).validate(t"jvm")).reason match
          case LiraError.Reason.Unsatisfiable(_) => true
          case _                                 => false
      . assert(identity)

      test(m"a matching build pin passes"):
        val provider = stub(t"beta", List(snapOne))
        val pinned = stub(t"alpha", List(snapTwo), deps = List(
          LiraManifest.Dependency(t"beta", snapOne, build = provider.payload.hash)))

        Buildpath(List(pinned, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"a version hint disagreement is advisory only"):
        val provider = stub(t"beta", List(snapOne), version = Semver(2, 0, 0))
        val needy = stub(t"alpha", List(snapTwo), deps = List(
          LiraManifest.Dependency(t"beta", snapOne, version = Semver(1, 0, 0))))

        Buildpath(List(needy, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 1)

      test(m"a derivative hash resolves to its declaring release"):
        val derivative = blob(encode(t"the canonical jar"))
        val holder = stub(t"alpha", List(snapOne), section = List(
          Section(t"jvm", tree = blob(encode(t"tree")), derivative = derivative)))

        val path = Buildpath(List(holder, stub(t"beta", List(snapTwo))))
        path.byDerivative(derivative).let(_.module).or(t"absent")
      . assert(_ == t"alpha")

      test(m"a development release is unpublishable (L117)"):
        capture[LiraError](Buildpath.publishable(stub(t"alpha", List(snapOne)), List())).reason
      . assert(_ == LiraError.Reason.VersionRequired)

      test(m"a build pin is unpublishable (L118)"):
        val pinned = stub(t"alpha", List(snapOne), version = Semver(0, 0, 0), deps = List(
          LiraManifest.Dependency(t"beta", snapTwo, build = blob(encode(t"pin")))))

        capture[LiraError](Buildpath.publishable(pinned, List())).reason match
          case LiraError.Reason.BuildPinned(_) => true
          case _                               => false
      . assert(identity)

      test(m"an unpublished dependency is unpublishable (L119)"):
        val needy = stub(t"alpha", List(snapOne), version = Semver(0, 0, 0),
          deps = List(LiraManifest.Dependency(t"beta", snapTwo)))

        capture[LiraError](Buildpath.publishable(needy, List())).reason match
          case LiraError.Reason.UnpublishedDependency(_) => true
          case _                                         => false
      . assert(identity)

      test(m"a minor number defying the lineage is unpublishable (L120)"):
        val wrong = stub(t"alpha", List(snapOne, snapTwo), version = Semver(1, 3, 0))

        capture[LiraError](Buildpath.publishable(wrong, List())).reason match
          case LiraError.Reason.VersionProjection(_) => true
          case _                                     => false
      . assert(identity)

      def makeRelease(api: scala.List[(Text, Text)], extra: scala.List[(Text, Text)]): Data =
        val context = Discipline.Context(t"jvm")
        val registry = Discipline.Registry(List())
        val apiItems = api.map { pair => (TreePath(pair(0)), encode(pair(1))) }
        val extraItems = extra.map { pair => (TreePath(pair(0)), encode(pair(1))) }
        val atomizations = registry.atomize(List.from(apiItems), context)
        val atomsData = AtomsBlob.encode(atomizations.stdlib.head)
        val snapshot = Snapshot(atomizations)

        val entries = (apiItems ++ extraItems).map: pair =>
          TreeEntry(pair(0), blob(pair(1)))

        val tree = LiraTree.of(List.from(entries))

        val manifest = LiraManifest
          ( module    = t"assignee",
            lineage   = List(snapshot),
            toolchain = List(LiraManifest.Tool(t"scala", t"3.9.0")),
            api       = List(LiraManifest.Api(t"opaque/1", blob(atomsData))),
            section   = List(Section(t"jvm", tree = blob(tree.encode))),
            payload   = payloadStub(t"replaced") )

        val blobs = (apiItems ++ extraItems).map { pair => pair(1) }
        Lira.assemble(manifest, List.from(blobs :+ tree.encode :+ atomsData))

      val versionOne = scala.List((t"a/A.class", t"alpha one"))

      def published(): Lira =
        val dev = Lira.read(makeRelease(versionOne, scala.Nil))
        val assigned = Publication.assign(dev, Unset, List())
        val stream = LiraPayload.decompress
          (dev.compressed, dev.manifest.payload.length, dev.manifest.payload.hash)

        Lira.read(Lira.assemble(assigned, BlobStream.read(stream).blobs.map(_.data)))

      test(m"a first release is assigned 0.1.0 with a fresh lineage"):
        val manifest = Publication.assign(Lira.read(makeRelease(versionOne, scala.Nil)), Unset,
          List())

        (manifest.version.let(_.minor).or(-1L), manifest.lineage.stdlib.size)
      . assert(_ == (1L, 1))

      test(m"a rigid addition is assigned the next minor version"):
        val base = published()
        val dev = Lira.read
          (makeRelease(versionOne :+ (t"a/B.class", t"beta"), scala.Nil))

        val manifest = Publication.assign(dev, base, List(base.manifest))
        (manifest.version, manifest.lineage.stdlib.size)
      . assert(_ == (Semver(0, 2, 0), 2))

      test(m"an implementation-only change is assigned the next patch version"):
        val base = published()
        val dev = Lira.read(makeRelease(versionOne, scala.List((t"doc/readme.md", t"docs"))))
        val manifest = Publication.assign(dev, base, List(base.manifest))
        (manifest.version, manifest.lineage.stdlib.size)
      . assert(_ == (Semver(0, 1, 1), 1))

      test(m"a rigid removal is refused without an explicit major"):
        val base = published()
        val dev = Lira.read(makeRelease(scala.List((t"a/C.class", t"gamma")), scala.Nil))

        capture[LiraError](Publication.assign(dev, base, List(base.manifest))).reason
      . assert(_ == LiraError.Reason.UngradedSuccessor)

      test(m"an explicit major begins a fresh lineage"):
        val base = published()
        val dev = Lira.read(makeRelease(scala.List((t"a/C.class", t"gamma")), scala.Nil))
        val manifest = Publication.assign(dev, base, List(base.manifest), forceMajor = true)
        (manifest.version, manifest.lineage.stdlib.size)
      . assert(_ == (Semver(0, 2, 0), 1))

      test(m"a used-set closes over replaceable references"):
        val rigid = Atom(t"target", AtomClass.Rigid,
          LiraHash(LiraHash.Domain.Atom(t"x/1"), encode(t"rigid")))

        val inline = Atom(t"caller[inline]", AtomClass.Replaceable,
          LiraHash(LiraHash.Domain.Atom(t"x/1"), encode(t"body")),
          references = List(AtomReference.Own(t"target")))

        val dependency = Atomization.of(t"x/1", List(rigid, inline))
        val closure = UsesBlob.closure(List(inline.valueHash), List((t"dep", dependency)))

        val expected = scala.collection.immutable.Set
          (LiraHash.text(inline.valueHash), LiraHash.text(rigid.valueHash))

        closure.stdlib.map { hash => LiraHash.text(hash) }.toSet == expected
      . assert(identity)

      test(m"a uses blob round-trips"):
        val atoms = List(blob(encode(t"u1")), blob(encode(t"u2")))
        val (module, back) = UsesBlob.decode(UsesBlob.encode(t"dep", atoms))
        (module, back.stdlib.size)
      . assert(_ == (t"dep", 2))

      test(m"spanning holds iff the candidate carries every used atom"):
        val one = Atom(t"a", AtomClass.Rigid, LiraHash(LiraHash.Domain.Atom(t"x/1"), encode(t"1")))
        val two = Atom(t"b", AtomClass.Rigid, LiraHash(LiraHash.Domain.Atom(t"x/1"), encode(t"2")))

        (UsesBlob.spanning(List(one.valueHash), List(one, two)),
         UsesBlob.spanning(List(one.valueHash, two.valueHash), List(one)))
      . assert(_ == (true, false))

      test(m"staleness detects replaced atoms in the used-set"):
        val old = blob(encode(t"old"))
        val neo = blob(encode(t"new"))

        (UsesBlob.staleness(List(old), List(Replacement(old, neo))),
         UsesBlob.staleness(List(neo), List(Replacement(old, neo))))
      . assert(_ == (true, false))

    suite(m"Derivative artifacts"):
      import distillate.*
      import galilei.*
      import prepositional.*
      import serpentine.*

      def store(datas: List[Data]): Blobstore = BlobStream.read(BlobStream.write(datas))

      val tree = LiraTree.of(List(
        TreeEntry(TreePath(t"a/A.class"), blob(classA)),
        TreeEntry(TreePath(t"a/A.tasty"), blob(tastyA))))

      test(m"derivation is byte-deterministic"):
        val blobstore = store(List(classA, tastyA))
        val one = Derivative.jar(tree, blobstore).serialize[Hex]
        val two = Derivative.jar(tree, blobstore).serialize[Hex]
        one == two
      . assert(identity)

      test(m"the derivative hash matches its pinned value"):
        LiraHash.text(Derivative.hash(tree, store(List(classA, tastyA))))
      . assert(_ == Tests.goldenDerivative)

      test(m"the derivative jar is readable by java.util.zip"):
        val data = Derivative.jar(tree, store(List(classA, tastyA)))

        val input = java.util.zip.ZipInputStream
          (java.io.ByteArrayInputStream(Array.unsafeJvm(data)))

        val names = scala.collection.mutable.ArrayBuffer[String]()
        var entry = input.getNextEntry()

        while entry != null do
          names += entry.nn.getName().nn
          entry = input.getNextEntry()

        input.close()
        names.toList
      . assert(_ == scala.List("a/A.class", "a/A.tasty"))

      test(m"materialization builds a classpath of cached derivative jars"):
        val cache = unsafely:
          t"/tmp/reliquary-test-${java.lang.System.nanoTime}".as[Path on Linux]

        val lira = Lira.read(makeLira())
        val first = Materializer.classpath(List(lira), t"jvm", cache)
        val second = Materializer.classpath(List(lira), t"jvm", cache)

        (first.entries.stdlib.size, first.entries.stdlib == second.entries.stdlib)
      . assert(_ == (1, true))

      test(m"materialization refuses a universe with no section"):
        val cache = unsafely:
          t"/tmp/reliquary-test-${java.lang.System.nanoTime}".as[Path on Linux]

        val lira = Lira.read(makeLira())

        capture[LiraError](Materializer.classpath(List(lira), t"nir", cache)).reason match
          case LiraError.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)
