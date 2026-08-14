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

    def blob(data: Data): Data = Lira.Hash(Lira.Hash.Domain.Blob, data)

    def makeLira(): Data =
      val context = Discipline.Context(t"jvm")
      val registry = Discipline.Registry(List())

      val rootTree = Lira.Tree.of(List(
        TreeEntry(TreePath(t"a/A.class"), blob(classA)),
        TreeEntry(TreePath(t"a/A.tasty"), blob(tastyA))))

      val overlayTree = Lira.Tree.of(List(TreeEntry(TreePath(t"a/A.sjsir"), blob(sjsirA))))

      val atomizations = registry.atomize(
        List((TreePath(t"a/A.class"), classA), (TreePath(t"a/A.tasty"), tastyA)), context)

      val atomsData = AtomsBlob.encode(atomizations.stdlib.head)
      val snapshot = Snapshot(atomizations)

      val manifest = Lira.Manifest(
        module    = t"example-core",
        version   = revolution.Semver(0, 1, 0),
        lineage   = List(snapshot),
        toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
        owns      = List(t"example"),
        api       = List(Lira.Manifest.Api(t"opaque/1", blob(atomsData))),
        section   = List(
          Section(t"jvm", tree = blob(rootTree.encode)),
          Section(t"sjsir", tree = blob(overlayTree.encode),
            delete = List(TreePath(t"a/A.class")))),
        payload   = Lira.Manifest.Payload(t"brotli", 0L, blob(encode(t""))))

      Lira.assemble(manifest,
        List(classA, tastyA, sjsirA, rootTree.encode, overlayTree.encode, atomsData))


    val schemas = List(
      (t"lira",              Lira.Schemas.lira,         Lira.Schemas.liraSignature),
      (t"lira-tree",         Lira.Schemas.tree,         Lira.Schemas.treeSignature),
      (t"lira-atoms",        Lira.Schemas.atoms,        Lira.Schemas.atomsSignature),
      (t"lira-uses",         Lira.Schemas.uses,         Lira.Schemas.usesSignature),
      (t"lira-delta",        Lira.Schemas.delta,        Lira.Schemas.deltaSignature),
      (t"lira-capabilities", Lira.Schemas.capabilities, Lira.Schemas.capabilitiesSignature))

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
          catch case error: Tel.Error => t"failed with ${error.reason}"
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
        Lira.Hash(Lira.Hash.Domain.Blob, sample).length
      . assert(_ == 32)

      test(m"distinct domains separate hashes of equal content"):
        val domains = List(Lira.Hash.Domain.Blob, Lira.Hash.Domain.Snapshot, Lira.Hash.Domain.Manifest,
          Lira.Hash.Domain.Key, Lira.Hash.Domain.Derivative, Lira.Hash.Domain.Atom(t"opaque/1"))

        val hashes = domains.map { domain => Lira.Hash.text(Lira.Hash(domain, sample)) }.stdlib
        (hashes.toSet.size, hashes.size)
      . assert { sizes => sizes(0) == sizes(1) }

      test(m"distinct disciplines separate atom hashes of equal content"):
        val one = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Atom(t"scala-tasty/1"), sample))
        val two = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Atom(t"scala-tasty/2"), sample))
        one != two
      . assert(identity)

      test(m"the separator byte disambiguates domain from content"):
        val one = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Atom(t"x"), encode(t"yz")))
        val two = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Atom(t"xy"), encode(t"z")))
        one != two
      . assert(identity)

      test(m"the empty blob hash matches its pinned value"):
        Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Blob, Array.freeze(Array[Byte](0))))
      . assert(_ == Lira.Hash.emptyBlob)

    suite(m"Validators"):
      def valid(method: Text, value: Text): Boolean =
        Lira.Validators.registry(Tel.Validator.Request.Scalar(method, value)) match
          case Tel.Validator.Response.Valid => true
          case _                            => false

      val goodHash: Text = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Blob, encode(t"x")))

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

      def blobHash(data: Data): Text = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Blob, data))

      val blobA = encode(t"alpha content")
      val blobB = encode(t"beta")
      val blobC = encode(t"gamma payload bytes")

      test(m"a written stream reads back and resolves every blob"):
        val store = BlobStream.read(BlobStream.write(List(blobA, blobB, blobC)))

        List(blobA, blobB, blobC).map: blob =>
          val hash = Lira.Hash(Lira.Hash.Domain.Blob, blob)
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
        val hashA = Lira.Hash(Lira.Hash.Domain.Blob, blobA)
        val hashB = Lira.Hash(Lira.Hash.Domain.Blob, blobB)
        val (low, high) = if Blob.compare(hashA, hashB) < 0 then (blobA, blobB) else (blobB, blobA)
        val stream = concat(BlobStream.write(List(high)), BlobStream.write(List(low)))

        capture[Lira.Error](BlobStream.read(stream)).reason match
          case Lira.Error.Reason.InvalidBlobStream(_) => true
          case _                                     => false
      . assert(identity)

      test(m"duplicate records in a stream are rejected"):
        val single = BlobStream.write(List(blobA))

        capture[Lira.Error](BlobStream.read(concat(single, single))).reason match
          case Lira.Error.Reason.InvalidBlobStream(_) => true
          case _                                     => false
      . assert(identity)

      test(m"a truncated stream is a malformed payload, not an L103 violation"):
        val stream = BlobStream.write(List(blobA, blobB))
        val short = Array[Byte](stream.length - 1)
        System.arraycopy(Array.unsafeJvm(stream), 0, short.raw, 0, stream.length - 1)

        capture[Lira.Error](BlobStream.read(Array.freeze(short))).reason match
          case Lira.Error.Reason.MalformedPayload(_) => true
          case _                                    => false
      . assert(identity)

      test(m"resolving an absent hash is an L104 error"):
        val store = BlobStream.read(BlobStream.write(List(blobA)))

        capture[Lira.Error](store.resolve(Lira.Hash(Lira.Hash.Domain.Blob, blobB))).reason match
          case Lira.Error.Reason.MissingBlob(_) => true
          case _                               => false
      . assert(identity)

      test(m"unreferenced blobs are reported"):
        val store = BlobStream.read(BlobStream.write(List(blobA, blobB)))
        store.unreferenced(Set(blobHash(blobA)))
      . assert(_ == List(blobHash(blobB)))

    suite(m"Compression envelope"):
      val stream = BlobStream.write(List(encode(t"payload blob one"), encode(t"and blob two")))

      test(m"a compressed payload decompresses to the original stream"):
        val compressed = Lira.Payload.compress(stream)
        val result = Lira.Payload.decompress(compressed, stream.length.toLong, Lira.Payload.hash(stream))
        result.serialize[Hex] == stream.serialize[Hex]
      . assert(identity)

      test(m"a payload longer than declared is an L102 error"):
        val compressed = Lira.Payload.compress(stream)

        capture[Lira.Error]
         (Lira.Payload.decompress(compressed, stream.length.toLong - 1, Lira.Payload.hash(stream)))
        . reason match
            case Lira.Error.Reason.PayloadLength(_) => true
            case _                                 => false
      . assert(identity)

      test(m"a payload with the wrong declared hash is an L105 error"):
        val compressed = Lira.Payload.compress(stream)
        val wrong = Lira.Hash(Lira.Hash.Domain.Blob, encode(t"something else"))

        capture[Lira.Error](Lira.Payload.decompress(compressed, stream.length.toLong, wrong)).reason match
          case Lira.Error.Reason.PayloadHash => true
          case _                            => false
      . assert(identity)

    suite(m"Trees and overlays"):
      def entry(path: Text, content: Text): TreeEntry =
        TreeEntry(TreePath(path), Lira.Hash(Lira.Hash.Domain.Blob, encode(content)))

      def same(left: Lira.Tree, right: Lira.Tree): Boolean =
        left.encode.serialize[Hex] == right.encode.serialize[Hex]

      val root = Lira.Tree.of(List(
        entry(t"a/One.class", t"one"),
        entry(t"a/One.tasty", t"one-tasty"),
        entry(t"b/Two.class", t"two")))

      test(m"a traversal path is rejected"):
        capture[Lira.Error](TreePath(t"../evil")).reason match
          case Lira.Error.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"tree entries sort identically regardless of input order"):
        val reordered = Lira.Tree.of(List(
          entry(t"b/Two.class", t"two"),
          entry(t"a/One.tasty", t"one-tasty"),
          entry(t"a/One.class", t"one")))

        same(reordered, root)
      . assert(identity)

      test(m"a duplicate path is rejected"):
        capture[Lira.Error](Lira.Tree.of(List(entry(t"a/x", t"1"), entry(t"a/x", t"2")))).reason match
          case Lira.Error.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"a tree round-trips through its canonical encoding"):
        same(Lira.Tree.decode(root.encode), root)
      . assert(identity)

      test(m"an empty tree round-trips"):
        same(Lira.Tree.decode(Lira.Tree.empty.encode), Lira.Tree.empty)
      . assert(identity)

      test(m"rows out of ascending path order are rejected on decode"):
        val hash = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Blob, encode(t"x")))
        val doc = t"tel 1.0 ${Lira.Schemas.treeSignature}\n\nentry b/x  $hash\nentry a/x  $hash\n"

        capture[Lira.Error](Lira.Tree.decode(encode(doc))).reason match
          case Lira.Error.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"a traversal path in a document is rejected on decode"):
        val hash = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Blob, encode(t"x")))
        val doc = t"tel 1.0 ${Lira.Schemas.treeSignature}\n\nentry ../evil  $hash\n"

        capture[Lira.Error](Lira.Tree.decode(encode(doc))).reason match
          case Lira.Error.Reason.InvalidTree(_) => true
          case _                               => false
      . assert(identity)

      test(m"materialization applies deletions, replacements and additions"):
        val overlay = Lira.Tree.of(List(
          entry(t"a/One.class", t"one-sjsir"),
          entry(t"c/Three.sjsir", t"three")))

        val delete = List(TreePath(t"b/Two.class"))

        val expected = Lira.Tree.of(List(
          entry(t"a/One.class", t"one-sjsir"),
          entry(t"a/One.tasty", t"one-tasty"),
          entry(t"c/Three.sjsir", t"three")))

        same(Overlay.materialize(root, delete, overlay), expected)
      . assert(identity)

      test(m"diff produces the minimal overlay that materializes back"):
        val target = Lira.Tree.of(List(
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
        capture[Lira.Error](Overlay.materialize(root, List(TreePath(t"absent")), Lira.Tree.empty))
        . reason match
            case Lira.Error.Reason.OverlayNotMinimal(_) => true
            case _                                     => false
      . assert(identity)

      test(m"an overlay entry identical to the root is L107"):
        val overlay = Lira.Tree.of(List(entry(t"a/One.class", t"one")))

        capture[Lira.Error](Overlay.materialize(root, List(), overlay)).reason match
          case Lira.Error.Reason.OverlayNotMinimal(_) => true
          case _                                     => false
      . assert(identity)

      test(m"a deleted-and-re-added path is L107"):
        val overlay = Lira.Tree.of(List(entry(t"a/One.class", t"one-other")))
        val delete = List(TreePath(t"a/One.class"))

        capture[Lira.Error](Overlay.materialize(root, delete, overlay)).reason match
          case Lira.Error.Reason.OverlayNotMinimal(_) => true
          case _                                     => false
      . assert(identity)

    suite(m"Atoms and snapshots"):
      def item(path: Text, content: Text): (TreePath, Data) = (TreePath(path), encode(content))

      def hex(data: Data): Text = data.serialize[Hex]

      object Special extends Discipline:
        def id: Text = t"special/1"
        def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".special")
        def domain: Discipline.Domain = Discipline.Domain.Universal
        def keying: Discipline.Keying = Discipline.Keying.Declaration

        def guarantees(universe: Text): Set[Discipline.Guarantee] =
          Set(Discipline.Guarantee.Recompilation)

        def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
        :   Atomization raises Discipline.Error =

          val atoms = content.map: (path, data) =>
            Atom(path.text, Atom.Class.Replaceable, Lira.Hash(Lira.Hash.Domain.Atom(id), data))

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
            ((t"a/One.class", Atom.Class.Rigid), (t"b/Two.class", Atom.Class.Rigid))
      . assert(identity)

      test(m"opaque atom hashes are domain-separated from blob hashes"):
        val atom = OpaqueDiscipline.atomize(List(item(t"x", t"content")), context).atoms.stdlib.head
        Lira.Hash.text(atom.valueHash) != Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Blob, encode(t"content")))
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
        val one = Lira.Hash(Lira.Hash.Domain.Atom(t"opaque/1"), encode(t"1"))
        val two = Lira.Hash(Lira.Hash.Domain.Atom(t"opaque/1"), encode(t"2"))
        val (low, high) = if Blob.compare(one, two) < 0 then (one, two) else (two, one)

        val rowOne = t"atom rigid  ${Lira.Hash.text(high)}  key-one"
        val rowTwo = t"atom rigid  ${Lira.Hash.text(low)}  key-two"
        val doc =
          t"tel 1.0 ${Lira.Schemas.atomsSignature}\n\ndiscipline opaque/1\n\n$rowOne\n$rowTwo\n"

        capture[Lira.Error](AtomsBlob.decode(encode(doc))).reason match
          case Lira.Error.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)

      test(m"a malformed atom class is rejected"):
        val hash = Lira.Hash.text(Lira.Hash(Lira.Hash.Domain.Atom(t"opaque/1"), encode(t"1")))

        val row = t"atom solid  $hash  key-one"
        val doc = t"tel 1.0 ${Lira.Schemas.atomsSignature}\n\ndiscipline opaque/1\n\n$row\n"

        capture[Lira.Error](AtomsBlob.decode(encode(doc))).reason match
          case Lira.Error.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)

      test(m"duplicate keys within a discipline are rejected"):
        val atom = Atom(t"same", Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(t"x/1"), encode(t"1")))
        val other = Atom(t"same", Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(t"x/1"), encode(t"2")))

        capture[Discipline.Error](Atomization.of(t"x/1", List(atom, other))).reason match
          case Discipline.Error.Reason.Duplicate(_) => true
          case _                                   => false
      . assert(identity)

      test(m"snapshots are permutation-invariant"):
        val one = Snapshot(List(OpaqueDiscipline.atomize(content, context)))
        val two = Snapshot(List(OpaqueDiscipline.atomize(content.reverse, context)))
        Lira.Hash.text(one) == Lira.Hash.text(two)
      . assert(identity)

      test(m"snapshots deduplicate value hashes across atomizations"):
        val atomization = OpaqueDiscipline.atomize(content, context)
        val once = Snapshot(List(atomization))
        val twice = Snapshot(List(atomization, atomization))
        Lira.Hash.text(once) == Lira.Hash.text(twice)
      . assert(identity)

      test(m"a changed atom value changes the snapshot"):
        val one = Snapshot(List(OpaqueDiscipline.atomize(content, context)))

        val changed = List(item(t"a/One.class", t"one-changed"), item(t"b/Two.class", t"two"))
        val two = Snapshot(List(OpaqueDiscipline.atomize(changed, context)))
        Lira.Hash.text(one) != Lira.Hash.text(two)
      . assert(identity)

    suite(m"Grades, lineage and versioning"):
      import revolution.Semver

      def atom(key: Text, atomClass: Atom.Class, content: Text): Atom =
        Atom(key, atomClass, Lira.Hash(Lira.Hash.Domain.Atom(t"x/1"), encode(content)))

      def release(atoms: Atom*): List[Atomization] =
        List(Atomization.of(t"x/1", List.from(atoms)))

      val base = release(atom(t"a", Atom.Class.Rigid, t"1"), atom(t"b", Atom.Class.Replaceable, t"2"))

      test(m"an identical atom set grades as a patch"):
        Grade.between(base, base)
      . assert(_ == Grade.Patch)

      test(m"a pure rigid addition grades as minor"):
        val next = release(
          atom(t"a", Atom.Class.Rigid, t"1"),
          atom(t"b", Atom.Class.Replaceable, t"2"),
          atom(t"c", Atom.Class.Rigid, t"3"))

        Grade.between(base, next)
      . assert(_ == Grade.Minor)

      test(m"a replaceable value change with a surviving key grades as minor"):
        val next = release(
          atom(t"a", Atom.Class.Rigid, t"1"),
          atom(t"b", Atom.Class.Replaceable, t"2-changed"))

        Grade.between(base, next)
      . assert(_ == Grade.Minor)

      test(m"a rigid removal grades as major"):
        Grade.between(base, release(atom(t"b", Atom.Class.Replaceable, t"2")))
      . assert(_ == Grade.Major)

      test(m"a rigid value change grades as major"):
        val next = release(
          atom(t"a", Atom.Class.Rigid, t"1-changed"),
          atom(t"b", Atom.Class.Replaceable, t"2"))

        Grade.between(base, next)
      . assert(_ == Grade.Major)

      test(m"a replaceable removal grades as major"):
        Grade.between(base, release(atom(t"a", Atom.Class.Rigid, t"1")))
      . assert(_ == Grade.Major)

      val snapshot = Snapshot(base)
      val older = Snapshot(release(atom(t"a", Atom.Class.Rigid, t"1")))

      test(m"a lineage ending in the release's snapshot passes L109"):
        Lineage.check(List(older, snapshot), snapshot)
        true
      . assert(identity)

      test(m"a lineage not ending in the release's snapshot fails L109"):
        capture[Lira.Error](Lineage.check(List(snapshot, older), snapshot)).reason
      . assert(_ == Lira.Error.Reason.LineageMismatch)

      test(m"an empty lineage fails L109"):
        capture[Lira.Error](Lineage.check(List(), snapshot)).reason
      . assert(_ == Lira.Error.Reason.LineageMismatch)

      test(m"lineage membership decides satisfaction"):
        val absent = Snapshot(release(atom(t"z", Atom.Class.Rigid, t"9")))

        (Lineage.contains(List(older, snapshot), older),
         Lineage.contains(List(older, snapshot), absent))
      . assert(_ == (true, false))

      test(m"a minor step appends its snapshot to the lineage"):
        Versioning.extendLineage(List(older), snapshot, Grade.Minor).stdlib
        . map { hash => Lira.Hash.text(hash) }
      . assert(_ == scala.List(Lira.Hash.text(older), Lira.Hash.text(snapshot)))

      test(m"a patch step leaves the lineage unchanged"):
        Versioning.extendLineage(List(older), older, Grade.Patch).stdlib.size
      . assert(_ == 1)

      test(m"a major step without explicit request is refused (L110)"):
        capture[Lira.Error](Versioning.extendLineage(List(older), snapshot, Grade.Major)).reason
      . assert:
          case Lira.Error.Reason.UngradedSuccessor(_) => true
          case _                                     => false

      test(m"a requested major step begins a fresh lineage"):
        Versioning.extendLineage(List(older), snapshot, Grade.Major, forceMajor = true).stdlib
        . map { hash => Lira.Hash.text(hash) }
      . assert(_ == scala.List(Lira.Hash.text(snapshot)))

      test(m"a delta records additions and replacements"):
        val next = release(
          atom(t"a", Atom.Class.Rigid, t"1"),
          atom(t"b", Atom.Class.Replaceable, t"2-changed"),
          atom(t"c", Atom.Class.Rigid, t"3"))

        val delta = Lira.Delta.compute(base, next)
        (delta.add.stdlib.size, delta.replace.stdlib.size)
      . assert(_ == (2, 1))

      test(m"a delta round-trips through its canonical encoding"):
        val next = release(
          atom(t"a", Atom.Class.Rigid, t"1"),
          atom(t"b", Atom.Class.Replaceable, t"2-changed"),
          atom(t"c", Atom.Class.Rigid, t"3"))

        val delta = Lira.Delta.compute(base, next)
        val back = Lira.Delta.decode(delta.encode)
        back.encode.serialize[Hex] == delta.encode.serialize[Hex]
      . assert(identity)

      test(m"an empty delta round-trips"):
        val delta = Lira.Delta.compute(base, base)
        val back = Lira.Delta.decode(delta.encode)
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
      . assert(_ == scala.List(Lira.Advisory.VersionMismatch(Semver(1, 2, 4), Semver(1, 3, 0))))

      test(m"a suffixed version raises a not-numeric advisory"):
        val suffixed = Semver(1, 2, 3, prerelease = List(t"RC1"))
        Versioning.advisories(suffixed, Unset, Grade.Patch).stdlib
      . assert(_ == scala.List(Lira.Advisory.NotNumeric(Semver(1, 2, 3, prerelease = List(t"RC1")))))

    suite(m"Container and verification"):
      test(m"an assembled lira reads back and verifies"):
        val report = Verification.install(Lira.read(makeLira()))
        report.materialized.stdlib.map { pair => pair(0).realm }
      . assert(_ == scala.List(t"jvm", t"sjsir"))

      test(m"resource/1 atomizes exports by name and tracks content"):
        import Lira.Manifest.{Resource, ResourceMode}
        val claims = List(
          Resource(ResourceMode.Export, TreePath(t"r/exported.conf")),
          Resource(ResourceMode.Track, TreePath(t"r/tracked.json")),
          Resource(ResourceMode.Scan, TreePath(t"r/plugins")))

        val registry = Discipline.Registry(List(), claims)

        def atoms(exportBytes: Text, trackBytes: Text) =
          registry.atomize(List(
            (TreePath(t"r/exported.conf"), encode(exportBytes)),
            (TreePath(t"r/tracked.json"), encode(trackBytes)),
            (TreePath(t"r/plugins/one.txt"), encode(t"scanned"))),
            Discipline.Context(t"jvm"))

        val one = atoms(t"alpha", t"schema-v1")
        val two = atoms(t"beta", t"schema-v1")
        val three = atoms(t"alpha", t"schema-v2")

        def summary(list: List[Atomization]) =
          list.stdlib.flatMap: atomization =>
            atomization.atoms.stdlib.map: atom =>
              (atom.key, atom.atomClass, Lira.Hash.text(atom.valueHash))

        // The scanned item is claimed atomless, so only two atoms exist; the export's hash is a
        // function of the name alone, so changing its bytes changes nothing; the tracked atom is
        // replaceable and its hash follows its content.
        (summary(one).map(_(0)), summary(one).map(_(1)),
         summary(one) == summary(two), summary(one) == summary(three))
      . assert(_ == (scala.List(t"r/exported.conf", t"r/tracked.json"),
          scala.List(Atom.Class.Rigid, Atom.Class.Replaceable), true, false))

      test(m"a resource path declared twice is L124"):
        import Lira.Manifest.{Resource, ResourceMode}

        capture[Lira.Error](ResourceDiscipline.check(List(
          Resource(ResourceMode.Export, TreePath(t"r/a.conf")),
          Resource(ResourceMode.Track, TreePath(t"r/a.conf"))))).reason match

          case Lira.Error.Reason.BadResource(_) => true
          case _                               => false
      . assert(identity)

      test(m"an export under a scanned directory is L124"):
        import Lira.Manifest.{Resource, ResourceMode}

        capture[Lira.Error](ResourceDiscipline.check(List(
          Resource(ResourceMode.Scan, TreePath(t"r/plugins")),
          Resource(ResourceMode.Export, TreePath(t"r/plugins/one.txt"))))).reason match

          case Lira.Error.Reason.BadResource(_) => true
          case _                               => false
      . assert(identity)

      test(m"a scanned directory may be empty and claims only what lies under it"):
        import Lira.Manifest.{Resource, ResourceMode}
        val discipline = ResourceDiscipline(List(
          Resource(ResourceMode.Scan, TreePath(t"r/plugins"))))

        val data = encode(t"x")

        (discipline.claims(TreePath(t"r/plugins/a.txt"), data),
         discipline.claims(TreePath(t"r/plugins"), data),
         discipline.claims(TreePath(t"r/pluginsx/a.txt"), data))
      . assert(_ == (true, false, false))

      test(m"an assembled release carries its resource atoms and round-trips"):
        import Lira.Manifest.{Resource, ResourceMode}
        val claims = List(
          Resource(ResourceMode.Export, TreePath(t"r/exported.conf")),
          Resource(ResourceMode.Scan, TreePath(t"r/plugins")))

        val content = List(
          (TreePath(t"a/A.class"), classA),
          (TreePath(t"r/exported.conf"), encode(t"config")),
          (TreePath(t"r/plugins/one.txt"), encode(t"plugin")))

        val bytes = LiraAssembler.assemble(t"example-core",
          List(LiraAssembler.SectionInput(t"jvm", content)),
          Discipline.Registry(List()),
          toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
          resource = claims)

        val back = Lira.read(bytes)
        val report = Verification.install(back)

        val resourceAtoms = report.atomizations.stdlib
          . filter(_.discipline == t"resource/1")
          . flatMap(_.atoms.stdlib.map(_.key))

        // The scanned item is atomless and the classfile falls to opaque/1, so resource/1
        // contributes exactly the one exported name.
        (back.manifest.resource.stdlib.map(_.mode),
         resourceAtoms,
         back.manifest.render == Lira.read(bytes).manifest.render)
      . assert(_ == (scala.List(Lira.Manifest.ResourceMode.Export, Lira.Manifest.ResourceMode.Scan),
          scala.List(t"r/exported.conf"), true))

      test(m"an export resolving to no item is L125"):
        import Lira.Manifest.{Resource, ResourceMode}

        val claims = List(Resource(ResourceMode.Export, TreePath(t"r/absent.conf")))

        capture[Lira.Error]:
          LiraAssembler.assemble(t"example-core",
            List(LiraAssembler.SectionInput(t"jvm", List((TreePath(t"a/A.class"), classA)))),
            Discipline.Registry(List()),
            toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
            resource = claims)

        . reason
      . assert(_ == Lira.Error.Reason.IneffectiveResource(t"r/absent.conf"))

      test(m"an export another discipline claims is L125"):
        import Lira.Manifest.{Resource, ResourceMode}

        object Greedy extends Discipline:
          def id: Text = t"greedy/1"
          def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".conf")
          def domain: Discipline.Domain = Discipline.Domain.Universal
          def keying: Discipline.Keying = Discipline.Keying.Declaration

          def guarantees(universe: Text): Set[Discipline.Guarantee] =
            Set(Discipline.Guarantee.Recompilation)

          def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
          :   Atomization raises Discipline.Error =
            Atomization.of(id, List())

        val claims = List(Resource(ResourceMode.Export, TreePath(t"r/taken.conf")))

        capture[Lira.Error]:
          LiraAssembler.assemble(t"example-core",
            List(LiraAssembler.SectionInput(t"jvm",
              List((TreePath(t"r/taken.conf"), encode(t"config"))))),
            Discipline.Registry(List(Greedy)),
            toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
            resource = claims)

        . reason
      . assert(_ == Lira.Error.Reason.IneffectiveResource(t"r/taken.conf"))

      test(m"a manifest with profiles and integrations round-trips through its rendering"):
        val rootTree = Lira.Tree.of(List(TreeEntry(TreePath(t"a/A.class"), blob(classA))))
        val altTree = Lira.Tree.of(List(TreeEntry(TreePath(t"a/A.class"), blob(sjsirA))))

        val context = Discipline.Context(t"jvm")
        val atomizations = Discipline.Registry(List()).atomize(
          List((TreePath(t"a/A.class"), classA)), context)

        val atomsData = AtomsBlob.encode(atomizations.stdlib.head)

        val manifest = Lira.Manifest(
          module      = t"example-core",
          version     = revolution.Semver(0, 1, 0),
          lineage     = List(Snapshot(atomizations)),
          toolchain   = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
          api         = List(Lira.Manifest.Api(t"opaque/1", blob(atomsData))),
          profile     = List(Lira.Manifest.Profile(t"jvm/1",
              breaks = List(Lira.Manifest.Guarantee.Linkage))),
          integration = List(
            Lira.Manifest.Integration(t"new", rank = 0L),
            Lira.Manifest.Integration(t"old", rank = 1L, label = t"built against the rudiments 0.x line")),
          dependency  = List(Lira.Manifest.Dependency(t"beta",
              blob(encode(t"snapshot")), integration = List(t"old"))),
          section     = List(
            Section(t"jvm", integration = t"new", tree = blob(rootTree.encode)),
            Section(t"jvm", integration = t"old", tree = blob(altTree.encode))),
          payload     = Lira.Manifest.Payload(t"brotli", 0L, blob(encode(t""))))

        val data = Lira.assemble(manifest,
          List(classA, sjsirA, rootTree.encode, altTree.encode, atomsData))

        val back = Lira.read(data).manifest

        // Assembly fills in the payload length and hash, so the round-trip property is that a
        // re-read renders identically, not that it matches the pre-assembly stub.
        (Lira.read(data).manifest.render == back.render,
         back.profile.stdlib.head.breaks.stdlib.head,
         back.integration.stdlib.map(_.id),
         back.section.stdlib.map(_.integration.or(t"-")),
         back.dependency.stdlib.head.integration.stdlib,
         back.integration.stdlib(1).label.or(t"-"))
      . assert(_ == (true, Lira.Manifest.Guarantee.Linkage, scala.List(t"new", t"old"),
          scala.List(t"new", t"old"), scala.List(t"old"),
          t"built against the rudiments 0.x line"))

      test(m"two sections sharing a universe and integration are L131"):
        val tree = Lira.Tree.of(List(TreeEntry(TreePath(t"a/A.class"), blob(classA))))

        val manifest = Lira.Manifest(
          module      = t"example-core",
          lineage     = List(Snapshot(List())),
          api         = List(),
          integration = List(Lira.Manifest.Integration(t"one")),
          section     = List(
            Section(t"jvm", integration = t"one", tree = blob(tree.encode)),
            Section(t"jvm", integration = t"one", tree = blob(tree.encode))),
          payload     = Lira.Manifest.Payload(t"brotli", 0L, blob(encode(t""))))

        capture[Lira.Error](Verification.integrations(manifest)).reason match
          case Lira.Error.Reason.BadIntegration(_) => true
          case _                                  => false
      . assert(identity)

      test(m"a section naming an undeclared integration is L131"):
        val manifest = Lira.Manifest(
          module      = t"example-core",
          lineage     = List(Snapshot(List())),
          api         = List(),
          integration = List(Lira.Manifest.Integration(t"one")),
          section     = List(Section(t"jvm", integration = t"other",
              tree = blob(encode(t"tree")))),
          payload     = Lira.Manifest.Payload(t"brotli", 0L, blob(encode(t""))))

        capture[Lira.Error](Verification.integrations(manifest)).reason match
          case Lira.Error.Reason.BadIntegration(_) => true
          case _                                  => false
      . assert(identity)

      test(m"a declared integration with no section is L133"):
        val manifest = Lira.Manifest(
          module      = t"example-core",
          lineage     = List(Snapshot(List())),
          api         = List(),
          integration = List(Lira.Manifest.Integration(t"one"), Lira.Manifest.Integration(t"two")),
          section     = List(Section(t"jvm", integration = t"one",
              tree = blob(encode(t"tree")))),
          payload     = Lira.Manifest.Payload(t"brotli", 0L, blob(encode(t""))))

        capture[Lira.Error](Verification.integrations(manifest)).reason
      . assert(_ == Lira.Error.Reason.UnrealizedIntegration(t"two"))

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
        val sjsir = report.materialized.stdlib.find { pair => pair(0).realm == t"sjsir" }

        sjsir.map { pair => pair(1).entries.map(_.path.text).stdlib }
      . assert(_ == scala.Some(scala.List(t"a/A.sjsir", t"a/A.tasty")))

      test(m"a corrupted directive is L115"):
        val data = makeLira().mutable(using Unsafe)
        data(0) = '?'.toByte

        capture[Lira.Error](Lira.read(Array.unsafeFrozen(data))).reason
      . assert(_ == Lira.Error.Reason.BadDirective)

      test(m"a manifest with a sigil in its pragma is L116"):
        val body = t"#!/usr/bin/env lira\ntel 1.0 ${Lira.Schemas.liraSignature} !\n\nmodule x\n##\n"

        capture[Lira.Error](Lira.read(encode(body))).reason match
          case Lira.Error.Reason.SigilSpecified      => true
          case Lira.Error.Reason.InvalidManifest(_)  => false
          case _                                    => false
      . assert(identity)

      test(m"a missing separator is rejected"):
        // truncate the file to just the directive line, which contains no separator
        val data = makeLira()
        val short = Array[Byte](20)
        System.arraycopy(Array.unsafeJvm(data), 0, short.raw, 0, 20)

        capture[Lira.Error](Lira.read(Array.freeze(short))).reason match
          case Lira.Error.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)

      test(m"a wrong declared payload hash is caught at verification"):
        val lira = Lira.read(makeLira())
        val wrong = lira.manifest.payload.copy(hash = blob(encode(t"wrong")))
        val tampered = lira.copy(manifest = lira.manifest.copy(payload = wrong))

        capture[Lira.Error](Verification.install(tampered)).reason
      . assert(_ == Lira.Error.Reason.PayloadHash)

      test(m"a wrong declared payload length is caught at verification"):
        val lira = Lira.read(makeLira())
        val payload = lira.manifest.payload
        val wrong = payload.copy(length = payload.length + 1)
        val tampered = lira.copy(manifest = lira.manifest.copy(payload = wrong))

        capture[Lira.Error](Verification.install(tampered)).reason match
          case Lira.Error.Reason.PayloadLength(_) => true
          case _                                 => false
      . assert(identity)

      test(m"a dangling atoms reference is L104"):
        val lira = Lira.read(makeLira())
        val wrong = List(Lira.Manifest.Api(t"opaque/1", blob(encode(t"absent"))))
        val tampered = lira.copy(manifest = lira.manifest.copy(api = wrong))

        capture[Lira.Error](Verification.install(tampered)).reason match
          case Lira.Error.Reason.MissingBlob(_) => true
          case _                               => false
      . assert(identity)

      test(m"a lineage not ending in the snapshot is L109"):
        val lira = Lira.read(makeLira())
        val tampered = lira.copy(manifest = lira.manifest.copy(lineage = List(blob(encode(t"x")))))

        capture[Lira.Error](Verification.install(tampered)).reason
      . assert(_ == Lira.Error.Reason.LineageMismatch)

      test(m"a corrupted compressed payload is rejected"):
        val data = makeLira().mutable(using Unsafe)
        data(data.length - 1) = (data(data.length - 1) ^ 0x55).toByte

        capture[Lira.Error](Verification.install(Lira.read(Array.unsafeFrozen(data)))).reason match
          case Lira.Error.Reason.MalformedPayload(_) => true
          case Lira.Error.Reason.PayloadHash         => true
          case Lira.Error.Reason.PayloadLength(_)    => true
          case _                                    => false
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

      def signed(): Lira.Manifest =
        val manifest = Lira.read(makeLira()).manifest

        ManifestSigning.sign
          (manifest, t"jon.pretty@propensive.com", t"ml-dsa-65", mlDsa65, privateKey, publicKey)

      test(m"a signed manifest verifies against the signer's key"):
        ManifestSigning.verify(signed(), ManifestSigning.Keyring(List(publicKey)), schemes)
        true
      . assert(identity)

      test(m"the signing input is unchanged by signing"):
        val manifest = Lira.read(makeLira()).manifest
        val one = Lira.Hash.text(ManifestSigning.input(manifest))
        val two = Lira.Hash.text(ManifestSigning.input(signed()))
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

        capture[Lira.Error](ManifestSigning.verify(tampered, keyring, schemes)).reason match
          case Lira.Error.Reason.BadSignature(_) => true
          case _                                => false
      . assert(identity)

      test(m"an unknown algorithm is rejected, never ignored"):
        val record = signed().signature.stdlib.head.copy(algorithm = t"quantum-magic")
        val manifest = signed().copy(signature = List(record))
        val keyring = ManifestSigning.Keyring(List(publicKey))

        capture[Lira.Error](ManifestSigning.verify(manifest, keyring, schemes)).reason match
          case Lira.Error.Reason.UnknownAlgorithm(_) => true
          case _                                    => false
      . assert(identity)

      test(m"an unknown key fingerprint is rejected"):
        val keyring = ManifestSigning.Keyring(List(otherPublic))

        capture[Lira.Error](ManifestSigning.verify(signed(), keyring, schemes)).reason match
          case Lira.Error.Reason.UnknownKey(_) => true
          case _                              => false
      . assert(identity)

      test(m"a signed lira survives assembly, reading and verification"):
        val lira = Lira.read(makeLira())
        val stream = Lira.Payload.decompress
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

      def payloadStub(seed: Text): Lira.Manifest.Payload =
        Lira.Manifest.Payload(t"brotli", 1L, blob(encode(seed)))

      def stub
        ( module:       Text,
          lineage:      List[Data],
          owns:         List[Text]                     = List(),
          resources:    List[Lira.Manifest.Resource]    = List(),
          deps:         List[Lira.Manifest.Dependency]  = List(),
          version:      Optional[Semver]               = Unset,
          section:      List[Section]                  = List(),
          integrations: List[Lira.Manifest.Integration] = List(),
          profiles:     List[Lira.Manifest.Profile]     = List(),
          toolchain:    List[Lira.Manifest.Tool]        = List() )
      :   Lira.Manifest =

        Lira.Manifest
          ( module      = module,
            version     = version,
            lineage     = lineage,
            toolchain   = toolchain,
            owns        = owns,
            resource    = resources,
            api         = List(),
            profile     = profiles,
            integration = integrations,
            dependency  = deps,
            section     = section,
            payload     = payloadStub(module) )

      val snapOne = Lira.Hash(Lira.Hash.Domain.Snapshot, encode(t"one"))
      val snapTwo = Lira.Hash(Lira.Hash.Domain.Snapshot, encode(t"two"))

      test(m"two releases of one module are L111"):
        val path = Buildpath(List(stub(t"alpha", List(snapOne)), stub(t"alpha", List(snapTwo))))

        capture[Lira.Error](path.validate(t"jvm")).reason match
          case Lira.Error.Reason.DuplicateModule(_) => true
          case _                                   => false
      . assert(identity)

      test(m"nested namespace claims are L112"):
        val path = Buildpath(List(
          stub(t"alpha", List(snapOne), owns = List(t"gossamer")),
          stub(t"beta", List(snapTwo), owns = List(t"gossamer.text"))))

        capture[Lira.Error](path.validate(t"jvm")).reason match
          case Lira.Error.Reason.NamespaceClash(_) => true
          case _                                  => false
      . assert(identity)

      test(m"disjoint namespace claims pass"):
        val path = Buildpath(List(
          stub(t"alpha", List(snapOne), owns = List(t"gossamer")),
          stub(t"beta", List(snapTwo), owns = List(t"gossamers"))))

        path.validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"an export path claimed by two modules is L126"):
        import Lira.Manifest.{Resource, ResourceMode}
        val claim = List(Resource(ResourceMode.Export, TreePath(t"r/shared.conf")))

        val path = Buildpath(List(
          stub(t"alpha", List(snapOne), resources = claim),
          stub(t"beta", List(snapTwo), resources = claim)))

        capture[Lira.Error](path.validate(t"jvm")).reason
      . assert(_ == Lira.Error.Reason.ResourceClash(t"r/shared.conf"))

      test(m"a scanned directory shared by two modules is exempt from L126"):
        import Lira.Manifest.{Resource, ResourceMode}
        val claim = List(Resource(ResourceMode.Scan, TreePath(t"r/plugins")))

        val path = Buildpath(List(
          stub(t"alpha", List(snapOne), resources = claim),
          stub(t"beta", List(snapTwo), resources = claim)))

        path.validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"an absent dependency is L113"):
        val needy = stub(t"alpha", List(snapOne),
          deps = List(Lira.Manifest.Dependency(t"missing", snapTwo)))

        capture[Lira.Error](Buildpath(List(needy)).validate(t"jvm")).reason match
          case Lira.Error.Reason.AbsentDependency(_) => true
          case _                                    => false
      . assert(identity)

      test(m"a universe-scoped dependency binds only its universes"):
        val needy = stub(t"alpha", List(snapOne),
          deps = List(Lira.Manifest.Dependency(t"missing", snapTwo, universe = List(t"nir"))))

        val path = Buildpath(List(needy))
        val jvm = path.validate(t"jvm").stdlib.size

        val nir = capture[Lira.Error](path.validate(t"nir")).reason match
          case Lira.Error.Reason.AbsentDependency(_) => true
          case _                                    => false

        (jvm, nir)
      . assert(_ == (0, true))

      test(m"an integration-scoped dependency binds only its integration"):
        val dependency = Lira.Manifest.Dependency(t"missing", snapTwo, integration = List(t"two"))
        val one = Lira.Manifest.Integration(t"one", rank = 0L)
        val two = Lira.Manifest.Integration(t"two", rank = 1L)

        val needy = stub(t"alpha", List(snapOne), deps = List(dependency),
          integrations = List(one, two))

        // The `two` integration needs an absent module, so only `one` yields a valid
        // assignment — closure decides the choice, with no new rule (§13.3).
        Buildpath(List(needy)).resolved(t"jvm")(0)(t"alpha")
      . assert(_ == t"one")

      test(m"the canonical assignment prefers the lower rank"):
        val alpha = stub(t"alpha", List(snapOne), integrations = List(
          Lira.Manifest.Integration(t"slow", rank = 7L),
          Lira.Manifest.Integration(t"fast", rank = 2L)))

        Buildpath(List(alpha)).resolved(t"jvm")(0)(t"alpha")
      . assert(_ == t"fast")

      test(m"an unranked integration sorts after every ranked one"):
        val alpha = stub(t"alpha", List(snapOne), integrations = List(
          Lira.Manifest.Integration(t"anon"),
          Lira.Manifest.Integration(t"ranked", rank = 9L)))

        Buildpath(List(alpha)).resolved(t"jvm")(0)(t"alpha")
      . assert(_ == t"ranked")

      test(m"equal ranks break the tie on id"):
        val alpha = stub(t"alpha", List(snapOne), integrations = List(
          Lira.Manifest.Integration(t"zeta", rank = 1L),
          Lira.Manifest.Integration(t"beta", rank = 1L)))

        Buildpath(List(alpha)).resolved(t"jvm")(0)(t"alpha")
      . assert(_ == t"beta")

      test(m"an integration whose dependency is unsatisfiable is not chosen"):
        // `old` requires a snapshot the present release of beta does not carry; `new` requires
        // one it does. Rule 5, not rule 1, is what rejects the wrong assignment here.
        val provider = stub(t"beta", List(snapTwo))

        val alpha = stub(t"alpha", List(snapOne),
          integrations = List(
            Lira.Manifest.Integration(t"old", rank = 0L),
            Lira.Manifest.Integration(t"new", rank = 1L)),
          deps = List(
            Lira.Manifest.Dependency(t"beta", snapOne, integration = List(t"old")),
            Lira.Manifest.Dependency(t"beta", snapTwo, integration = List(t"new"))))

        Buildpath(List(alpha, provider)).resolved(t"jvm")(0)(t"alpha")
      . assert(_ == t"new")

      test(m"no satisfiable integration is L132"):
        val provider = stub(t"beta", List(snapTwo))

        val alpha = stub(t"alpha", List(snapOne),
          integrations = List(Lira.Manifest.Integration(t"only", rank = 0L)),
          deps = List(Lira.Manifest.Dependency(t"beta", snapOne, integration = List(t"only"))))

        capture[Lira.Error](Buildpath(List(alpha, provider)).resolved(t"jvm")).reason
      . assert(_ == Lira.Error.Reason.NoAssignment(t"alpha"))

      test(m"a release declaring no integration assigns the implicit one"):
        Buildpath(List(stub(t"alpha", List(snapOne)))).resolved(t"jvm")(0)(t"alpha").absent
      . assert(identity)

      test(m"lineage membership satisfies a requirement"):
        val provider = stub(t"beta", List(snapOne, snapTwo))
        val needy = stub(t"alpha", List(snapOne),
          deps = List(Lira.Manifest.Dependency(t"beta", snapOne)))

        Buildpath(List(needy, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"a recorded span satisfies a requirement outside the lineage"):
        // beta's lineage carries only snapTwo, so alpha's snapOne requirement fails on lineage
        // membership; the recorded span across the major boundary carries it (§13.4).
        val provider = stub(t"beta", List(snapTwo))

        val needy = stub(t"alpha", List(snapOne),
          deps = List(Lira.Manifest.Dependency(t"beta", snapOne, spans = List(snapTwo))))

        Buildpath(List(needy, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"a span naming a snapshot the candidate does not carry is still L114"):
        val provider = stub(t"beta", List(snapTwo))
        val other = Lira.Hash(Lira.Hash.Domain.Snapshot, encode(t"three"))

        val needy = stub(t"alpha", List(snapOne),
          deps = List(Lira.Manifest.Dependency(t"beta", snapOne, spans = List(other))))

        capture[Lira.Error](Buildpath(List(needy, provider)).validate(t"jvm")).reason match
          case Lira.Error.Reason.Unsatisfiable(_) => true
          case _                                 => false
      . assert(identity)

      test(m"a span lets an integration resolve that lineage membership would reject"):
        val provider = stub(t"beta", List(snapTwo))

        val alpha = stub(t"alpha", List(snapOne),
          integrations = List(Lira.Manifest.Integration(t"spanned", rank = 0L)),
          deps = List(Lira.Manifest.Dependency(t"beta", snapOne, integration = List(t"spanned"),
              spans = List(snapTwo))))

        Buildpath(List(alpha, provider)).resolved(t"jvm")(0)(t"alpha")
      . assert(_ == t"spanned")

      test(m"a requirement outside the lineage is L114"):
        val provider = stub(t"beta", List(snapTwo))
        val needy = stub(t"alpha", List(snapOne),
          deps = List(Lira.Manifest.Dependency(t"beta", snapOne)))

        capture[Lira.Error](Buildpath(List(needy, provider)).validate(t"jvm")).reason match
          case Lira.Error.Reason.Unsatisfiable(_) => true
          case _                                 => false
      . assert(identity)

      test(m"a diamond resolves iff one lineage contains both snapshots"):
        val provider = stub(t"omega", List(snapOne, snapTwo))
        val left = stub(t"alpha", List(blob(encode(t"al"))),
          deps = List(Lira.Manifest.Dependency(t"omega", snapOne)))

        val right = stub(t"beta", List(blob(encode(t"be"))),
          deps = List(Lira.Manifest.Dependency(t"omega", snapTwo)))

        Buildpath(List(left, right, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"a build pin must match the implementation identity"):
        val provider = stub(t"beta", List(snapOne))
        val pinned = stub(t"alpha", List(snapTwo), deps = List(
          Lira.Manifest.Dependency(t"beta", snapOne, build = blob(encode(t"other")))))

        capture[Lira.Error](Buildpath(List(pinned, provider)).validate(t"jvm")).reason match
          case Lira.Error.Reason.Unsatisfiable(_) => true
          case _                                 => false
      . assert(identity)

      test(m"a matching build pin passes"):
        val provider = stub(t"beta", List(snapOne))
        val pinned = stub(t"alpha", List(snapTwo), deps = List(
          Lira.Manifest.Dependency(t"beta", snapOne, build = provider.payload.hash)))

        Buildpath(List(pinned, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 0)

      test(m"a version hint disagreement is advisory only"):
        val provider = stub(t"beta", List(snapOne), version = Semver(2, 0, 0))
        val needy = stub(t"alpha", List(snapTwo), deps = List(
          Lira.Manifest.Dependency(t"beta", snapOne, version = Semver(1, 0, 0))))

        Buildpath(List(needy, provider)).validate(t"jvm").stdlib.size
      . assert(_ == 1)

      test(m"a derivative hash resolves to its declaring release"):
        val derivative = blob(encode(t"the canonical jar"))
        val holder = stub(t"alpha", List(snapOne), section = List(
          Section(t"jvm", tree = blob(encode(t"tree")), derivative = derivative)))

        val path = Buildpath(List(holder, stub(t"beta", List(snapTwo))))
        path.byDerivative(derivative).let(_(0).module).or(t"absent")
      . assert(_ == t"alpha")

      test(m"a development release is unpublishable (L117)"):
        capture[Lira.Error](Buildpath.publishable(stub(t"alpha", List(snapOne)), List())).reason
      . assert(_ == Lira.Error.Reason.VersionRequired)

      test(m"a build pin is unpublishable (L118)"):
        val pinned = stub(t"alpha", List(snapOne), version = Semver(0, 0, 0), deps = List(
          Lira.Manifest.Dependency(t"beta", snapTwo, build = blob(encode(t"pin")))))

        capture[Lira.Error](Buildpath.publishable(pinned, List())).reason match
          case Lira.Error.Reason.BuildPinned(_) => true
          case _                               => false
      . assert(identity)

      test(m"an unpublished dependency is unpublishable (L119)"):
        val needy = stub(t"alpha", List(snapOne), version = Semver(0, 0, 0),
          deps = List(Lira.Manifest.Dependency(t"beta", snapTwo)))

        capture[Lira.Error](Buildpath.publishable(needy, List())).reason match
          case Lira.Error.Reason.UnpublishedDependency(_) => true
          case _                                         => false
      . assert(identity)

      test(m"a minor number defying the lineage is unpublishable (L120)"):
        val wrong = stub(t"alpha", List(snapOne, snapTwo), version = Semver(1, 3, 0))

        capture[Lira.Error](Buildpath.publishable(wrong, List())).reason match
          case Lira.Error.Reason.VersionProjection(_) => true
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

        val tree = Lira.Tree.of(List.from(entries))

        val manifest = Lira.Manifest
          ( module    = t"assignee",
            lineage   = List(snapshot),
            toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
            api       = List(Lira.Manifest.Api(t"opaque/1", blob(atomsData))),
            section   = List(Section(t"jvm", tree = blob(tree.encode))),
            payload   = payloadStub(t"replaced") )

        val blobs = (apiItems ++ extraItems).map { pair => pair(1) }
        Lira.assemble(manifest, List.from(blobs :+ tree.encode :+ atomsData))

      val versionOne = scala.List((t"a/A.class", t"alpha one"))

      def published(): Lira =
        val dev = Lira.read(makeRelease(versionOne, scala.Nil))
        val assigned = Publication.assign(dev, Unset, List())
        val stream = Lira.Payload.decompress
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

        capture[Lira.Error](Publication.assign(dev, base, List(base.manifest))).reason
      . assert:
          case Lira.Error.Reason.UngradedSuccessor(_) => true
          case _                                     => false

      test(m"an explicit major begins a fresh lineage"):
        val base = published()
        val dev = Lira.read(makeRelease(scala.List((t"a/C.class", t"gamma")), scala.Nil))
        val manifest = Publication.assign(dev, base, List(base.manifest), forceMajor = true)
        (manifest.version, manifest.lineage.stdlib.size)
      . assert(_ == (Semver(0, 2, 0), 1))

      test(m"a used-set closes over replaceable references"):
        val rigid = Atom(t"target", Atom.Class.Rigid,
          Lira.Hash(Lira.Hash.Domain.Atom(t"x/1"), encode(t"rigid")))

        val inline = Atom(t"caller[inline]", Atom.Class.Replaceable,
          Lira.Hash(Lira.Hash.Domain.Atom(t"x/1"), encode(t"body")),
          references = List(Atom.Reference.Own(t"target")))

        val dependency = Atomization.of(t"x/1", List(rigid, inline))
        val closure = UsesBlob.closure(List(inline.valueHash), List((t"dep", dependency)))

        val expected = scala.collection.immutable.Set
          (Lira.Hash.text(inline.valueHash), Lira.Hash.text(rigid.valueHash))

        closure.stdlib.map { hash => Lira.Hash.text(hash) }.toSet == expected
      . assert(identity)

      test(m"a uses blob round-trips"):
        val atoms = List(blob(encode(t"u1")), blob(encode(t"u2")))
        val (module, back) = UsesBlob.decode(UsesBlob.encode(t"dep", atoms))
        (module, back.stdlib.size)
      . assert(_ == (t"dep", 2))

      test(m"spanning holds iff the candidate carries every used atom"):
        val one = Atom(t"a", Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(t"x/1"), encode(t"1")))
        val two = Atom(t"b", Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(t"x/1"), encode(t"2")))

        (UsesBlob.spanning(List(one.valueHash), List(one, two)),
         UsesBlob.spanning(List(one.valueHash, two.valueHash), List(one)))
      . assert(_ == (true, false))

      test(m"staleness detects replaced atoms in the used-set"):
        val old = blob(encode(t"old"))
        val neo = blob(encode(t"new"))

        (UsesBlob.staleness(List(old), List(Replacement(old, neo))),
         UsesBlob.staleness(List(neo), List(Replacement(old, neo))))
      . assert(_ == (true, false))

    suite(m"Joins, pins and coherence"):
      def payloadStub(module: Text): Lira.Manifest.Payload =
        Lira.Manifest.Payload(t"brotli", 0L, Lira.Hash(Lira.Hash.Domain.Blob, encode(module)))

      def stub
        ( module:    Text,
          lineage:   List[Data],
          deps:      List[Lira.Manifest.Dependency] = List(),
          section:   List[Section]                 = List(),
          profiles:  List[Lira.Manifest.Profile]    = List(),
          toolchain: List[Lira.Manifest.Tool]       = List() )
      :   Lira.Manifest =

        Lira.Manifest
          ( module     = module,
            lineage    = lineage,
            toolchain  = toolchain,
            api        = List(),
            profile    = profiles,
            dependency = deps,
            section    = section,
            payload    = payloadStub(module) )

      val snapOne = Lira.Hash(Lira.Hash.Domain.Snapshot, encode(t"join-one"))
      val snapTwo = Lira.Hash(Lira.Hash.Domain.Snapshot, encode(t"join-two"))
      val jsSection = Section(t"js", tree = blob(encode(t"js-tree")))

      def needy(): Lira.Manifest =
        stub(t"alpha", List(snapOne),
          deps = List(Lira.Manifest.Dependency(t"webstuff", snapTwo, serves = t"js")))

      test(m"a serves dependency round-trips through the manifest"):
        val bytes = LiraAssembler.assemble(t"consumer",
          List(LiraAssembler.SectionInput(t"jvm", List((TreePath(t"a/A.class"), classA)))),
          Discipline.Registry(List()),
          toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
          dependency = List(Lira.Manifest.Dependency(t"native-bits", snapTwo,
            universe = List(t"jvm"), serves = t"nir")))

        Lira.read(bytes).manifest.dependency.stdlib.map: dependency =>
          (dependency.module, dependency.serves, dependency.universe.stdlib)
      . assert(_ == scala.List((t"native-bits", Optional(t"nir"), scala.List(t"jvm"))))

      test(m"a join edge to a universe outside the target fails closure"):
        val web = stub(t"webstuff", List(snapTwo), section = List(jsSection))

        capture[Lira.Error](Buildpath(List(needy(), web)).validate(t"jvm")).reason
      . assert(_ == Lira.Error.Reason.AbsentDependency(t"webstuff"))

      test(m"a join edge into the target's joins passes closure"):
        val web = stub(t"webstuff", List(snapTwo), section = List(jsSection))
        Buildpath(List(needy(), web)).validate(t"jvm", joins = List(t"js")).stdlib.size
      . assert(_ == 0)

      test(m"a join edge to content the candidate does not offer fails closure"):
        val web = stub(t"webstuff", List(snapTwo),
          section = List(Section(t"jvm", tree = blob(encode(t"t")))))

        capture[Lira.Error]:
          Buildpath(List(needy(), web)).validate(t"jvm", joins = List(t"js"))
        . reason
      . assert(_ == Lira.Error.Reason.AbsentDependency(t"webstuff"))

      test(m"a serving release resolves its own dependencies in its universe"):
        // `webstuff` serves `js`, and its own dependency is scoped to `js` — so the target
        // being `jvm` does not exempt it: applicability quantifies over the universe a release
        // serves (§13.3), and the absent `polyfill` fails closure.
        val web = stub(t"webstuff", List(snapTwo), section = List(jsSection),
          deps = List(Lira.Manifest.Dependency(t"polyfill", snapOne, universe = List(t"js"))))

        capture[Lira.Error]:
          Buildpath(List(needy(), web)).validate(t"jvm", joins = List(t"js"))
        . reason
      . assert(_ == Lira.Error.Reason.AbsentDependency(t"polyfill"))

      test(m"a pin selects a declared integration over the canonical one"):
        val alpha = Lira.Manifest(
          module      = t"alpha",
          lineage     = List(snapOne),
          api         = List(),
          integration = List(
            Lira.Manifest.Integration(t"slow", rank = 7L),
            Lira.Manifest.Integration(t"fast", rank = 2L)),
          section     = List(),
          payload     = payloadStub(t"alpha"))

        Buildpath(List(alpha)).resolved(t"jvm", pins = List((t"alpha", t"slow")))(0)(t"alpha")
      . assert(_ == t"slow")

      test(m"a pin naming an undeclared integration is refused"):
        val alpha = stub(t"alpha", List(snapOne))

        capture[Lira.Error]:
          Buildpath(List(alpha)).resolved(t"jvm", pins = List((t"alpha", t"missing")))
        . reason match
            case Lira.Error.Reason.BadIntegration(_) => true
            case _                                  => false
      . assert(identity)

      test(m"rule 6 imposes a declared profile's coherence over the whole path"):
        object Strict extends EcosystemProfile:
          def id: Text = t"strict/1"
          def certifies: Set[Discipline.Guarantee] = Set(Discipline.Guarantee.Linkage)

          def check(previous: EcosystemProfile.Evidence, next: EcosystemProfile.Evidence)
          :   List[EcosystemProfile.Violation] raises Discipline.Error =
            List()

          override def coherence(releases: List[Lira.Manifest]): List[Text] =
            List.from:
              releases.stdlib.filter(_.toolchain.stdlib.isEmpty).map: manifest =>
                t"${manifest.module} records no toolchain"

        val registry = EcosystemProfile.Registry(List(Strict))
        val scala39 = List(Lira.Manifest.Tool(t"scala", t"3.9.0"))

        val declarer = stub(t"alpha", List(snapOne),
          profiles = List(Lira.Manifest.Profile(t"strict/1")), toolchain = scala39)

        val bare = stub(t"beta", List(snapTwo))
        val tooled = stub(t"beta", List(snapTwo), toolchain = scala39)

        val failing =
          capture[Lira.Error]:
            Buildpath(List(declarer, bare)).validate(t"jvm", profiles = registry)
          . reason match
              case Lira.Error.Reason.ProfileViolated(t"strict/1", _) => true
              case _                                                => false

        val passing =
          Buildpath(List(declarer, tooled)).validate(t"jvm", profiles = registry).stdlib.size

        (failing, passing)
      . assert(_ == (true, 0))

    suite(m"Derivative artifacts"):
      import distillate.*
      import galilei.*
      import prepositional.*
      import serpentine.*

      def store(datas: List[Data]): Blobstore = BlobStream.read(BlobStream.write(datas))

      val tree = Lira.Tree.of(List(
        TreeEntry(TreePath(t"a/A.class"), blob(classA)),
        TreeEntry(TreePath(t"a/A.tasty"), blob(tastyA))))

      test(m"derivation is byte-deterministic"):
        val blobstore = store(List(classA, tastyA))
        val one = Derivative.jar(tree, blobstore).serialize[Hex]
        val two = Derivative.jar(tree, blobstore).serialize[Hex]
        one == two
      . assert(identity)

      test(m"the derivative hash matches its pinned value"):
        Lira.Hash.text(Derivative.hash(tree, store(List(classA, tastyA))))
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

      test(m"materialization refuses a universe with no section as a closure failure"):
        val cache = unsafely:
          t"/tmp/reliquary-test-${java.lang.System.nanoTime}".as[Path on Linux]

        val lira = Lira.read(makeLira())

        capture[Lira.Error](Materializer.classpath(List(lira), t"nir", cache)).reason
      . assert(_ == Lira.Error.Reason.AbsentDependency(t"example-core"))

    suite(m"Publish-time verification"):
      object Special extends Discipline:
        def id: Text = t"special/1"
        def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".special")
        def domain: Discipline.Domain = Discipline.Domain.Universal
        def keying: Discipline.Keying = Discipline.Keying.Declaration

        def guarantees(universe: Text): Set[Discipline.Guarantee] =
          Set(Discipline.Guarantee.Recompilation)

        def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
        :   Atomization raises Discipline.Error =

          val atoms = content.map: (path, data) =>
            Atom(path.text, Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(id), data))

          Atomization.of(id, atoms)

      def assembled(): Data =
        LiraAssembler.assemble(t"example-core",
          List(
            LiraAssembler.SectionInput(t"jvm",
              List((TreePath(t"a/A.class"), classA), (TreePath(t"a/A.special"), tastyA))),
            LiraAssembler.SectionInput(t"sjsir",
              List((TreePath(t"a/A.class"), classA), (TreePath(t"a/A.special"), tastyA)))),
          Discipline.Registry(List(Special)),
          toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")))

      test(m"sections presenting different APIs are L108"):
        capture[Lira.Error]:
          LiraAssembler.assemble(t"example-core",
            List(
              LiraAssembler.SectionInput(t"jvm", List((TreePath(t"a/A.class"), classA))),
              LiraAssembler.SectionInput(t"sjsir", List((TreePath(t"a/A.class"), sjsirA)))),
            Discipline.Registry(List()),
            toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")))

        . reason match
            case Lira.Error.Reason.ApiDivergence(_) => true
            case _                                 => false
      . assert(identity)

      test(m"a declared profile with no implementation refuses assembly, L140"):
        capture[Lira.Error]:
          LiraAssembler.assemble(t"example-core",
            List(LiraAssembler.SectionInput(t"jvm", List((TreePath(t"a/A.class"), classA)))),
            Discipline.Registry(List()),
            toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
            profile = List(Lira.Manifest.Profile(t"jvm/1")))

        . reason
      . assert(_ == Lira.Error.Reason.UnimplementedClaim(t"jvm/1"))

      test(m"declared derivative hashes recompute at verification"):
        val lira = Lira.read(assembled())
        val report = Verification.install(lira)
        Derivative.verify(lira.manifest, report)
        true
      . assert(identity)

      test(m"a tampered derivative hash is L138"):
        val lira = Lira.read(assembled())
        val report = Verification.install(lira)

        val bogus = blob(encode(t"not the derivative"))

        val tampered = lira.manifest.copy(section = List.from:
          lira.manifest.section.stdlib.map: section =>
            section.copy(derivative = bogus))

        capture[Lira.Error](Derivative.verify(tampered, report)).reason
      . assert(_ == Lira.Error.Reason.BadDerivative(t"jvm"))

      test(m"re-atomization accepts what the assembler produced"):
        val lira = Lira.read(assembled())
        val report = Verification.install(lira)
        Verification.reatomize(lira.manifest, report, List(Special))
        true
      . assert(identity)

      test(m"a declared discipline with no implementation is L140 at re-atomization"):
        val lira = Lira.read(assembled())
        val report = Verification.install(lira)

        capture[Lira.Error](Verification.reatomize(lira.manifest, report, List())).reason
      . assert(_ == Lira.Error.Reason.UnimplementedClaim(t"special/1"))

      test(m"an atoms listing that does not recompute is L141"):
        // The declared listing is computed over different bytes than the tree carries, which
        // `install` cannot see — the listing parses and the snapshot matches it — and only
        // re-atomization catches.
        val context = Discipline.Context(t"jvm")
        val registry = Discipline.Registry(List())
        val wrong = List((TreePath(t"a/A.class"), sjsirA))

        val tree = Lira.Tree.of(List(TreeEntry(TreePath(t"a/A.class"), blob(classA))))
        val listing = AtomsBlob.encode(registry.atomize(wrong, context).stdlib.head)
        val snapshot = Snapshot(registry.atomize(wrong, context))

        val manifest = Lira.Manifest(
          module    = t"example-core",
          version   = revolution.Semver(0, 1, 0),
          lineage   = List(snapshot),
          toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")),
          api       = List(Lira.Manifest.Api(t"opaque/1", blob(listing))),
          section   = List(Section(t"jvm", tree = blob(tree.encode))),
          payload   = Lira.Manifest.Payload(t"brotli", 0L, blob(encode(t""))))

        val lira = Lira.read(Lira.assemble(manifest, List(classA, tree.encode, listing)))
        val report = Verification.install(lira)

        capture[Lira.Error](Verification.reatomize(lira.manifest, report, List())).reason
      . assert(_ == Lira.Error.Reason.AtomsMismatch(t"opaque/1"))

      test(m"evidence reconstructs each section's content for profiles"):
        val lira = Lira.read(assembled())
        val report = Verification.install(lira)
        val evidence = Verification.evidence(lira.manifest, report)

        evidence.sections.stdlib.map: section =>
          (section.realm, section.content.stdlib.map(_(0).text))

      . assert(_ == scala.List(
          (t"jvm", scala.List(t"a/A.class", t"a/A.special")),
          (t"sjsir", scala.List(t"a/A.class", t"a/A.special"))))

      test(m"a universe-specific discipline claims nothing outside its domain"):
        val scoped = new Discipline:
          def id: Text = t"scoped/1"
          def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".class")
          def domain: Discipline.Domain = Discipline.Domain.Realms(Set(t"jvm"))
          def keying: Discipline.Keying = Discipline.Keying.Membership

          def guarantees(universe: Text): Set[Discipline.Guarantee] =
            Set(Discipline.Guarantee.Linkage)

          def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
          :   Atomization raises Discipline.Error =

            val atoms = content.map: (path, data) =>
              Atom(path.text, Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(id), data))

            Atomization.of(id, atoms)

        val content = List((TreePath(t"a/A.class"), classA))
        val registry = Discipline.Registry(List(scoped))

        val jvm = registry.atomize(content, Discipline.Context(t"jvm"))
        val sjsir = registry.atomize(content, Discipline.Context(t"sjsir"))

        (jvm.stdlib.map(_.discipline), sjsir.stdlib.map(_.discipline))
      . assert(_ == (scala.List(t"scoped/1"), scala.List(t"opaque/1")))

    suite(m"Host contracts and requirements"):
      def capabilities(rows: Text): Data =
        encode(t"tel 1.0 ${Lira.Schemas.capabilitiesSignature}\n\n$rows")

      val hostContext = Discipline.Context(t"host")
      val gitOnly = capabilities(t"capability\n  name git\n")

      def atomsOf(data: Data): List[Atom] =
        CapabilityDiscipline.atomize(List((TreePath(t"capabilities"), data)), hostContext).atoms

      def hashesOf(data: Data): scala.List[Text] =
        atomsOf(data).stdlib.map { atom => Lira.Hash.text(atom.valueHash) }

      test(m"a capability listing atomizes to one rigid atom per row"):
        val listing = capabilities(t"capability\n  name git\ncapability\n  name sh\n")

        atomsOf(listing).stdlib.map { atom => (atom.key, atom.atomClass) }.toSet
      . assert(_ == scala.collection.immutable.Set
          ((t"git", Atom.Class.Rigid), (t"sh", Atom.Class.Rigid)))

      test(m"a probe is advisory and enters no atom"):
        hashesOf(capabilities(t"capability\n  name git\n  probe  command -v git\n"))
        == hashesOf(gitOnly)
      . assert(identity)

      test(m"a version predicate folds into the atom's value"):
        hashesOf(capabilities(t"capability\n  name git\n  version  >= 2.30\n"))
        != hashesOf(gitOnly)
      . assert(identity)

      test(m"unsorted capability rows are rejected"):
        val listing = capabilities(t"capability\n  name sh\ncapability\n  name git\n")

        capture[Discipline.Error](atomsOf(listing)).reason match
          case Discipline.Error.Reason.Malformed(_) => true
          case _                                   => false
      . assert(identity)

      test(m"a duplicated capability is rejected"):
        val listing = capabilities(t"capability\n  name git\ncapability\n  name git\n")

        capture[Discipline.Error](atomsOf(listing)).reason match
          case Discipline.Error.Reason.Malformed(_) => true
          case _                                   => false
      . assert(identity)

      test(m"a host contract assembles, reads and verifies"):
        val bytes = LiraAssembler.assemble(t"posix",
          List(LiraAssembler.SectionInput(t"host",
            List((TreePath(t"capabilities"), gitOnly)))),
          Discipline.Registry(List(CapabilityDiscipline)),
          toolchain = List(Lira.Manifest.Tool(t"lira", t"0.1")))

        val lira = Lira.read(bytes)
        Verification.install(lira)
        (lira.manifest.hostContract, lira.manifest.section.stdlib.map(_.realm))
      . assert(_ == (true, scala.List(t"host")))

      // L135's four exclusions, each on a hand-built manifest, since the assembler itself
      // refuses to produce one.
      def handBuilt
        ( integrations:  List[Lira.Manifest.Integration] = List(),
          integrationId: Optional[Text]                 = Unset,
          dependencies:  List[Lira.Manifest.Dependency]  = List(),
          requires:      List[Lira.Manifest.Requires]    = List(),
          extraSection:  Boolean                        = false )
      :   Data =

        val tree = Lira.Tree.of(List(TreeEntry(TreePath(t"capabilities"), blob(gitOnly))))

        val atomization =
          CapabilityDiscipline.atomize(List((TreePath(t"capabilities"), gitOnly)), hostContext)

        val atomsData = AtomsBlob.encode(atomization)
        val snapshot = Snapshot(List(atomization))
        val jvmTree = Lira.Tree.of(List(TreeEntry(TreePath(t"a/A.class"), blob(classA))))

        val sections =
          val host = Section(t"host", integrationId, blob(tree.encode), requires = requires)

          if extraSection
          then List(host, Section(t"jvm", integrationId, blob(jvmTree.encode)))
          else List(host)

        val manifest = Lira.Manifest(
          module      = t"posix",
          version     = revolution.Semver(0, 1, 0),
          lineage     = List(snapshot),
          toolchain   = List(Lira.Manifest.Tool(t"lira", t"0.1")),
          api         = List(Lira.Manifest.Api(t"capability/1", blob(atomsData))),
          integration = integrations,
          dependency  = dependencies,
          section     = sections,
          payload     = Lira.Manifest.Payload(t"brotli", 0L, blob(encode(t""))))

        Lira.assemble(manifest,
          List(gitOnly, tree.encode, atomsData, classA, jvmTree.encode))

      def shapeFailure(bytes: Data): Boolean =
        capture[Lira.Error](Verification.install(Lira.read(bytes))).reason match
          case Lira.Error.Reason.BadHostContract(_) => true
          case _                                   => false

      test(m"a host contract with a second section is L135"):
        shapeFailure(handBuilt(extraSection = true))
      . assert(identity)

      test(m"a host contract declaring an integration is L135"):
        shapeFailure(handBuilt(
          integrations = List(Lira.Manifest.Integration(t"alt")), integrationId = t"alt"))
      . assert(identity)

      test(m"a host contract declaring a dependency is L135"):
        shapeFailure(handBuilt(dependencies =
          List(Lira.Manifest.Dependency(t"other", blob(encode(t"snap"))))))
      . assert(identity)

      test(m"a host contract carrying requirements is L135"):
        shapeFailure(handBuilt(requires =
          List(Lira.Manifest.Requires(t"other", blob(encode(t"snap"))))))
      . assert(identity)

      test(m"section requirements round-trip through the manifest"):
        val requirement =
          Lira.Manifest.Requires(t"posix", blob(encode(t"snap")), uses = blob(encode(t"uses")))

        val bytes = LiraAssembler.assemble(t"consumer",
          List(LiraAssembler.SectionInput(t"jvm",
            List((TreePath(t"a/A.class"), classA)), requires = List(requirement))),
          Discipline.Registry(List()),
          toolchain = List(Lira.Manifest.Tool(t"scala", t"3.9.0")))

        val back = Lira.read(bytes).manifest.section.stdlib.head.requires.stdlib

        back.map: entry =>
          (entry.module, Lira.Hash.text(entry.api), entry.uses.let(Lira.Hash.text(_)))
      . assert(_ == scala.List(
          (t"posix", Lira.Hash.text(blob(encode(t"snap"))),
           Optional(Lira.Hash.text(blob(encode(t"uses")))))))

      // Rule 7 (§13.3, hosts.md §7), over stub manifests: satisfaction is manifest-decidable,
      // spanning arrives through the caller-supplied lookups.
      val snapA = Lira.Hash(Lira.Hash.Domain.Snapshot, encode(t"contract-a"))
      val snapB = Lira.Hash(Lira.Hash.Domain.Snapshot, encode(t"contract-b"))
      val usesHash = blob(encode(t"uses-blob"))

      def payloadStub(module: Text): Lira.Manifest.Payload =
        Lira.Manifest.Payload(t"brotli", 0L, Lira.Hash(Lira.Hash.Domain.Blob, encode(module)))

      def library(requires: List[Lira.Manifest.Requires], module: Text = t"consumer")
      :   Lira.Manifest =
        Lira.Manifest(
          module  = module,
          lineage = List(snapB),
          api     = List(),
          section = List(Section(t"jvm", tree = blob(encode(t"tree")), requires = requires)),
          payload = payloadStub(module))

      def contractStub(module: Text, lineage: List[Data]): Lira.Manifest =
        Lira.Manifest(
          module  = module,
          lineage = lineage,
          api     = List(),
          section = List(Section(t"host", tree = blob(encode(t"host-tree")))),
          payload = payloadStub(module))

      test(m"a requirement satisfied by the contract's lineage passes rule 7"):
        val lib = library(List(Lira.Manifest.Requires(t"posix", snapA)))
        val contract = contractStub(t"posix", List(snapA))
        Buildpath(List(lib)).validate(t"jvm", contracts = List(contract)).stdlib.size
      . assert(_ == 0)

      test(m"an unsatisfiable requirement is L136"):
        val lib = library(List(Lira.Manifest.Requires(t"posix", snapA)))
        val contract = contractStub(t"posix", List(snapB))

        capture[Lira.Error]:
          Buildpath(List(lib)).validate(t"jvm", contracts = List(contract))
        . reason
      . assert(_ == Lira.Error.Reason.UnsatisfiedRequirement(t"posix"))

      test(m"validation without a contract reports rule 7 as pending"):
        val lib = library(List(Lira.Manifest.Requires(t"posix", snapA)))

        Buildpath(List(lib)).validate(t"jvm").stdlib.exists: advisory =>
          advisory match
            case Lira.Advisory.HostPending(modules) => modules.stdlib == scala.List(t"posix")
            case _                                 => false
      . assert(identity)

      test(m"a requirement naming a library module is L137"):
        val lib = library(List(Lira.Manifest.Requires(t"other", snapA)))
        val other = library(List(), module = t"other")
        val contract = contractStub(t"posix", List(snapA))

        capture[Lira.Error]:
          Buildpath(List(lib, other)).validate(t"jvm", contracts = List(contract))
        . reason
      . assert(_ == Lira.Error.Reason.NotHostContract(t"other"))

      test(m"a non-contract given as a contract is L137"):
        val lib = library(List(Lira.Manifest.Requires(t"posix", snapA)))
        val fake = library(List(), module = t"posix")

        capture[Lira.Error]:
          Buildpath(List(lib)).validate(t"jvm", contracts = List(fake))
        . reason
      . assert(_ == Lira.Error.Reason.NotHostContract(t"posix"))

      test(m"cross-contract spanning satisfies a requirement"):
        // The requirement names `posix`, whose snapshot no given contract carries; the used-set
        // is contained in a *different* module's atom set, which hosts.md §7 accepts because
        // atoms are content-addressed and module-blind.
        val lib = library(List(Lira.Manifest.Requires(t"posix", snapA, uses = usesHash)))
        val javalib = contractStub(t"scalajs-javalib", List(snapB))

        val atoms = { (module: Text) =>
          if module == t"scalajs-javalib"
          then scala.collection.immutable.Set(t"h1", t"h2")
          else Unset
        }

        val used = { (data: Data) =>
          if Blob.compare(data, usesHash) == 0
          then scala.collection.immutable.Set(t"h1")
          else Unset
        }

        Buildpath(List(lib))
        . validate(t"jvm", contracts = List(javalib), atoms = atoms, used = used)
        . stdlib.size
      . assert(_ == 0)

      test(m"spanning fails where the used-set is not contained"):
        val lib = library(List(Lira.Manifest.Requires(t"posix", snapA, uses = usesHash)))
        val javalib = contractStub(t"scalajs-javalib", List(snapB))

        val atoms = { (module: Text) =>
          if module == t"scalajs-javalib"
          then scala.collection.immutable.Set(t"h2")
          else Unset
        }

        val used = { (data: Data) =>
          if Blob.compare(data, usesHash) == 0
          then scala.collection.immutable.Set(t"h1")
          else Unset
        }

        capture[Lira.Error]:
          Buildpath(List(lib))
          . validate(t"jvm", contracts = List(javalib), atoms = atoms, used = used)
        . reason
      . assert(_ == Lira.Error.Reason.UnsatisfiedRequirement(t"posix"))

    suite(m"Tags"):
      def payloadStub(module: Text): Lira.Manifest.Payload =
        Lira.Manifest.Payload(t"brotli", 0L, Lira.Hash(Lira.Hash.Domain.Blob, encode(module)))

      def release
        ( module: Text, tags: List[Text], payload: Text, version: revolution.Semver )
      :   Lira.Manifest =

        Lira.Manifest(
          module  = module,
          version = version,
          tag     = tags,
          lineage = List(Lira.Hash(Lira.Hash.Domain.Snapshot, encode(payload))),
          api     = List(),
          section = List(Section(t"jvm", tree = blob(encode(t"tree")))),
          payload = Lira.Manifest.Payload(t"brotli", 0L, Lira.Hash(Lira.Hash.Domain.Blob,
              encode(payload))))

      val one = revolution.Semver(0, 1, 0)
      val two = revolution.Semver(0, 2, 0)

      test(m"tags round-trip through the manifest"):
        val bytes = LiraAssembler.assemble(t"jdk",
          List(LiraAssembler.SectionInput(t"jvm", List((TreePath(t"a/A.class"), classA)))),
          Discipline.Registry(List()),
          tag = List(t"jdk-19", t"jdk-19.0.1"),
          toolchain = List(Lira.Manifest.Tool(t"jsig-harvest", t"0.1")))

        Lira.read(bytes).manifest.tag.stdlib
      . assert(_ == scala.List(t"jdk-19", t"jdk-19.0.1"))

      test(m"a tag carried by another release of the module is L142"):
        val earlier = release(t"jdk", List(t"jdk-19"), t"one", one)
        val later = release(t"jdk", List(t"jdk-19"), t"two", two)

        capture[Lira.Error](Buildpath.publishable(later, List(earlier))).reason
      . assert(_ == Lira.Error.Reason.TagReassigned(t"jdk-19"))

      test(m"re-signing the same release may add tags but never drop one"):
        val original = release(t"jdk", List(t"jdk-19"), t"one", one)
        val augmented = release(t"jdk", List(t"jdk-19", t"jdk-19-ga"), t"one", one)
        val stripped = release(t"jdk", List(), t"one", one)

        Buildpath.publishable(augmented, List(original))

        capture[Lira.Error](Buildpath.publishable(stripped, List(original))).reason
      . assert(_ == Lira.Error.Reason.TagReassigned(t"jdk-19"))

      test(m"the same tag on a different module is no clash"):
        val jdk = release(t"jdk", List(t"lts"), t"one", one)
        val android = release(t"android", List(t"lts"), t"two", two)
        Buildpath.publishable(android, List(jdk))
        true
      . assert(identity)

      test(m"a malformed tag fails schema validation"):
        val bytes = LiraAssembler.assemble(t"jdk",
          List(LiraAssembler.SectionInput(t"jvm", List((TreePath(t"a/A.class"), classA)))),
          Discipline.Registry(List()),
          tag = List(t"9uplet"),
          toolchain = List(Lira.Manifest.Tool(t"jsig-harvest", t"0.1")))

        capture[Lira.Error](Lira.read(bytes)).reason match
          case Lira.Error.Reason.InvalidManifest(_) => true
          case _                                   => false
      . assert(identity)
