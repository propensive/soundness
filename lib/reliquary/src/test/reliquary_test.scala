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

import strategies.throwUnsafely
import Tel.given

object Tests extends Suite(m"Reliquary Tests"):
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
