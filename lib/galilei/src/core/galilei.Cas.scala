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
package galilei

import anticipation.*
import contingency.*
import corpuscular.*
import denominative.*
import distillate.*
import fulminate.*
import gastronomy.*
import gigantism.Every
import gossamer.*
import inimitable.*
import monotonous.*
import murmuration.{Inclusive, Traversable}
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

import alphabets.hexLowerCase
import denominative.dysasymptotics.linearSize

// A content-addressed store: one blob per digest of its bytes, kept under one root, so a blob
// is written once, verified as it is written, and found by anything that knows its digest.
// The algorithm is in the store's type (`Cas in Sha2[256]`), as it is in every digest it
// accepts (`Digest in Sha2[256]`), and the storage is the type of its root (`on (Path on
// Local)`), chosen when the store is made through a `Cas.Storage` for that type.
object Cas:
  object Storage:
    // A directory on any platform with a backend. Blob names are hexadecimal plus a suffix, so
    // they are well-formed on every plane and need no checking.
    given path: [plane <: Platform: Filesystem]
    =>  ( backend: FilesystemBackend on plane, fdtables: Every[Fdtable] )
    =>  (Path on plane) is Storage =

      new Storage:
        type Self = Path on plane

        private def child(root: Path on plane, name: Text): Path on plane =
          root.unsafeChild(name)(using Unsafe)

        def has(root: Path on plane, name: Text): Boolean = backend.exists(child(root, name), true)

        def read(root: Path on plane, name: Text): Data raises Io.Error =
          child(root, name).read[Data]

        // Written to a sibling with a unique name, then moved into place atomically, as
        // `create[File]` does; a failure leaves nothing behind.
        def replace(root: Path on plane, name: Text, data: Data): Unit raises Io.Error =
          val temporary: Path on plane = child(root, t".$name.${Uuid().show}.part")

          try
            temporary.write(data)
            backend.move(temporary, child(root, name), true, false)
          catch case throwable: Throwable =>
            safely(backend.deleteIfExists(temporary))
            throw throwable

        def names(root: Path on plane): List[Text] raises Io.Error =
          backend.children(root).to[List]

        def delete(root: Path on plane, name: Text): Unit raises Io.Error =
          backend.deleteIfExists(child(root, name))

  // How blobs are kept at a root of type `Self`: tested, read, replaced atomically (a reader
  // never sees a partial blob), listed by name and deleted. A root is a filesystem directory
  // today; another storage provides another instance.
  trait Storage extends Typeclass:
    def has(root: Self, name: Text): Boolean
    def read(root: Self, name: Text): Data raises Io.Error
    def replace(root: Self, name: Text, data: Data): Unit raises Io.Error
    def names(root: Self): List[Text] raises Io.Error
    def delete(root: Self, name: Text): Unit raises Io.Error

  case class Error(expected: Text, actual: Text)(using Diagnostics)
  extends fulminate.Error(m"the data's digest, $actual, is not $expected")

  // The store at `root`, whose blobs are named `<digest><suffix>`. The hash and its permit are
  // checked here, once, for every digest the store will compute.
  def apply[algorithm <: Algorithm]
    (using hash: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]])[locus]
    (root: locus, suffix: Text = t"")(using storage: locus is Storage)
  :   Cas in algorithm on locus =

    val root0 = root
    val suffix0 = suffix
    val storage0 = storage
    val hash0 = hash

    new Cas:
      type Form = algorithm
      type Plane = locus
      val root: Plane = root0
      val suffix: Text = suffix0
      protected val storage: Plane is Storage = storage0
      protected val hash: Hash in Form = hash0

  // As a collection, a store is its digests. The typeclasses are pure, so these views cannot
  // report an I/O failure: a store that cannot be listed or read looks empty through them, and
  // the methods are where a failure is raised.
  // `Self` is matched exactly, and a store's type carries its storage too, so each instance is
  // for any subtype of `Cas in algorithm`.
  given countable: [cas <: Cas] => cas is Countable:
    def size(self: cas): Int = safely(self.digests).or(Nil).size

  given traversable: [algorithm <: Algorithm, cas <: Cas in algorithm]
  =>  cas is Traversable by (Digest in algorithm) =
    cas => Iterator.from(List.iterator(safely(cas.digests).or(Nil)))

  given inclusive: [algorithm <: Algorithm, cas <: Cas in algorithm]
  =>  cas is Inclusive by (Digest in algorithm) =
    _.has(_)

  given applicable: [algorithm <: Algorithm, cas <: Cas in algorithm] => cas is Applicable:
    type Self = cas
    type Operand = Digest in algorithm
    type Result = Data
    def contains(value: Self, index: Operand): Boolean = value.has(index)

    // `at` reads only after `contains`, so a failure here is a blob that vanished or could not
    // be read in between, which no `Optional` can express.
    def access(value: Self, index: Operand): Data =
      safely(value(index)).or(panic(m"the blob ${index.serialize[Hex]} could not be read"))

trait Cas extends Formal, Planar:
  type Form <: Algorithm
  type Plane

  val root: Plane
  val suffix: Text
  protected val storage: Plane is Cas.Storage
  protected val hash: Hash in Form

  // The blob's name under the root: lowercase hexadecimal, then the suffix.
  def name(digest: Digest in Form): Text = t"${digest.serialize[Hex]}$suffix"

  def digest(data: Data): Digest in Form = Digester(_ => ()).digest(data).apply[Form](using hash)
  def has(digest: Digest in Form): Boolean = storage.has(root, name(digest))
  def missing(digests: List[Digest in Form]): List[Digest in Form] = digests.filter(!has(_))

  def apply(digest: Digest in Form)(using Tactic[Io.Error]): Optional[Data] =
    if has(digest) then storage.read(root, name(digest)) else Unset

  // `cas() = data`: stores the data under its own digest, unless it is there already.
  def update(data: Data)(using Tactic[Io.Error]): Digest in Form =
    val digest: Digest in Form = this.digest(data)
    if !has(digest) then storage.replace(root, name(digest), data)
    digest

  // `cas(digest) = data`: the same, checking first that the data really has the digest.
  def update(expected: Digest in Form, data: Data)(using Tactic[Io.Error], Tactic[Cas.Error])
  :   Unit =

    val actual: Digest in Form = digest(data)

    if actual != expected then abort(Cas.Error(expected.serialize[Hex], actual.serialize[Hex]))
    else if !has(expected) then storage.replace(root, name(expected), data)

  // Every digest stored: the names carrying the suffix whose remainder is hexadecimal.
  def digests(using Tactic[Io.Error]): List[Digest in Form] =
    storage.names(root).bind: name =>
      if name.ends(suffix)
      then safely(name.keep(name.length - suffix.length).as[Digest in Form]).let(List(_)).or(Nil)
      else Nil

  // Deletes every blob whose digest is not in `keep`; how many were deleted.
  def prune(keep: Set[Digest in Form])(using Tactic[Io.Error]): Int =
    val stale: List[Digest in Form] = digests.filter(!keep.has(_))
    stale.each: digest => storage.delete(root, name(digest))
    stale.size
