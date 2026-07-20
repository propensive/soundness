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
package aperture

import scala.caps

import soundness.*

import proscenium.compat.*

// Openable instances for testing: an in-memory "document" openable in two different forms
// from the same target type (to exercise form selection and its ambiguity), and in a unique
// form from another (to exercise form inference).
trait Doc
trait Bin

case class Ref(name: Text)
case class Sole(name: Text)

enum TestFlag:
  case Fast, Careful

class DocHandle(val name: Text, val flags: List[TestFlag]) extends caps.ExclusiveCapability:
  private var appended: List[Text] = Nil
  def titleOf: Text = t"doc:$name"
  def appendedText: Text = appended.stdlib.reverse.join

extension (handle: (DocHandle & Granting[Grant.Read])^)
  def title: Text = handle.titleOf

extension (handle: (DocHandle & Granting[Grant.Write])^)
  def append(text: Text): Unit = ()

// Named classes rather than anonymous given instances: instantiating an anonymous subclass
// freshens the handle's capability type in the inferred `Result` member (see the equivalent
// comment on galilei's `FileOpenable`).
class DocOpenable extends Openable:
  type Self = Ref
  type Form = Doc
  type Operand = TestFlag
  type Result = DocHandle

  def open[grants <: Grant, result]
    ( value: Ref, mode: Mode granting grants, flags: List[TestFlag] )
    ( block: ((DocHandle & Granting[grants])^) ?=> result )
  :   result =

    block(using new DocHandle(value.name, flags) with Granting[grants] {})

class BinOpenable extends Openable:
  type Self = Ref
  type Form = Bin
  type Operand = TestFlag
  type Result = DocHandle

  def open[grants <: Grant, result]
    ( value: Ref, mode: Mode granting grants, flags: List[TestFlag] )
    ( block: ((DocHandle & Granting[grants])^) ?=> result )
  :   result =

    block(using new DocHandle(t"bin:"+value.name, flags) with Granting[grants] {})

class SoleOpenable extends Openable:
  type Self = Sole
  type Form = Doc
  type Operand = TestFlag
  type Result = DocHandle

  def open[grants <: Grant, result]
    ( value: Sole, mode: Mode granting grants, flags: List[TestFlag] )
    ( block: ((DocHandle & Granting[grants])^) ?=> result )
  :   result =

    block(using new DocHandle(value.name, flags) with Granting[grants] {})

given docOpenable: DocOpenable = DocOpenable()
given binOpenable: BinOpenable = BinOpenable()
given soleOpenable: SoleOpenable = SoleOpenable()

// A creatable counterpart: an in-memory "vault" of documents, committed on scope close so
// that rollback semantics are observable.
class Vault:
  var committed: Optional[List[Text]] = Unset
  var made: Boolean = false

case class VaultRef(vault: Vault)

class VaultScribe private[aperture] (var lines: List[Text])
extends caps.ExclusiveCapability:
  def append(line: Text): Unit = lines = line :: lines

class VaultCreatable extends Creatable:
  type Self = VaultRef
  type Form = Doc
  type Operand = TestFlag
  type Grants = Grant.Read & Grant.Write
  type Result = VaultScribe

  override def make(value: VaultRef, flags: List[TestFlag]): Unit =
    value.vault.made = true
    value.vault.committed = Nil

  def create[result]
    ( value: VaultRef, flags: List[TestFlag] )
    ( block: ((VaultScribe & Granting[Grant.Read & Grant.Write])^) ?=> result )
  :   result =

    val scribe = new VaultScribe(Nil) with Granting[Grant.Read & Grant.Write] {}
    val outcome = block(using scribe)
    value.vault.committed = scribe.lines.reverse
    outcome

given vaultCreatable: VaultCreatable = VaultCreatable()

object Tests extends Suite(m"Aperture Tests"):
  def run(): Unit =
    test(m"An entity opens readably by default"):
      Ref(t"alpha").open[Doc]() { handle ?=> handle.title }
    . assert(_ == t"doc:alpha")

    test(m"An explicit form selects between instances"):
      Ref(t"alpha").open[Bin]() { handle ?=> handle.title }
    . assert(_ == t"doc:bin:alpha")

    test(m"The form is inferred when the target has a unique instance"):
      Sole(t"beta").open() { handle ?=> handle.title }
    . assert(_ == t"doc:beta")

    test(m"A mode of Read & Write permits both kinds of operation"):
      Ref(t"gamma").open[Doc](Read & Write): handle ?=>
        handle.append(t"more")
        handle.title
    . assert(_ == t"doc:gamma")

    test(m"Flags are passed through to the instance"):
      Ref(t"delta").open[Doc](TestFlag.Fast, TestFlag.Careful) { handle ?=> handle.flags }
    . assert(_ == List(TestFlag.Fast, TestFlag.Careful))

    test(m"Flags may follow an explicit mode"):
      Ref(t"epsilon").open[Doc](Read & Write, TestFlag.Fast) { handle ?=> handle.flags }
    . assert(_ == List(TestFlag.Fast))

    test(m"A write operation without the Write grant does not compile"):
      demilitarize:
        Ref(t"zeta").open[Doc]() { handle ?=> handle.append(t"nope") }
      . map(_.message)
    . assert(_.nonEmpty)

    test(m"An ambiguous form does not compile"):
      demilitarize:
        Ref(t"eta").open() { handle ?=> handle.title }
      . map(_.message)
    . assert(_.nonEmpty)

    test(m"Instantiation creates an empty artifact and returns the target"):
      val vault = Vault()
      val target = VaultRef(vault).create()
      (target.vault.made, vault.committed.vouch)
    . assert(_ == (true, Nil))

    test(m"Scoped authoring commits at scope close"):
      val vault = Vault()

      VaultRef(vault).create(): scribe ?=>
        scribe.append(t"first")
        scribe.append(t"second")

      vault.committed.vouch
    . assert(_ == List(t"first", t"second"))

    test(m"An exception escaping the scope commits nothing"):
      val vault = Vault()

      try
        VaultRef(vault).create(): scribe ?=>
          scribe.append(t"doomed")
          throw new RuntimeException("boom")
      catch case _: RuntimeException => ()

      vault.committed
    . assert(_ == Unset)

    test(m"The scoped result is returned from the authoring block"):
      val vault = Vault()

      VaultRef(vault).create(): scribe ?=>
        scribe.append(t"only")
        t"result"
    . assert(_ == t"result")

    test(m"A composite mode's atoms are its constituent atomic modes"):
      (Read & Write & Exclusive).atoms
    . assert(_ == Set(Read, Write, Exclusive))

    test(m"An atomic mode's atoms are itself"):
      Write.atoms
    . assert(_ == Set(Write))

    test(m"A read operation with only the Write grant does not compile"):
      demilitarize:
        Ref(t"theta").open[Doc](Write) { handle ?=> handle.title }
      . map(_.message)
    . assert(_.nonEmpty)
