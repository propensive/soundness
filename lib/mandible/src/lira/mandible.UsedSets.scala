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
package mandible

import java.lang.classfile as jlc
import java.lang.classfile.constantpool as jlcc

import scala.collection.immutable.SortedSet
import scala.jdk.CollectionConverters.*

import anticipation.*
import contingency.*
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

// The used-set extractor (LIRA §13.4, hosts.md §7): what a body of compiled classes actually
// depends on, as membership keys, and — resolved against a contract's atom listing — as the
// Uses blob a `requires` or `dependency` record attaches.
//
// A compiled consumer's dependencies are exactly its constant pool's outward references: every
// `Class`, `Methodref`, `Fieldref` and `InterfaceMethodref` entry the JVM will resolve, spelled
// by receiver — which is precisely membership-key spelling (`classfile.md` §6), so a reference
// and the atom it uses agree by construction. References to classes the content itself declares
// are internal and excluded.
//
// Resolution is against the listing of the contract release the module was *compiled against*
// (`tasty.md` §12's rule, transposed): a matched key contributes its atom's value hash to the
// used-set, and `used ⊆ atoms(H)` then decides — for any release `H` of any contract under the
// same discipline — whether that host satisfies the module, by set inclusion (hosts.md §7).
// Unmatched keys are returned rather than dropped: they are references to *other* modules
// (buildpath dependencies, whose own used-sets cover their platform use transitively), and a
// caller expecting the contract to cover everything can see at once what it did not.
//
// One boundary is worth stating: a platform member reached *through* a non-platform receiver —
// `myWidget.hashCode()`, whose constant-pool owner is the widget class — is the widget module's
// use, not this one's, and lands in its used-set when that module is published. Direct platform
// references, which any real consumer of a platform also makes, are what this records.
object UsedSets:

  // The outward reference keys of the given classfiles, sorted: method and field references as
  // `<owner>#<name>:<descriptor>` and `<owner>.<name>:<descriptor>`, referenced classes by
  // internal name. Array references contribute their element class; primitives contribute
  // nothing; references into the content itself are excluded.
  def references(content: List[(TreePath, Data)]): List[Text] =
    val own = scala.collection.mutable.HashSet[String]()
    val found = scala.collection.mutable.SortedSet[String]()
    val models = scala.collection.mutable.ListBuffer[jlc.ClassModel]()

    content.stdlib.foreach: (path, data) =>
      if path.text.s.endsWith(".class") || path.text.s.endsWith(".sig") then
        val model = jlc.ClassFile.of().nn.parse(Array.unsafeJvm(data)).nn
        models += model
        own += model.thisClass.nn.asInternalName.nn

    def classOf(internal: String): Optional[String] =
      val element = internal.dropWhile(_ == '[')

      if element.length == internal.length then internal
      else if element.startsWith("L") && element.endsWith(";")
      then element.substring(1, element.length - 1).nn
      else Unset

    models.foreach: model =>
      model.constantPool.nn.asScala.foreach:
        case entry: jlcc.ClassEntry =>
          classOf(entry.asInternalName.nn).let: name =>
            if !own.contains(name) then found += name

        case entry: jlcc.MemberRefEntry =>
          val owner = entry.owner.nn.asInternalName.nn

          if classOf(owner).let { name => !own.contains(name) }.or(false) then
            val name = entry.name.nn.stringValue.nn
            val descriptor = entry.`type`.nn.stringValue.nn

            val separator = entry match
              case _: jlcc.FieldRefEntry => "."
              case _                     => "#"

            found += s"$owner$separator$name:$descriptor"

        case _ => ()

    List.from(found.toList.map(_.tt))

  // Splits reference keys against a contract's atom listing: the value hashes of the matched
  // keys — the used-set — and the keys the listing does not carry.
  def resolve(references: List[Text], listing: Atomization): (List[Data], List[Text]) =
    val byKey: scala.collection.immutable.Map[Text, Data] =
      listing.atoms.stdlib.map { atom => atom.key -> atom.valueHash }.toMap

    val matched = scala.collection.mutable.ListBuffer[Data]()
    val unmatched = scala.collection.mutable.ListBuffer[Text]()

    references.stdlib.foreach: key =>
      byKey.get(key) match
        case scala.Some(hash) => matched += hash
        case _                => unmatched += key

    (List.from(matched.toList), List.from(unmatched.toList))

  // The Uses metadata blob for one module's use of one contract (§13.4, §14): the resolved
  // used-set, encoded for a `requires` or `dependency` record's `uses` field, with the
  // unmatched keys alongside for the caller's judgement.
  def uses(module: Text, content: List[(TreePath, Data)], listing: Atomization)
  :   (Data, List[Text]) =

    val (matched, unmatched) = resolve(references(content), listing)
    (UsesBlob.encode(module, matched), unmatched)
