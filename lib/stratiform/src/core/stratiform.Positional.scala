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
package stratiform

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import rudiments.*
import vacuous.*

// The schema-free §19.2/§20.2 atom phase shared by both derivation engines
// (issue #1694). A derived product codec approximates the schema's member
// classification from Scala types (`Tel.Nature`) and runs the same positional
// assignment `Tel.Type.assign` performs against a real schema: atoms fill
// members in declaration order, skipping only non-required members that
// cannot take the atom.
private[stratiform] object Positional:

  // One field of a derived product, as the atom phase sees it. `required`
  // is false for `Optional` fields and fields with a declared default,
  // matching §20's effective-polarity model.
  case class Profile
    ( keyword:    Text,
      nature:     Tel.Nature,
      repeatable: Boolean,
      required:   Boolean )

  // What a repeatable field of a derived product contributes to the record which holds it,
  // from the document its collection encodes to. They are plain methods, and not part of the
  // derivation's inline body, since an array which is read within the expansion, in a
  // capture-checked module, is taken afresh for one which might hold something, and so
  // cannot be traversed.
  //
  // A repeatable scalar field: all occurrences inline or none (§22.2). A single occurrence
  // still terminates the run -- a repeatable member holds its atom position and would consume
  // any atom that followed -- so it takes the child form. An empty collection (or an unset
  // `Optional`) contributes nothing but breaks the run, exactly like an absent scalar member.
  def scalars(document: Tel.Document, keyword: Text): Tel.Mutation.Member =
    val children = document.children.bind(_.compounds)

    if children.length == 0 then Tel.Mutation.Member.Break
    else if children.length == 1 then
      children.at(Prim).lay(Tel.Mutation.Member.Break): child =>
        Tel.Mutation.Member.Child(child.copy(keyword = keyword))
    else
      val texts = scala.collection.mutable.ListBuffer.empty[Text]
      children.each: child => texts += child.atoms.at(Prim).lay("".tt)(text(_))
      Tel.Mutation.Member.Value(keyword, texts.to(List))

  // The compounds of a collection's document, each under the keyword of the field which holds
  // the collection: a repeated field is its keyword repeated.
  def rekeyed(document: Tel.Document, keyword: Text)
  :   scala.collection.immutable.List[Tel.Compound] =

    val compounds = scala.collection.mutable.ListBuffer.empty[Tel.Compound]

    document.children.bind(_.compounds).each: compound =>
      compounds += compound.copy(keyword = keyword)

    compounds.toList

  // A repeatable struct field: each occurrence as a child, or a break if there are none.
  def structs(document: Tel.Document, keyword: Text)
  :   scala.collection.immutable.List[Tel.Mutation.Member] =

    val children = document.children.bind(_.compounds)

    if children.length == 0 then scala.collection.immutable.List(Tel.Mutation.Member.Break) else
      val members = scala.collection.mutable.ListBuffer.empty[Tel.Mutation.Member]
      children.each: child => members += Tel.Mutation.Member.Child(child.copy(keyword = keyword))
      members.toList

  def text(atom: Tel.Atom): Text = atom match
    case Tel.Atom.Inline(value, _)  => value
    case Tel.Atom.Source(value)     => value
    case Tel.Atom.Literal(_, value) => value

  // §20.2 step 3 over a compound line's atoms (all three presentation
  // forms): returns, per member, the atoms assigned to it, in order. Errors
  // raise-and-continue through the ambient tactic, mirroring
  // `Tel.Type.assignAtoms`: an unassignable atom is dropped and later atoms
  // still assign.
  def assign(atoms: Array[Tel.Atom]^{}, profiles: Array[Profile]^{})
    ( using Tactic[Tel.Error] )
  :   Array[List[Tel.Atom]]^{} =

    val assigned = new scala.Array[List[Tel.Atom]](profiles.length)
    var slot = 0

    while slot < profiles.length do
      assigned(slot) = Nil
      slot += 1

    var position = 0
    var index = 0

    while index < atoms.length do
      val value = text(atoms.readUnchecked(index))

      // Step 3a: advance past non-required members that cannot take this
      // atom — Struct-natured ones, and Flag-natured ones whose keyword the
      // atom's text does not match. A Scalar member is never skipped; a
      // required member is never skipped.
      var scanning = true

      while scanning && position < profiles.length do
        val profile = profiles.readUnchecked(position)

        val skippable =
          !profile.required &&
            (profile.nature == Tel.Nature.Struct ||
              (profile.nature == Tel.Nature.Flag && value != profile.keyword))

        if skippable then position += 1 else scanning = false

      if position >= profiles.length then
        // Step 3b: more atoms than assignable member positions (E302).
        // Recovery drops the whole excess run, reported once.
        raise(Tel.Error(Tel.Error.Reason.TooManyAtoms))
        index = atoms.length
      else
        val profile = profiles.readUnchecked(position)

        profile.nature match
          case Tel.Nature.Struct =>
            // Step 3c: an atom at a required member only fillable by a
            // keyword child (E303). The atom is dropped.
            raise(Tel.Error(Tel.Error.Reason.AtomAtNonAssignablePos))

          case Tel.Nature.Flag =>
            if value == profile.keyword then
              assigned(position) = assigned(position) :+ atoms.readUnchecked(index)
              if !profile.repeatable then position += 1
            else
              // Step 3d: a required Flag member's atom must match its
              // keyword (E305). The atom is dropped.
              raise(Tel.Error(Tel.Error.Reason.AtomFlagKeywordMismatch))

          case Tel.Nature.Scalar =>
            assigned(position) = assigned(position) :+ atoms.readUnchecked(index)
            // Step 3e: a repeatable member holds its position and consumes
            // every remaining atom.
            if !profile.repeatable then position += 1

        index += 1

    assigned.asInstanceOf[Array[List[Tel.Atom]]^{}]
