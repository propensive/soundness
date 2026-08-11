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
package xenophile

import anticipation.*
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

// The atomization rules of `dts/1`.
//
// The folding principle (LIRA §10.3) decides the shape: a declaration whose *addition* cannot
// break a consumer stands alone as an atom, and everything whose addition can break one folds
// into the enclosing atom's value, so the whole compatibility check stays set arithmetic.
//
// TypeScript makes that decision sharper than most languages, because a type is both something to
// call and something to implement. Adding a member to an interface cannot break a consumer who
// only *calls* it, and always breaks one who *implements* it — so members are atoms of their own
// (the addition is a minor for callers) and the sorted member-key list also folds into the
// interface's own atom (the same addition is a major for implementors). This mirrors rule 5 of
// `tasty.md` §8, where an open template folds its abstract members for exactly this reason;
// every TypeScript interface is open in that sense, since structural typing needs no declaration
// of intent to implement one.
object DtsAtomizer:
  val id: Text = t"dts/1"

  // --- canonical binary encoding ---------------------------------------------------------------

  private def uvarint(out: java.io.ByteArrayOutputStream, value0: Long): Unit =
    var value = value0

    while value >= 0x80L do
      out.write(((value & 0x7f) | 0x80).toInt)
      value >>>= 7

    out.write(value.toInt)

  private def utf8(out: java.io.ByteArrayOutputStream, text: Text): Unit =
    val bytes = text.s.getBytes("UTF-8").nn
    uvarint(out, bytes.length.toLong)
    out.write(bytes)

  private def tag(out: java.io.ByteArrayOutputStream, char: Char): Unit = out.write(char.toInt)

  private def flag(out: java.io.ByteArrayOutputStream, value: Boolean): Unit =
    out.write(if value then 1 else 0)

  private def hash(encode: java.io.ByteArrayOutputStream => Unit): Data =
    val out = java.io.ByteArrayOutputStream()
    encode(out)
    LiraHash(LiraHash.Domain.Atom(id), Array.unsafeFrozen(out.toByteArray.nn))

  // --- types -----------------------------------------------------------------------------------

  // Type parameters encode as de Bruijn indices, so a binder's *name* never enters a hash:
  // renaming `T` to `U` throughout a declaration changes nothing a consumer can observe, and
  // must therefore change no atom. `binders` holds the enclosing binder scopes, innermost last.
  private def encode
    ( out:     java.io.ByteArrayOutputStream,
      typed:   Typescript.Type,
      binders: scala.List[scala.List[Text]] )
  :   Unit =

    def index(name: Text): Optional[(Int, Int)] =
      val depths = binders.reverse.zipWithIndex

      depths.collectFirst:
        case (names, depth) if names.contains(name) => (depth, names.indexOf(name))

      . getOrElse(Unset)

    typed match
      case Typescript.Type.Named(name, arguments) =>
        index(name) match
          case position: (Int, Int) =>
            tag(out, 'P')
            uvarint(out, position(0).toLong)
            uvarint(out, position(1).toLong)

          case _ =>
            tag(out, 'N')
            utf8(out, name)

        uvarint(out, arguments.stdlib.length.toLong)
        arguments.stdlib.foreach { argument => encode(out, argument, binders) }

      case Typescript.Type.Literal(value, kind) =>
        tag(out, 'L')
        // The kind is folded alongside the value: `"1"` and `1` are different types whose source
        // forms differ only by quoting, which the lexer removed.
        uvarint(out, kind.ordinal.toLong)
        utf8(out, value)

      // Unions and intersections are set-like — `A | B` and `B | A` are the same type — so their
      // members sort. Everything below whose order is semantic keeps declaration order.
      case Typescript.Type.Union(members) =>
        tag(out, '|')
        sorted(out, members, binders)

      case Typescript.Type.Intersection(members) =>
        tag(out, '&')
        sorted(out, members, binders)

      case Typescript.Type.Tuple(members, _) =>
        // A tuple's element order is its meaning; its element *labels* are documentation and
        // never distinguish two types, so they are not folded.
        tag(out, 'T')
        uvarint(out, members.stdlib.length.toLong)
        members.stdlib.foreach { member => encode(out, member, binders) }

      case Typescript.Type.Array(element) =>
        tag(out, 'A')
        encode(out, element, binders)

      case Typescript.Type.Object(members) =>
        tag(out, 'O')
        val ordered = members.stdlib.sortBy { member => member.selector.s }
        uvarint(out, ordered.length.toLong)
        ordered.foreach { member => encodeMember(out, member, binders) }

      case Typescript.Type.Keyof(target) =>
        tag(out, 'K')
        encode(out, target, binders)

      case Typescript.Type.Typeof(target) =>
        tag(out, 'Y')
        utf8(out, target)

      case Typescript.Type.Indexed(target, key) =>
        tag(out, 'X')
        encode(out, target, binders)
        encode(out, key, binders)

      case Typescript.Type.Predicate(parameter, target) =>
        // The narrowed type is the contract; which parameter it narrows is positional, and the
        // parameter's name is not consumer surface.
        tag(out, '?')
        encode(out, target, binders)

      case Typescript.Type.Function(parameters, result, typed, construct) =>
        tag(out, if construct then 'C' else 'F')
        val names = typed.stdlib.map(_.name)
        val inner = binders :+ names

        uvarint(out, typed.stdlib.length.toLong)

        typed.stdlib.foreach: parameter =>
          // A bound and a default are contract: a caller may rely on the default, and a bound
          // decides which arguments are legal.
          parameter.bound.lay(tag(out, '0')) { bound => tag(out, '1'); encode(out, bound, inner) }

          parameter.default.lay(tag(out, '0')): default =>
            tag(out, '1')
            encode(out, default, inner)

        uvarint(out, parameters.stdlib.length.toLong)

        parameters.stdlib.foreach: parameter =>
          // Parameter *names* are not folded — TypeScript call sites are positional — but
          // optionality and rest-ness decide which calls are legal, so both are.
          flag(out, parameter.optional)
          flag(out, parameter.rest)
          parameter.typed.lay(tag(out, '0')) { value => tag(out, '1'); encode(out, value, inner) }

        encode(out, result, inner)

  private def sorted
    ( out:     java.io.ByteArrayOutputStream,
      members: List[Typescript.Type],
      binders: scala.List[scala.List[Text]] )
  :   Unit =

    val encodings = members.stdlib.map: member =>
      val buffer = java.io.ByteArrayOutputStream()
      encode(buffer, member, binders)
      // An immutable view: the sort's comparator would otherwise capture the mutable arrays
      // exclusively, which capture checking rejects.
      val bytes: scala.Array[Byte] = buffer.toByteArray().nn
      // An immutable, unsigned view: the comparator would otherwise capture the mutable arrays
      // exclusively, which capture checking rejects, and signed bytes would sort wrongly.
      scala.Vector.tabulate(bytes.length) { index => bytes(index) & 0xff }

    uvarint(out, encodings.length.toLong)

    encodings.sortWith { (left, right) => compare(left, right) < 0 }.foreach: encoding =>
      uvarint(out, encoding.length.toLong)
      encoding.foreach { byte => out.write(byte) }

  // Sorting by encoded bytes, rather than by rendered text, keeps the order a property of the
  // structure: two types that encode identically must sort adjacently whatever they were called.
  private def compare(left: scala.Vector[Int], right: scala.Vector[Int]): Int =
    val shared = left.length.min(right.length)
    var index = 0
    var result = 0

    while result == 0 && index < shared do
      result = left(index) - right(index)
      index += 1

    if result != 0 then result else left.length - right.length

  private def encodeMember
    ( out:     java.io.ByteArrayOutputStream,
      member:  Typescript.Member,
      binders: scala.List[scala.List[Text]] )
  :   Unit =

    utf8(out, member.selector)
    uvarint(out, member.kind.ordinal.toLong)
    uvarint(out, member.visibility.ordinal.toLong)
    flag(out, member.static)
    flag(out, member.readonly)
    flag(out, member.optional)
    flag(out, member.isAbstract)

    // Overload signatures keep declaration order: TypeScript resolves a call against them in
    // order, so reordering them changes which one a given call selects.
    uvarint(out, member.signatures.stdlib.length.toLong)
    member.signatures.stdlib.foreach { signature => encode(out, signature, binders) }

  // --- atoms -----------------------------------------------------------------------------------

  private def binderNames(typed: List[Typescript.Type.Parameter]): scala.List[Text] =
    typed.stdlib.map(_.name)

  def atomize(declaration: TypescriptDeclaration): List[Atom] =
    val key = declaration.key
    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    val members = declaration.declaredMembers.stdlib.filter(_.visible)
    val binders = scala.List(binderNames(typeParameters(declaration)))

    // Members are atoms of their own, so adding one is a minor for a consumer who only calls it.
    members.foreach: member =>
      val encoding = hash: out =>
        tag(out, 'M')
        utf8(out, key)
        encodeMember(out, member, binders)

      atoms += Atom(t"$key#${member.selector}", AtomClass.Rigid, encoding)

    val encoding = hash: out =>
      declaration match
        case TypescriptDeclaration.Interface(_, _, typed, extending, _, _) =>
          tag(out, 'I')
          utf8(out, key)
          parameters(out, typed, binders)
          // Heritage keeps declaration order, which decides how conflicting inherited members
          // resolve.
          uvarint(out, extending.stdlib.length.toLong)
          extending.stdlib.foreach { base => encode(out, base, binders) }

        case TypescriptDeclaration.Class(_, _, typed, extending, implements, _, isAbstract, _) =>
          tag(out, 'C')
          utf8(out, key)
          parameters(out, typed, binders)
          flag(out, isAbstract)
          extending.lay(tag(out, '0')) { base => tag(out, '1'); encode(out, base, binders) }
          uvarint(out, implements.stdlib.length.toLong)
          implements.stdlib.foreach { base => encode(out, base, binders) }

        case TypescriptDeclaration.Alias(_, _, typed, target, _) =>
          tag(out, 'A')
          utf8(out, key)
          parameters(out, typed, binders)
          encode(out, target, binders)

        case TypescriptDeclaration.Enumeration(_, _, members, constant, _) =>
          tag(out, 'E')
          utf8(out, key)
          // A `const enum` is inlined into consumers at *their* compile time, so its values are
          // a materially stronger commitment than an ordinary enum's; the flag folds so that a
          // change of kind is never invisible.
          flag(out, constant)
          uvarint(out, members.stdlib.length.toLong)

          members.stdlib.foreach: (name, value) =>
            utf8(out, name)
            value.lay(tag(out, '0')) { text => tag(out, '1'); utf8(out, text) }

        case TypescriptDeclaration.Function(_, _, signatures, _) =>
          tag(out, 'F')
          utf8(out, key)
          uvarint(out, signatures.stdlib.length.toLong)
          signatures.stdlib.foreach { signature => encode(out, signature, binders) }

        case TypescriptDeclaration.Variable(_, _, typed, constant, _) =>
          tag(out, 'V')
          utf8(out, key)
          flag(out, constant)
          typed.lay(tag(out, '0')) { value => tag(out, '1'); encode(out, value, binders) }

      // The sorted member-key list, folded into the declaration's own atom. This is what makes
      // adding a member a *major* for implementors while it remains a minor for callers — the
      // one place where a single change is honestly two different events.
      val keys = members.map(_.selector).sortBy(_.s)
      uvarint(out, keys.length.toLong)
      keys.foreach { selector => utf8(out, selector) }

    atoms += Atom(key, AtomClass.Rigid, encoding)

    List.from(atoms.toList)

  private def parameters
    ( out:     java.io.ByteArrayOutputStream,
      typed:   List[Typescript.Type.Parameter],
      binders: scala.List[scala.List[Text]] )
  :   Unit =

    uvarint(out, typed.stdlib.length.toLong)

    typed.stdlib.foreach: parameter =>
      parameter.bound.lay(tag(out, '0')) { bound => tag(out, '1'); encode(out, bound, binders) }
      parameter.default.lay(tag(out, '0')) { value => tag(out, '1'); encode(out, value, binders) }

  private def typeParameters(declaration: TypescriptDeclaration): List[Typescript.Type.Parameter] =
    declaration match
      case TypescriptDeclaration.Interface(_, _, typed, _, _, _)   => typed
      case TypescriptDeclaration.Class(_, _, typed, _, _, _, _, _) => typed
      case TypescriptDeclaration.Alias(_, _, typed, _, _)          => typed
      case _                                                       => Nil
