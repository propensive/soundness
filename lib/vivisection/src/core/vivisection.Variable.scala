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
package vivisection

import anticipation.*
import gossamer.*
import proscenium.*
import spectacular.*
import vacuous.*

object Variable:
  // Where a variable's storage was found. The JVM flattens a source-level scope into slots and
  // fields; the provenance records the storage each logical variable was recovered from, so that
  // an assignment later knows where to write.
  enum Provenance:
    case Local(slot: Int)
    case Captured(fieldPath: List[Text])
    case Field(owner: Text)
    case Element(index: Int)
    case Cell(inner: Provenance)

  // Whether a lazy binding has been evaluated. An unforced lazy val is *never* forced by the
  // debugger — forcing would change the program under observation — so its value is absent and
  // it renders as `∿∿∿`, the notation for unforced state.
  enum State:
    case Forced
    case Unforced

  object Snapshot:
    // Remote identities render as the class name (or the array's component letter) followed by a
    // fullwidth `＠` and the JDWP object identifier — echoing Java's `Foo@1a2b` closely enough to
    // read instantly as an object identity, while staying distinct from any literal rendering.
    // Parameterised over subtypes because `Inspectable`'s `Self` is invariant (see `Jdwp.Value`).
    given inspectable: [snapshot <: Snapshot] => snapshot is Inspectable = render(_)

    private def render(snapshot: Snapshot): Text = snapshot match
      case Primitive(value) =>
        value.inspect

      case Str(_, text) =>
        text.inspect

      case Arr(id, component, length, prefix) =>
        val items = prefix.stdlib.zipWithIndex.map: (element, index) =>
          val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
          subscript+render(element).s

        val ellipsis = if length > prefix.stdlib.length then "…" else ""

        items.mkString("⦋"+letter(component), "∣", ellipsis+"⦌＠"+id.long).tt

      case Obj(id, cls) =>
        val simple = cls.s.substring(cls.s.lastIndexOf('.') + 1).nn
        (simple+"＠"+id.long).tt

      case Null =>
        t"null"

    private def letter(tag: Jdwp.Tag): String = tag match
      case Jdwp.Tag.ByteTag    => "🅱"
      case Jdwp.Tag.CharTag    => "🅲"
      case Jdwp.Tag.DoubleTag  => "🅳"
      case Jdwp.Tag.FloatTag   => "🅵"
      case Jdwp.Tag.IntTag     => "🅸"
      case Jdwp.Tag.LongTag    => "🅹"
      case Jdwp.Tag.ShortTag   => "🆂"
      case Jdwp.Tag.BooleanTag => "🆉"
      case _                   => "🅻"

  // The decoded, debugger-side view of a remote value: primitives arrive whole, strings and a
  // bounded prefix of each array are fetched eagerly, and any other object stays a summary for
  // on-demand expansion.
  enum Snapshot:
    case Primitive(value: Jdwp.Value)
    case Str(id: ObjectId, text: Text)
    case Arr(id: ObjectId, component: Jdwp.Tag, length: Int, prefix: List[Snapshot])
    case Obj(id: ObjectId, cls: Text)
    case Null

  given inspectable: Variable is Inspectable = variable =>
    val value = variable.state match
      case State.Unforced => t"∿∿∿"
      case State.Forced   => variable.value.let(_.inspect).or(t"○")

    t"${variable.name}:$value"

  // Maps a JVM type signature to the wire tag with which to request a value of that type. Every
  // reference signature requests with the generic object tag; the VM's reply carries the precise
  // tag.
  private[vivisection] def tag(signature: Text): Jdwp.Tag = Jdwp.Tag(signature.s.charAt(0))

  // Renders a JVM type signature as the Scala type it erased from, as far as the signature alone
  // can say: primitives by name, `Lscala/collection/List;` as `scala.collection.List`, arrays
  // recursively. A module class's trailing `$` is dropped and inner-class `$`s read as dots.
  private[vivisection] def demangle(signature: Text): Text = signature.s.charAt(0) match
    case 'B' => t"Byte"
    case 'C' => t"Char"
    case 'D' => t"Double"
    case 'F' => t"Float"
    case 'I' => t"Int"
    case 'J' => t"Long"
    case 'S' => t"Short"
    case 'Z' => t"Boolean"
    case 'V' => t"Unit"
    case '[' => t"Array[${demangle(signature.s.substring(1).nn.tt)}]"

    case 'L' =>
      val name = signature.s.substring(1, signature.s.length - 1).nn
      val stripped = if name.endsWith("$") then name.substring(0, name.length - 1).nn else name
      stripped.replace('/', '.').nn.replace('$', '.').nn.tt

    case _ =>
      signature

  // The name a wire tag implies, for values whose signature is not to hand (array elements).
  private[vivisection] def tagName(tag: Jdwp.Tag): Text = tag match
    case Jdwp.Tag.ByteTag    => t"Byte"
    case Jdwp.Tag.CharTag    => t"Char"
    case Jdwp.Tag.DoubleTag  => t"Double"
    case Jdwp.Tag.FloatTag   => t"Float"
    case Jdwp.Tag.IntTag     => t"Int"
    case Jdwp.Tag.LongTag    => t"Long"
    case Jdwp.Tag.ShortTag   => t"Short"
    case Jdwp.Tag.BooleanTag => t"Boolean"
    case Jdwp.Tag.VoidTag    => t"Unit"
    case Jdwp.Tag.StringTag  => t"String"
    case Jdwp.Tag.ArrayTag   => t"Array"
    case _                   => t"Object"

  // Recovers the written name from a compiler-mangled capture field: `x$3` was `x`. `Unset` for
  // any name which does not carry a purely numeric suffix.
  private[vivisection] def captured(name: Text): Optional[Text] =
    val string = name.s
    val dollar = string.lastIndexOf('$')

    if dollar <= 0 then Unset else
      val suffix = string.substring(dollar + 1).nn

      if suffix.length > 0 && suffix.forall(_.isDigit) then string.substring(0, dollar).nn.tt
      else Unset

  // Recovers the written name from a lazy val's backing field: `x$lzy2` was `x`.
  private[vivisection] def lazyField(name: Text): Optional[Text] =
    val string = name.s
    val index = string.lastIndexOf("$lzy")

    if index <= 0 then Unset else
      val suffix = string.substring(index + 4).nn

      if suffix.length > 0 && suffix.forall(_.isDigit) then string.substring(0, index).nn.tt
      else Unset

// A logical variable visible at a suspended frame: the name and form the programmer wrote,
// recovered from the JVM's storage form before anything downstream sees it — captures
// un-flattened from their fields, ref cells unboxed, lazy sentinels read but never forced.
// `static` is the variable's declared Scala type, when TASTy resolution (a later phase) has
// supplied it; `erased` is always available from the JVM signature.
case class Variable
  ( name:       Text,
    static:     Optional[Text],
    erased:     Text,
    value:      Optional[Variable.Snapshot],
    provenance: Variable.Provenance,
    mutable:    Boolean,
    state:      Variable.State )
