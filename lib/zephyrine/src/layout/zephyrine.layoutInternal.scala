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
package zephyrine

import scala.quoted.*

import anticipation.*
import gigantism.*
import hypotenuse.*
import proscenium.*
import rudiments.*

object layoutInternal:
  // Stores of a fixed-width integer at a position in a mutable array, in each byte order: the
  // counterparts of the loads on `Data`.
  def store8(target: scala.Array[Byte]^, offset: Int, value: Byte): Unit = target(offset) = value

  def store16(target: scala.Array[Byte]^, offset: Int, value: Short, big: Boolean): Unit =
    val high = if big then offset else offset + 1
    target(high) = (value >> 8).toByte
    target(2*offset + 1 - high) = value.toByte

  def store32(target: scala.Array[Byte]^, offset: Int, value: Int, big: Boolean): Unit =
    val high = if big then offset else offset + 2
    store16(target, high, (value >> 16).toShort, big)
    store16(target, 2*offset + 2 - high, value.toShort, big)

  def store64(target: scala.Array[Byte]^, offset: Int, value: Long, big: Boolean): Unit =
    val high = if big then offset else offset + 4
    store32(target, high, (value >> 32).toInt, big)
    store32(target, 2*offset + 4 - high, value.toInt, big)

  def derive[record <: Product: Type](big0: Expr[Boolean]): Macro[record is Structural] =
    import quotes.reflect.*

    val big: Boolean = big0.valueOrAbort
    val bigEndian: Expr[Boolean] = Expr(big)
    val record = TypeRepr.of[record]
    val symbol = record.typeSymbol

    if !symbol.isClassDef || !symbol.flags.is(Flags.Case) then
      report.errorAndAbort(s"a layout can be derived only for a case class, not ${record.show}")

    type Target = scala.Array[Byte]^

    // What is known of one field: its width in bytes if it is a sized integer, or the instance
    // its type defers to if it is not.
    case class Field(name: String, fieldType: TypeRepr, width: Int, instance: Option[Term])

    def is[value: Type](field: Field): Boolean = field.fieldType =:= TypeRepr.of[value]

    def field(name: String, fieldType: TypeRepr): Field =
      def sized(width: Int): Field = Field(name, fieldType, width, None)
      def any(types: TypeRepr*): Boolean = types.exists(fieldType =:= _)

      if any(TypeRepr.of[Byte], TypeRepr.of[B8], TypeRepr.of[U8], TypeRepr.of[S8]) then sized(1)
      else if any(TypeRepr.of[Short], TypeRepr.of[B16], TypeRepr.of[U16], TypeRepr.of[S16])
      then sized(2)
      else if any(TypeRepr.of[Int], TypeRepr.of[B32], TypeRepr.of[U32], TypeRepr.of[S32])
      then sized(4)
      else if any(TypeRepr.of[Long], TypeRepr.of[B64], TypeRepr.of[U64], TypeRepr.of[S64])
      then sized(8)
      else fieldType.asType.absolve match
        case '[value] => Expr.summon[value is Structural] match
          case Some(instance) =>
            Field(name, fieldType, 0, Some(instance.asTerm))

          case None =>
            report.errorAndAbort
              ( s"the field $name of ${record.show} has no fixed layout: its type, " +
                s"${fieldType.show}, is not a sized integer and is not Structural" )

    def load(field: Field, data: Expr[Data], offset: Expr[Int]): Expr[Any] =
      def byte: Expr[Byte] = '{$data.u8($offset).toByte}

      def short: Expr[Short] =
        if big then '{$data.u16be($offset).toShort} else '{$data.u16($offset).toShort}

      def int: Expr[Int] = if big then '{$data.i32be($offset)} else '{$data.i32($offset)}
      def long: Expr[Long] = if big then '{$data.u64be($offset)} else '{$data.u64($offset)}

      field.instance match
        case Some(instance) => field.fieldType.asType.absolve match
          case '[value] => '{${instance.asExprOf[value is Structural]}.load($data, $offset)}

        case None =>
          if is[Byte](field) then byte
          else if is[B8](field) then '{$byte.bits}
          else if is[U8](field) then '{$byte.bits.u8}
          else if is[S8](field) then '{$byte.bits.s8}
          else if is[Short](field) then short
          else if is[B16](field) then '{$short.bits}
          else if is[U16](field) then '{$short.bits.u16}
          else if is[S16](field) then '{$short.bits.s16}
          else if is[Int](field) then int
          else if is[B32](field) then '{$int.bits}
          else if is[U32](field) then '{$int.bits.u32}
          else if is[S32](field) then '{$int.bits.s32}
          else if is[Long](field) then long
          else if is[B64](field) then '{$long.bits}
          else if is[U64](field) then '{$long.bits.u64}
          else '{$long.bits.s64}

    def store(field: Field, value: Term, target: Expr[Target], offset: Expr[Int]): Expr[Unit] =
      def as[field: Type]: Expr[field] = value.asExprOf[field]
      def byte(byte: Expr[Byte]): Expr[Unit] = '{layoutInternal.store8($target, $offset, $byte)}

      def short(short: Expr[Short]): Expr[Unit] =
        '{layoutInternal.store16($target, $offset, $short, $bigEndian)}

      def int(int: Expr[Int]): Expr[Unit] =
        '{layoutInternal.store32($target, $offset, $int, $bigEndian)}

      def long(long: Expr[Long]): Expr[Unit] =
        '{layoutInternal.store64($target, $offset, $long, $bigEndian)}

      field.instance match
        case Some(instance) => field.fieldType.asType.absolve match
          case '[value] =>
            '{${instance.asExprOf[value is Structural]}.store(${as[value]}, $target, $offset)}

        case None =>
          if is[Byte](field) then byte(as[Byte])
          else if is[B8](field) then byte('{${as[B8]}.s8.byte})
          else if is[U8](field) then byte('{${as[U8]}.bits.s8.byte})
          else if is[S8](field) then byte('{${as[S8]}.byte})
          else if is[Short](field) then short(as[Short])
          else if is[B16](field) then short('{${as[B16]}.s16.short})
          else if is[U16](field) then short('{${as[U16]}.bits.s16.short})
          else if is[S16](field) then short('{${as[S16]}.short})
          else if is[Int](field) then int(as[Int])
          else if is[B32](field) then int('{${as[B32]}.s32.int})
          else if is[U32](field) then int('{${as[U32]}.bits.s32.int})
          else if is[S32](field) then int('{${as[S32]}.int})
          else if is[Long](field) then long(as[Long])
          else if is[B64](field) then long('{${as[B64]}.s64.long})
          else if is[U64](field) then long('{${as[U64]}.bits.s64.long})
          else long('{${as[S64]}.long})

    val fields: scala.List[Field] =
      symbol.caseFields.map: member => field(member.name, record.memberType(member))

    // Every sized integer's offset is a constant; a field with an instance of its own adds
    // that instance's width to each offset after it.
    def offsets(base: Expr[Int]): scala.List[Expr[Int]] =
      fields.scanLeft((0, scala.List[Term]())): (state, next) =>
        (state(0) + next.width, state(1) ++ next.instance.toList)

      . map: (fixed, instances) =>
          instances.foldLeft(if fixed == 0 then base else '{$base + ${Expr(fixed)}}):
            (sum, instance) => '{$sum + ${instance.asExprOf[Structural]}.width}

    def loader(data: Expr[Data], offset: Expr[Int]): Expr[record] =
      val arguments = fields.zip(offsets(offset)).map: (field, position) =>
        load(field, data, position).asTerm

      New(TypeTree.of[record]).select(symbol.primaryConstructor).appliedToTypes(record.typeArgs)
      . appliedToArgs(arguments)
      . asExprOf[record]

    def storer(value: Expr[record], target: Expr[Target], offset: Expr[Int]): Expr[Unit] =
      val stores = symbol.caseFields.zip(fields).zip(offsets(offset)).map:
        case ((member, field), position) =>
          store(field, value.asTerm.select(member), target, position)

      Expr.block(stores, '{()})

    ' {
        new Structural:
          type Self = record
          def width: Int = ${offsets('{0}).last}
          def load(data: Data, offset: Int): record = ${loader('data, 'offset)}

          def store(value: record, target: scala.Array[Byte]^, offset: Int): Unit =
            ${storer('value, 'target, 'offset)}
      }
