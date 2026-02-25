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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import java.lang.classfile.instruction as jlci

import scala.reflect.*

import anticipation.*
import contingency.*
import digression.*
import escritoire.*
import escapade.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import proscenium.*
import spectacular.*
import vacuous.*

import tableStyles.minimal
import textMetrics.uniform
import columnAttenuation.ignore

object Bytecode:
  given teletypeable: Bytecode is Teletypeable = bytecode =>
    val table = Scaffold[Instruction]
      ( Column(e"$Bold(Source)", textAlign = TextAlignment.Right): line =>
          line.line.let: line =>
            val source = e"${rgb"#88aabb"}(${bytecode.sourceFile.or(t"")})"
            e"$source:${rgb"#ddddbb"}(${line.show})"

          . or(e""),
        Column(e"")(_.offset.show.subscripts),
        Column(e"$Bold(Opcode)")(_.opcode.teletype),
        Column(e"$Bold(Stack)")(_.stack.let(_.teletype).or(e"")) )

    table.tabulate(bytecode.instructions).grid(160).render.join(e"\n")

  given printable: Bytecode is Printable = _.teletype.render(_)

  given stack: List[Frame] is Teletypeable = stack =>
    e"${rgb"#aaaaaa"}(${stack.reverse.map(_.teletype).join(e"┃ ", e" ┊ ", e"")})"

  object Frame:
    given showable: Frame is Showable =
      case Array(frame) => t"[$frame]"
      case L(name)      => t"*$name"
      case other        => other.toString.tt

  enum Frame:
    case Z, B, C, S, I, J, F, D
    case L(name: Text)
    case Array(frame: Frame)

  case class Instruction
    ( opcode: Opcode, line: Optional[Int], stack: Optional[List[Frame]], offset: Int )

  enum Opcode:
    case Nop
    case AconstNull
    case IconstM1
    case Iconst0
    case Iconst1
    case Iconst2
    case Iconst3
    case Iconst4
    case Iconst5
    case Lconst0
    case Lconst1
    case Fconst0
    case Fconst1
    case Fconst2
    case Dconst0
    case Dconst1
    case Bipush(byte: Byte)
    case Sipush(short: Short)
    case Ldc(index: Byte)
    case LdcW(index: Short)
    case Ldc2W(index: Short)
    case Iload(index: Byte)
    case Lload(index: Byte)
    case Fload(index: Byte)
    case Dload(index: Byte)
    case Aload(index: Byte)
    case Iload0
    case Iload1
    case Iload2
    case Iload3
    case Lload0
    case Lload1
    case Lload2
    case Lload3
    case Fload0
    case Fload1
    case Fload2
    case Fload3
    case Dload0
    case Dload1
    case Dload2
    case Dload3
    case Aload0
    case Aload1
    case Aload2
    case Aload3
    case Iaload
    case Laload
    case Faload
    case Daload
    case Aaload
    case Baload
    case Caload
    case Saload
    case Istore(variable: Byte)
    case Lstore(variable: Byte)
    case Fstore(variable: Byte)
    case Dstore(variable: Byte)
    case Astore(variable: Byte)
    case Istore0
    case Istore1
    case Istore2
    case Istore3
    case Lstore0
    case Lstore1
    case Lstore2
    case Lstore3
    case Fstore0
    case Fstore1
    case Fstore2
    case Fstore3
    case Dstore0
    case Dstore1
    case Dstore2
    case Dstore3
    case Astore0
    case Astore1
    case Astore2
    case Astore3
    case Iastore
    case Lastore
    case Fastore
    case Dastore
    case Aastore
    case Bastore
    case Castore
    case Sastore
    case Pop
    case Pop2
    case Dup
    case DupX1
    case DupX2
    case Dup2
    case Dup2X1
    case Dup2X2
    case Swap
    case Iadd
    case Ladd
    case Fadd
    case Dadd
    case Isub
    case Lsub
    case Fsub
    case Dsub
    case Imul
    case Lmul
    case Fmul
    case Dmul
    case Idiv
    case Ldiv
    case Fdiv
    case Ddiv
    case Irem
    case Lrem
    case Frem
    case Drem
    case Ineg
    case Lneg
    case Fneg
    case Dneg
    case Ishl
    case Lshl
    case Ishr
    case Lshr
    case Iushr
    case Lushr
    case Iand
    case Land
    case Ior
    case Lor
    case Ixor
    case Lxor
    case Iinc(index: Byte, const: Byte)
    case I2l
    case I2f
    case I2d
    case L2i
    case L2f
    case L2d
    case F2i
    case F2l
    case F2d
    case D2i
    case D2l
    case D2f
    case I2b
    case I2c
    case I2s
    case Lcmp
    case Fcmpl
    case Fcmpg
    case Dcmpl
    case Dcmpg
    case Ifeq(offset: Short)
    case Ifne(offset: Short)
    case Iflt(offset: Short)
    case Ifge(offset: Short)
    case Ifgt(offset: Short)
    case Ifle(offset: Short)
    case IfIcmpeq(offset: Short)
    case IfIcmpne(offset: Short)
    case IfIcmplt(offset: Short)
    case IfIcmpge(offset: Short)
    case IfIcmpgt(offset: Short)
    case IfIcmple(offset: Short)
    case IfAcmpeq(offset: Short)
    case IfAcmpne(offset: Short)
    case Goto(offset: Short)
    case Jsr(offset: Short)
    case Ret(index: Byte)
    case Tableswitch()
    case Lookupswitch()
    case Ireturn
    case Lreturn
    case Freturn
    case Dreturn
    case Areturn
    case Return
    case Getstatic(index: Short)
    case Putstatic(index: Short)
    case Getfield(index: Short)
    case Putfield(index: Short)
    case Invokevirtual(owner: Text, method: Text)
    case Invokespecial(owner: Text, method: Text)
    case Invokestatic(owner: Text, method: Text)
    case Invokeinterface(owner: Text, method: Text, count: Byte)
    case Invokedynamic(method: Text)
    case New(index: Short)
    case Newarray(atype: Byte)
    case Anewarray(index: Short)
    case Arraylength
    case Athrow
    case Checkcast(index: Short)
    case Instanceof(index: Short)
    case Monitorenter
    case Monitorexit
    case Wide()
    case Multianewarray(index: Short, dimensions: Byte)
    case Ifnull(branch: Short)
    case Ifnonnull(branch: Short)
    case GotoW(branch: Int)
    case JsrW(branch: Int)
    case Breakpoint
    case OpCb
    case OpCc
    case OpCd
    case OpCe
    case OpCf
    case OpD0
    case OpD1
    case OpD2
    case OpD3
    case OpD4
    case OpD5
    case OpD6
    case OpD7
    case OpD8
    case OpD9
    case OpDa
    case OpDb
    case OpDc
    case OpDd
    case OpDe
    case OpDf
    case OpE0
    case OpE1
    case OpE2
    case OpE3
    case OpE4
    case OpE5
    case OpE6
    case OpE7
    case OpE8
    case OpE9
    case OpEa
    case OpEb
    case OpEc
    case OpEd
    case OpEe
    case OpEf
    case OpF0
    case OpF1
    case OpF2
    case OpF3
    case OpF4
    case OpF5
    case OpF6
    case OpF7
    case OpF8
    case OpF9
    case OpFa
    case OpFb
    case OpFc
    case OpFd
    case Impdep1
    case Impdep2

    def cost: Int =
      this.ordinal match
        case
          0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19
          | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
          | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63
          | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 87 | 88 | 89
          | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 100 | 104 | 108 | 112 | 116 | 120 | 122 | 124 | 126
          | 128 | 130 | 132 | 196 =>
            0

        case
          97 | 101 | 105 | 109 | 113 | 117 | 121 | 123 | 125 | 127 | 129 | 131 | 98 | 102 | 106
          | 110 | 114 | 118 | 99 | 103 | 107 | 111 | 115 | 119 | 133 | 134 | 135 | 136 | 137 | 138
          | 139 | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150 | 151 | 152 | 153
          | 154 | 155 | 156 | 157 | 158 | 159 | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 198 | 199
          | 183 | 184 =>
            1

        case
          46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 178 | 179
          | 180 | 181 | 192 | 193 | 167 | 168 | 169 | 200 | 201 | 172 | 173 | 174 | 175 | 176 | 177
          | 190 | 182 =>
            2

        case 170 | 171 | 187 | 188 | 189 | 197| 194 | 195 | 191 | 185 | 186 => 3

        case opcode =>
          panic(m"unrecognized opcode $opcode")

    def describe: Text = ordinal match
      case 0x00 => t"do nothing"
      case 0x01 => t"push null onto the operand stack"
      case 0x02 => t"push the int constant -1"
      case 0x03 => t"push the int constant 0"
      case 0x04 => t"push the int constant 1"
      case 0x05 => t"push the int constant 2"
      case 0x06 => t"push the int constant 3"
      case 0x07 => t"push the int constant 4"
      case 0x08 => t"push the int constant 5"
      case 0x09 => t"push the long constant 0L"
      case 0x0A => t"push the long constant 1L"
      case 0x0B => t"push the float constant 0.0f"
      case 0x0C => t"push the float constant 1.0f"
      case 0x0D => t"push the float constant 2.0f"
      case 0x0E => t"push the double constant 0.0"
      case 0x0F => t"push the double constant 1.0"
      case 0x10 => t"push a byte"
      case 0x11 => t"push a short"
      case 0x12 => t"push item from the constant pool"
      case 0x13 => t"same as ldc but uses a wider index"
      case 0x14 => t"push a long or double from the constant pool"
      case 0x15 => t"load int from local variable"
      case 0x16 => t"load long from local variable"
      case 0x17 => t"load float from local variable"
      case 0x18 => t"load double from local variable"
      case 0x19 => t"load reference from local variable"
      case 0x1A => t"load int from local variable 0"
      case 0x1B => t"load int from local variable 1"
      case 0x1C => t"load int from local variable 2"
      case 0x1D => t"load int from local variable 3"
      case 0x1E => t"load long from local variable 0"
      case 0x1F => t"load long from local variable 1"
      case 0x20 => t"load long from local variable 2"
      case 0x21 => t"load long from local variable 3"
      case 0x22 => t"load float from local variable 0"
      case 0x23 => t"load float from local variable 1"
      case 0x24 => t"load float from local variable 2"
      case 0x25 => t"load float from local variable 3"
      case 0x26 => t"load double from local variable 0"
      case 0x27 => t"load double from local variable 1"
      case 0x28 => t"load double from local variable 2"
      case 0x29 => t"load double from local variable 3"
      case 0x2A => t"load reference from local variable 0"
      case 0x2B => t"load reference from local variable 1"
      case 0x2C => t"load reference from local variable 2"
      case 0x2D => t"load reference from local variable 3"
      case 0x2E => t"load int from an array"
      case 0x2F => t"load long from an array"
      case 0x30 => t"load float from an array"
      case 0x31 => t"load double from an array"
      case 0x32 => t"load reference from an array"
      case 0x33 => t"load byte or boolean from an array"
      case 0x34 => t"load char from an array"
      case 0x35 => t"load short from an array"
      case 0x36 => t"store int into local variable"
      case 0x37 => t"store long into local variable"
      case 0x38 => t"store float into local variable"
      case 0x39 => t"store double into local variable"
      case 0x3A => t"store reference into local variable"
      case 0x3B => t"store int into local variable 0"
      case 0x3C => t"store int into local variable 1"
      case 0x3D => t"store int into local variable 2"
      case 0x3E => t"store int into local variable 3"
      case 0x3F => t"store long into local variable 0"
      case 0x40 => t"store long into local variable 1"
      case 0x41 => t"store long into local variable 2"
      case 0x42 => t"store long into local variable 3"
      case 0x43 => t"store float into local variable 0"
      case 0x44 => t"store float into local variable 1"
      case 0x45 => t"store float into local variable 2"
      case 0x46 => t"store float into local variable 3"
      case 0x47 => t"store double into local variable 0"
      case 0x48 => t"store double into local variable 1"
      case 0x49 => t"store double into local variable 2"
      case 0x4A => t"store double into local variable 3"
      case 0x4B => t"store reference into local variable 0"
      case 0x4C => t"store reference into local variable 1"
      case 0x4D => t"store reference into local variable 2"
      case 0x4E => t"store reference into local variable 3"
      case 0x4F => t"store int into an array"
      case 0x50 => t"store long into an array"
      case 0x51 => t"store float into an array"
      case 0x52 => t"store double into an array"
      case 0x53 => t"store reference into an array"
      case 0x54 => t"store byte or boolean into an array"
      case 0x55 => t"store char into an array"
      case 0x56 => t"store short into an array"
      case 0x57 => t"pop the top value from the stack"
      case 0x58 => t"pop the top one or two values"
      case 0x59 => t"duplicate the top value on the stack"
      case 0x5A => t"duplicate the top value and insert two values down"
      case 0x5B => t"duplicate the top value and insert three values down"
      case 0x5C => t"duplicate the top one or two stack words"
      case 0x5D => t"duplicate top one/two words and insert two/three down"
      case 0x5E => t"duplicate top one/two words and insert four/five down"
      case 0x5F => t"swap the two topmost values on the stack"
      case 0x60 => t"add two ints"
      case 0x61 => t"add two longs"
      case 0x62 => t"add two floats"
      case 0x63 => t"add two doubles"
      case 0x64 => t"subtract two ints"
      case 0x65 => t"subtract two longs"
      case 0x66 => t"subtract two floats"
      case 0x67 => t"subtract two doubles"
      case 0x68 => t"multiply two ints"
      case 0x69 => t"multiply two longs"
      case 0x6A => t"multiply two floats"
      case 0x6B => t"multiply two doubles"
      case 0x6C => t"divide two ints"
      case 0x6D => t"divide two longs"
      case 0x6E => t"divide two floats"
      case 0x6F => t"divide two doubles"
      case 0x70 => t"remainder of two ints"
      case 0x71 => t"remainder of two longs"
      case 0x72 => t"remainder of two floats"
      case 0x73 => t"remainder of two doubles"
      case 0x74 => t"negate int"
      case 0x75 => t"negate long"
      case 0x76 => t"negate float"
      case 0x77 => t"negate double"
      case 0x78 => t"shift int left"
      case 0x79 => t"shift long left"
      case 0x7A => t"shift int right"
      case 0x7B => t"shift long right"
      case 0x7C => t"shift int right"
      case 0x7D => t"shift long right"
      case 0x7E => t"bitwise AND of two ints"
      case 0x7F => t"bitwise AND of two longs"
      case 0x80 => t"bitwise OR of two ints"
      case 0x81 => t"bitwise OR of two longs"
      case 0x82 => t"bitwise XOR of two ints"
      case 0x83 => t"bitwise XOR of two longs"
      case 0x84 => t"increment local int variable by a signed byte"
      case 0x85 => t"convert int to long"
      case 0x86 => t"convert int to float"
      case 0x87 => t"convert int to double"
      case 0x88 => t"convert long to int"
      case 0x89 => t"convert long to float"
      case 0x8A => t"convert long to double"
      case 0x8B => t"convert float to int"
      case 0x8C => t"convert float to long"
      case 0x8D => t"convert float to double"
      case 0x8E => t"convert double to int"
      case 0x8F => t"convert double to long"
      case 0x90 => t"convert double to float"
      case 0x91 => t"convert int to byte"
      case 0x92 => t"convert int to char"
      case 0x93 => t"convert int to short"
      case 0x94 => t"compare two longs"
      case 0x95 => t"compare two floats, treating NaN as less"
      case 0x96 => t"compare two floats, treating NaN as greater"
      case 0x97 => t"compare two doubles, treating NaN as less"
      case 0x98 => t"compare two doubles, treating NaN as greater"
      case 0x99 => t"branch if int == 0"
      case 0x9A => t"branch if int != 0"
      case 0x9B => t"branch if int < 0"
      case 0x9C => t"branch if int >= 0"
      case 0x9D => t"branch if int > 0"
      case 0x9E => t"branch if int <= 0"
      case 0x9F => t"branch if two ints are equal"
      case 0xA0 => t"branch if two ints are not equal"
      case 0xA1 => t"branch if first int < second int"
      case 0xA2 => t"branch if first int >= second int"
      case 0xA3 => t"branch if first int > second int"
      case 0xA4 => t"branch if first int <= second int"
      case 0xA5 => t"branch if two references are equal"
      case 0xA6 => t"branch if two references are not equal"
      case 0xA7 => t"unconditional branch"
      case 0xA8 => t"jump to subroutine"
      case 0xA9 => t"return from subroutine"
      case 0xAA => t"jump based on int key in [low..high] range"
      case 0xAB => t"jump based on a set of int keys"
      case 0xAC => t"return int from method"
      case 0xAD => t"return long from method"
      case 0xAE => t"return float from method"
      case 0xAF => t"return double from method"
      case 0xB0 => t"return reference from method"
      case 0xB1 => t"return void from method"
      case 0xB2 => t"get a static field from a class"
      case 0xB3 => t"set a static field in a class"
      case 0xB4 => t"fetch a field from an object"
      case 0xB5 => t"set a field in an object"
      case 0xB6 => t"invoke an instance method"
      case 0xB7 => t"invoke an instance method for constructors, private, or superclass methods"
      case 0xB8 => t"invoke a static method"
      case 0xB9 => t"invoke an interface method"
      case 0xBA => t"invoke a dynamic call site"
      case 0xBB => t"create a new object"
      case 0xBC => t"create a new array of a primitive type"
      case 0xBD => t"create a new array of reference type"
      case 0xBE => t"get the length of an array"
      case 0xBF => t"throw an exception or error"
      case 0xC0 => t"check whether object is of a certain reference type"
      case 0xC1 => t"determine if object is of a certain reference type"
      case 0xC2 => t"enter monitor for synchronization"
      case 0xC3 => t"exit monitor for synchronization"
      case 0xC4 => t"extend local variable indexes or iinc with a wider index"
      case 0xC5 => t"create a new multidimensional array"
      case 0xC6 => t"branch if reference is null"
      case 0xC7 => t"branch if reference is not null"
      case 0xC8 => t"unconditional branch with a 32-bit offset"
      case 0xC9 => t"jump to subroutine with a wide offset"
      case 0xCA => t"reserved for breakpoints"
      case 0xFE => t"implementation-dependent"
      case 0xFF => t"implementation-dependent"
      case _    => t"unrecognized"

    def highlight: Rgb24 = cost match
      case 0 => rgb"#1a6a6c"
      case 1 => rgb"#659e24"
      case 2 => rgb"#e3a232"
      case 3 => rgb"#b31250"
      case _ => rgb"#777777"

    def transform(stack: List[Frame]): List[Frame] =
      import Frame.*
      this match
        case Nop                      => stack
        case New(_)                   => L(t"class") :: stack
        case Dup                      => stack.head :: stack.head :: stack.tail
        case Invokespecial(_, _)      => L(t"unknown") :: stack.tail // FIXME
        case Astore(_)                => stack.tail
        case Astore1                  => stack.tail
        case Astore2                  => stack.tail
        case Astore3                  => stack.tail
        case Aload(_)                 => L(t"?") :: stack
        case Aload0                   => L(t"?") :: stack
        case Aload1                   => L(t"?") :: stack
        case Aload2                   => L(t"?") :: stack
        case Aload3                   => L(t"?") :: stack
        case Athrow                   => L(t"?") :: Nil
        case Areturn                  => L(t"?") :: Nil
        case Iload1                   => I :: stack
        case Checkcast(_)             => L(t"?") :: stack.tail
        case Invokedynamic(_)         => L(t"?") :: stack.tail // FIXME
        case Getstatic(_)             => L(t"?") :: stack
        case Invokevirtual(_, _)      => L(t"?") :: stack.tail
        case Invokestatic(_, _)       => L(t"?") :: stack.tail
        case Invokeinterface(_, _, _) => L(t"?") :: stack.tail
        case Ifnonnull(_)             => stack.tail
        case Ifeq(_)                  => stack.tail
        case Instanceof(_)            => L(t"?") :: stack.tail
        case opcode                   => unsafely(throw Exception("Unhandled "+opcode))

  object Opcode:
    def apply(source: jlc.Instruction): Opcode = source match
      case invocation: jlci.InvokeInstruction =>
        val classname = invocation.owner.nn.name.nn.stringValue.nn.replace("/", ".").nn
        val method = invocation.name.nn.stringValue.nn

        source.opcode.nn.bytecode.absolve match
          case 182 => Invokevirtual(classname, method)
          case 183 => Invokespecial(classname, method)
          case 184 => Invokestatic(classname, method)
          case 185 => Invokeinterface(classname, method, 0)

      case invocation: jlci.InvokeDynamicInstruction =>
        val method = StackTrace.rewrite(invocation.name.nn.stringValue.nn, true)
        Invokedynamic(method)

      case other =>
        source.opcode.nn.bytecode match
          case 0   => Nop
          case 1   => AconstNull
          case 2   => IconstM1
          case 3   => Iconst0
          case 4   => Iconst1
          case 5   => Iconst2
          case 6   => Iconst3
          case 7   => Iconst4
          case 8   => Iconst5
          case 9   => Lconst0
          case 10  => Lconst1
          case 11  => Fconst0
          case 12  => Fconst1
          case 13  => Fconst2
          case 14  => Dconst0
          case 15  => Dconst1
          case 16  => Bipush(0)
          case 17  => Sipush(0)
          case 18  => Ldc(0)
          case 19  => LdcW(0)
          case 20  => Ldc2W(0)
          case 21  => Iload(0)
          case 22  => Lload(0)
          case 23  => Fload(0)
          case 24  => Dload(0)
          case 25  => Aload(0)
          case 26  => Iload0
          case 27  => Iload1
          case 28  => Iload2
          case 29  => Iload3
          case 30  => Lload0
          case 31  => Lload1
          case 32  => Lload2
          case 33  => Lload3
          case 34  => Fload0
          case 35  => Fload1
          case 36  => Fload2
          case 37  => Fload3
          case 38  => Dload0
          case 39  => Dload1
          case 40  => Dload2
          case 41  => Dload3
          case 42  => Aload0
          case 43  => Aload1
          case 44  => Aload2
          case 45  => Aload3
          case 46  => Iaload
          case 47  => Laload
          case 48  => Faload
          case 49  => Daload
          case 50  => Aaload
          case 51  => Baload
          case 52  => Caload
          case 53  => Saload
          case 54  => Istore(0)
          case 55  => Lstore(0)
          case 56  => Fstore(0)
          case 57  => Dstore(0)
          case 58  => Astore(0)
          case 59  => Istore0
          case 60  => Istore1
          case 61  => Istore2
          case 62  => Istore3
          case 63  => Lstore0
          case 64  => Lstore1
          case 65  => Lstore2
          case 66  => Lstore3
          case 67  => Fstore0
          case 68  => Fstore1
          case 69  => Fstore2
          case 70  => Fstore3
          case 71  => Dstore0
          case 72  => Dstore1
          case 73  => Dstore2
          case 74  => Dstore3
          case 75  => Astore0
          case 76  => Astore1
          case 77  => Astore2
          case 78  => Astore3
          case 79  => Iastore
          case 80  => Lastore
          case 81  => Fastore
          case 82  => Dastore
          case 83  => Aastore
          case 84  => Bastore
          case 85  => Castore
          case 86  => Sastore
          case 87  => Pop
          case 88  => Pop2
          case 89  => Dup
          case 90  => DupX1
          case 91  => DupX2
          case 92  => Dup2
          case 93  => Dup2X1
          case 94  => Dup2X2
          case 95  => Swap
          case 96  => Iadd
          case 97  => Ladd
          case 98  => Fadd
          case 99  => Dadd
          case 100 => Isub
          case 101 => Lsub
          case 102 => Fsub
          case 103 => Dsub
          case 104 => Imul
          case 105 => Lmul
          case 106 => Fmul
          case 107 => Dmul
          case 108 => Idiv
          case 109 => Ldiv
          case 110 => Fdiv
          case 111 => Ddiv
          case 112 => Irem
          case 113 => Lrem
          case 114 => Frem
          case 115 => Drem
          case 116 => Ineg
          case 117 => Lneg
          case 118 => Fneg
          case 119 => Dneg
          case 120 => Ishl
          case 121 => Lshl
          case 122 => Ishr
          case 123 => Lshr
          case 124 => Iushr
          case 125 => Lushr
          case 126 => Iand
          case 127 => Land
          case 128 => Ior
          case 129 => Lor
          case 130 => Ixor
          case 131 => Lxor
          case 132 => Iinc(0, 0)
          case 133 => I2l
          case 134 => I2f
          case 135 => I2d
          case 136 => L2i
          case 137 => L2f
          case 138 => L2d
          case 139 => F2i
          case 140 => F2l
          case 141 => F2d
          case 142 => D2i
          case 143 => D2l
          case 144 => D2f
          case 145 => I2b
          case 146 => I2c
          case 147 => I2s
          case 148 => Lcmp
          case 149 => Fcmpl
          case 150 => Fcmpg
          case 151 => Dcmpl
          case 152 => Dcmpg
          case 153 => Ifeq(0)
          case 154 => Ifne(0)
          case 155 => Iflt(0)
          case 156 => Ifge(0)
          case 157 => Ifgt(0)
          case 158 => Ifle(0)
          case 159 => IfIcmpeq(0)
          case 160 => IfIcmpne(0)
          case 161 => IfIcmplt(0)
          case 162 => IfIcmpge(0)
          case 163 => IfIcmpgt(0)
          case 164 => IfIcmple(0)
          case 165 => IfAcmpeq(0)
          case 166 => IfAcmpne(0)
          case 167 => Goto(0)
          case 168 => Jsr(0)
          case 169 => Ret(0)
          case 170 => Tableswitch()
          case 171 => Lookupswitch()
          case 172 => Ireturn
          case 173 => Lreturn
          case 174 => Freturn
          case 175 => Dreturn
          case 176 => Areturn
          case 177 => Return
          case 178 => Getstatic(0)
          case 179 => Putstatic(0)
          case 180 => Getfield(0)
          case 181 => Putfield(0)
          case 187 => New(0)
          case 188 => Newarray(0)
          case 189 => Anewarray(0)
          case 190 => Arraylength
          case 191 => Athrow
          case 192 => Checkcast(0)
          case 193 => Instanceof(0)
          case 194 => Monitorenter
          case 195 => Monitorexit
          case 196 => Wide()
          case 197 => Multianewarray(0, 0)
          case 198 => Ifnull(0)
          case 199 => Ifnonnull(0)
          case 200 => GotoW(0)
          case 201 => JsrW(0)

          case opcode =>
            panic(m"unrecognized opcode $opcode")

    given Opcode is Teletypeable = opcode =>
      opcode.show.cut(t" ") match
        case Nil               => panic(m"opcode should never be empty")
        case List(keyword)     => e"${opcode.highlight}(${opcode.show})"
        case keyword :: params => e"${opcode.highlight}($keyword) $Italic(${params.join(t" ")})"

    given Opcode is Showable =
      case Nop                  => t"nop"
      case AconstNull           => t"a·const·null"
      case IconstM1             => t"i·const₋₁"
      case Iconst0              => t"i·const₀"
      case Iconst1              => t"i·const₁"
      case Iconst2              => t"i·const₂"
      case Iconst3              => t"i·const₃"
      case Iconst4              => t"i·const₄"
      case Iconst5              => t"i·const₅"
      case Lconst0              => t"l·const₀"
      case Lconst1              => t"l·const₁"
      case Fconst0              => t"f·const₀"
      case Fconst1              => t"f·const₁"
      case Fconst2              => t"f·const₂"
      case Dconst0              => t"d·const₀"
      case Dconst1              => t"d·const₁"
      case Bipush(_)            => t"b·i·push"
      case Sipush(_)            => t"s·i·push"
      case Ldc(_)               => t"ldc"
      case LdcW(_)              => t"ldcʷ"
      case Ldc2W(_)             => t"ldc²ʷ"
      case Iload(_)             => t"i·load"
      case Lload(_)             => t"l·load"
      case Fload(_)             => t"f·load"
      case Dload(_)             => t"d·load"
      case Aload(_)             => t"a·load"
      case Iload0               => t"i·load₀"
      case Iload1               => t"i·load₁"
      case Iload2               => t"i·load₂"
      case Iload3               => t"i·load₃"
      case Lload0               => t"l·load₀"
      case Lload1               => t"l·load₁"
      case Lload2               => t"l·load₂"
      case Lload3               => t"l·load₃"
      case Fload0               => t"f·load₀"
      case Fload1               => t"f·load₁"
      case Fload2               => t"f·load₂"
      case Fload3               => t"f·load₃"
      case Dload0               => t"d·load₀"
      case Dload1               => t"d·load₁"
      case Dload2               => t"d·load₂"
      case Dload3               => t"d·load₃"
      case Aload0               => t"a·load₀"
      case Aload1               => t"a·load₁"
      case Aload2               => t"a·load₂"
      case Aload3               => t"a·load₃"
      case Iaload               => t"iᴬ·load"
      case Laload               => t"lᴬ·load"
      case Faload               => t"fᴬ·load"
      case Daload               => t"dᴬ·load"
      case Aaload               => t"aᴬ·load"
      case Baload               => t"bᴬ·load"
      case Caload               => t"cᴬ·load"
      case Saload               => t"sᴬ·load"
      case Istore(_)            => t"i·store"
      case Lstore(_)            => t"l·store"
      case Fstore(_)            => t"f·store"
      case Dstore(_)            => t"d·store"
      case Astore(_)            => t"a·store"
      case Istore0              => t"i·store₀"
      case Istore1              => t"i·store₁"
      case Istore2              => t"i·store₂"
      case Istore3              => t"i·store₃"
      case Lstore0              => t"l·store₀"
      case Lstore1              => t"l·store₁"
      case Lstore2              => t"l·store₂"
      case Lstore3              => t"l·store₃"
      case Fstore0              => t"f·store₀"
      case Fstore1              => t"f·store₁"
      case Fstore2              => t"f·store₂"
      case Fstore3              => t"f·store₃"
      case Dstore0              => t"d·store₀"
      case Dstore1              => t"d·store₁"
      case Dstore2              => t"d·store₂"
      case Dstore3              => t"d·store₃"
      case Astore0              => t"a·store₀"
      case Astore1              => t"a·store₁"
      case Astore2              => t"a·store₂"
      case Astore3              => t"a·store₃"
      case Iastore              => t"iᴬ·store"
      case Lastore              => t"lᴬ·store"
      case Fastore              => t"fᴬ·store"
      case Dastore              => t"dᴬ·store"
      case Aastore              => t"aᴬ·store"
      case Bastore              => t"bᴬ·store"
      case Castore              => t"cᴬ·store"
      case Sastore              => t"sᴬ·store"
      case Pop                  => t"pop"
      case Pop2                 => t"pop²"
      case Dup                  => t"dup"
      case DupX1                => t"dup∕₁"
      case DupX2                => t"dup∕₂"
      case Dup2                 => t"dup²"
      case Dup2X1               => t"dup²∕₁"
      case Dup2X2               => t"dup²∕₂"
      case Swap                 => t"swap"
      case Iadd                 => t"i·add"
      case Ladd                 => t"l·add"
      case Fadd                 => t"f·add"
      case Dadd                 => t"d·add"
      case Isub                 => t"i·sub"
      case Lsub                 => t"l·sub"
      case Fsub                 => t"f·sub"
      case Dsub                 => t"d·sub"
      case Imul                 => t"i·mul"
      case Lmul                 => t"l·mul"
      case Fmul                 => t"f·mul"
      case Dmul                 => t"d·mul"
      case Idiv                 => t"i·div"
      case Ldiv                 => t"l·div"
      case Fdiv                 => t"f·div"
      case Ddiv                 => t"d·div"
      case Irem                 => t"i·rem"
      case Lrem                 => t"l·rem"
      case Frem                 => t"f·rem"
      case Drem                 => t"d·rem"
      case Ineg                 => t"i·neg"
      case Lneg                 => t"l·neg"
      case Fneg                 => t"f·neg"
      case Dneg                 => t"d·neg"
      case Ishl                 => t"i·shl"
      case Lshl                 => t"l·shl"
      case Ishr                 => t"i·shr"
      case Lshr                 => t"l·shr"
      case Iushr                => t"i·ushr"
      case Lushr                => t"l·ushr"
      case Iand                 => t"i·and"
      case Land                 => t"l·and"
      case Ior                  => t"i·or"
      case Lor                  => t"l·or"
      case Ixor                 => t"i·xor"
      case Lxor                 => t"l·xor"
      case Iinc(_, _)           => t"i·inc"
      case I2l                  => t"i→l"
      case I2f                  => t"i→"
      case I2d                  => t"i→d"
      case L2i                  => t"l→i"
      case L2f                  => t"l→f"
      case L2d                  => t"l→d"
      case F2i                  => t"f→i"
      case F2l                  => t"f→l"
      case F2d                  => t"f→d"
      case D2i                  => t"d→i"
      case D2l                  => t"d→l"
      case D2f                  => t"d→f"
      case I2b                  => t"i→b"
      case I2c                  => t"i→c"
      case I2s                  => t"i→s"
      case Lcmp                 => t"l·cmp"
      case Fcmpl                => t"f·cmpl"
      case Fcmpg                => t"f·cmpg"
      case Dcmpl                => t"d·cmpl"
      case Dcmpg                => t"d·cmpg"
      case Ifeq(_)              => t"if·eq"
      case Ifne(_)              => t"if·ne"
      case Iflt(_)              => t"if·lt"
      case Ifge(_)              => t"if·ge"
      case Ifgt(_)              => t"if·gt"
      case Ifle(_)              => t"if·le"
      case IfIcmpeq(_)          => t"if·i·cmp·eq"
      case IfIcmpne(_)          => t"if·i·cmp·ne"
      case IfIcmplt(_)          => t"if·i·cmp·lt"
      case IfIcmpge(_)          => t"if·i·cmp·ge"
      case IfIcmpgt(_)          => t"if·i·cmp·gt"
      case IfIcmple(_)          => t"if·i·cmp·le"
      case IfAcmpeq(_)          => t"if·a·cmp·eq"
      case IfAcmpne(_)          => t"if·a·cmp·ne"
      case Goto(_)              => t"goto"
      case Jsr(_)               => t"jsr"
      case Ret(_)               => t"ret"
      case Tableswitch()        => t"table·switch"
      case Lookupswitch()       => t"lookup·switch"
      case Ireturn              => t"i·return"
      case Lreturn              => t"l·return"
      case Freturn              => t"f·return"
      case Dreturn              => t"d·return"
      case Areturn              => t"a·return"
      case Return               => t"return"
      case Getstatic(_)         => t"get·static"
      case Putstatic(_)         => t"put·static"
      case Getfield(_)          => t"get·field"
      case Putfield(_)          => t"put·field"
      case New(_)               => t"new"
      case Newarray(_)          => t"new·array"
      case Anewarray(_)         => t"a·new·array"
      case Arraylength          => t"array·length"
      case Athrow               => t"a·throw"
      case Checkcast(_)         => t"check·cast"
      case Instanceof(_)        => t"instance·of"
      case Monitorenter         => t"monitor·enter"
      case Monitorexit          => t"monitor·exit"
      case Wide()               => t"wide"
      case Multianewarray(_, _) => t"multi·a·new·array"
      case Ifnull(_)            => t"if·null"
      case Ifnonnull(_)         => t"if·!null"
      case GotoW(_)             => t"gotoʷ"
      case JsrW(_)              => t"jsrʷ"
      case Breakpoint           => t"breakpoint"
      case OpCb                 => t"-CB-"
      case OpCc                 => t"-CC-"
      case OpCd                 => t"-CD-"
      case OpCe                 => t"-CE-"
      case OpCf                 => t"-CF-"
      case OpD0                 => t"-D0-"
      case OpD1                 => t"-D1-"
      case OpD2                 => t"-D2-"
      case OpD3                 => t"-D3-"
      case OpD4                 => t"-D4-"
      case OpD5                 => t"-D5-"
      case OpD6                 => t"-D6-"
      case OpD7                 => t"-D7-"
      case OpD8                 => t"-D8-"
      case OpD9                 => t"-D9-"
      case OpDa                 => t"-DA-"
      case OpDb                 => t"-DB-"
      case OpDc                 => t"-DC-"
      case OpDd                 => t"-DD-"
      case OpDe                 => t"-DE-"
      case OpDf                 => t"-DF-"
      case OpE0                 => t"-E0-"
      case OpE1                 => t"-E1-"
      case OpE2                 => t"-E2-"
      case OpE3                 => t"-E3-"
      case OpE4                 => t"-E4-"
      case OpE5                 => t"-E5-"
      case OpE6                 => t"-E6-"
      case OpE7                 => t"-E7-"
      case OpE8                 => t"-E8-"
      case OpE9                 => t"-E9-"
      case OpEa                 => t"-EA-"
      case OpEb                 => t"-EB-"
      case OpEc                 => t"-EC-"
      case OpEd                 => t"-ED-"
      case OpEe                 => t"-EE-"
      case OpEf                 => t"-EF-"
      case OpF0                 => t"-F0-"
      case OpF1                 => t"-F1-"
      case OpF2                 => t"-F2-"
      case OpF3                 => t"-F3-"
      case OpF4                 => t"-F4-"
      case OpF5                 => t"-F5-"
      case OpF6                 => t"-F6-"
      case OpF7                 => t"-F7-"
      case OpF8                 => t"-F8-"
      case OpF9                 => t"-F9-"
      case OpFa                 => t"-FA-"
      case OpFb                 => t"-FB-"
      case OpFc                 => t"-FC-"
      case OpFd                 => t"-FD-"
      case Impdep1              => t"imp·dep₁"
      case Impdep2              => t"imp·dep₂"

      case Invokevirtual(cls, name) =>
        val cls2 = StackTrace.rewrite(cls.s)
        val name2 = StackTrace.rewrite(name.s, true)
        t"invoke·virtual $cls2 ⌗ $name2"

      case Invokespecial(cls, name) =>
        val cls2 = StackTrace.rewrite(cls.s)
        val name2 = StackTrace.rewrite(name.s, true)
        t"invoke·special $cls2 . $name2"

      case Invokestatic(cls, name) =>
        val cls2 = StackTrace.rewrite(cls.s)
        val name2 = StackTrace.rewrite(name.s, true)
        t"invoke·static $cls2 . $name2"

      case Invokeinterface(cls, name, _) =>
        val cls2 = StackTrace.rewrite(cls.s)
        val name2 = StackTrace.rewrite(name.s, true)
        t"invoke·interface $cls2 ⌗ $name2"

      case Invokedynamic(name) =>
        val name2 = StackTrace.rewrite(name.s, true)
        t"invoke·dynamic $name2"

case class Bytecode(sourceFile: Optional[Text], instructions: List[Bytecode.Instruction]):
  def embed(codepoint: Codepoint): Bytecode =
    val instructions2 = instructions.map: instruction =>
      instruction.copy(line = instruction.line.let(_ + codepoint.line - 1))

    Bytecode(codepoint.source.cut(t"/").last, instructions2)
