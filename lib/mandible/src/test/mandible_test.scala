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

import soundness.*

import classloaders.threadContext

object Tests extends Suite(m"Mandible tests"):
  def run(): Unit =
    test(m"Locate a known method on a classfile"):
      val rewrite =
        Classfile[StackTrace].let(_.methods.find(_.name == t"rewrite").getOrElse(Unset)).vouch
    . assert()

    test(m"Disassemble a known method's bytecode"):
      val bytecode =
        Classfile[StackTrace]
        . let(_.methods.find(_.name == t"rewrite").getOrElse(Unset))
        . let(_.bytecode)
        . vouch
      bytecode.instructions.size
    . assert(_ > 0)

    test(m"Bytecode carries declared maxStack and maxLocals"):
      val bytecode =
        Classfile[StackTrace]
        . let(_.methods.find(_.name == t"rewrite").getOrElse(Unset))
        . let(_.bytecode)
        . vouch
      (bytecode.maxStack, bytecode.maxLocals)
    . assert((s, l) => s >= 0 && l >= 0)

    test(m"Method descriptor parser handles primitives and references"):
      Bytecode.Descriptor.parse(t"(Ljava/lang/String;I)V")
    . assert: parsed =>
        parsed.args.size == 2 && parsed.result.absent

    test(m"Method descriptor parser handles array types and return"):
      Bytecode.Descriptor.parse(t"([[Ljava/lang/Object;J)Z")
    . assert: parsed =>
        parsed.args.size == 2 && parsed.result == Bytecode.Frame.Z

    test(m"Detect virtual call as effectively static when receiver is a singleton"):
      // Construct: GETSTATIC Foo$.MODULE$:LFoo$;  followed by  INVOKEVIRTUAL Foo$.doIt()V
      val moduleFrame = Bytecode.Frame.L(t"Foo$$")
      val getstatic =
        Bytecode.Instruction
          ( Bytecode.Opcode.Getstatic(t"Foo$$", t"MODULE$$", t"LFoo$$;"),
            Unset,
            moduleFrame :: Nil,
            0 )

      val invoke =
        Bytecode.Instruction
          ( Bytecode.Opcode.Invokevirtual(t"Foo$$", t"doIt", t"()V"),
            Unset,
            Nil,
            3 )

      Bytecode(Unset, List(getstatic, invoke), 1, 0).effectivelyStaticCalls
    . assert(_ == Set(3))

    test(m"A virtual call on an opaque receiver is not flagged as static"):
      val opaqueFrame = Bytecode.Frame.L(t"?")
      val getstatic =
        Bytecode.Instruction
          ( Bytecode.Opcode.Getstatic(t"Bar", t"thing", t"Ljava/lang/Object;"),
            Unset,
            opaqueFrame :: Nil,
            0 )

      val invoke =
        Bytecode.Instruction
          ( Bytecode.Opcode.Invokevirtual(t"Foo$$", t"doIt", t"()V"),
            Unset,
            Nil,
            3 )

      Bytecode(Unset, List(getstatic, invoke), 1, 0).effectivelyStaticCalls
    . assert(_.isEmpty)
