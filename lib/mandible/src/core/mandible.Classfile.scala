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
import java.lang.classfile.attribute as jlca
import java.lang.classfile.instruction as jlci

import java.nio.charset.StandardCharsets
import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hellenism.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Classfile:
  // ClassfileError → Classfile.Error
  case class Error()(using Diagnostics)
  extends fulminate.Error(293, 0)(m"there was an error reading the classfile")

  given aggregable: Classfile is Aggregable by Data = stream => new Classfile(stream.read[Data].readable)

  def apply(name: Text)(using classloader: Classloader): Optional[Classfile] =
    // Cast to the pure stdlib view (same erasure): the frozen member of the `Optional`
    // union freshens to an `any.rd` the enclosing object cannot admit.
    classloader(name).asInstanceOf[Optional[scala.IArray[Byte]]].let(new Classfile(_))

  def apply[classtype: ClassTag](using classloader: Classloader): Optional[Classfile] =
    val cls = classtype.runtimeClass
    val name = t"${cls.getName().nn.replace('.', '/').nn}.class"
    // Cast to the pure stdlib view (same erasure): the frozen member of the `Optional`
    // union freshens to an `any.rd` the enclosing object cannot admit.
    classloader(name).asInstanceOf[Optional[scala.IArray[Byte]]].let(new Classfile(_))

// `data` is the stdlib immutable array, not the frozen `Data`: a frozen-array constructor
// field would make `Classfile` itself a capability. Conversion happens in the companion.
class Classfile(data: scala.IArray[Byte]):
  val sourceFile: Optional[Text] =
    model.attributes.nn.to[List].stdlib.collect:
      case attribute: jlca.SourceFileAttribute =>
        attribute.sourceFile().nn.stringValue.nn.tt

    . headOption.getOrElse(Unset)

  // The JSR-45 `SourceDebugExtension`, whose body is the text of an SMAP mapping the synthetic
  // line numbers the classfile records back to the source positions they stand for. The body is
  // modified UTF-8, which for the ASCII an SMAP contains is plain UTF-8.
  val sourceDebugExtension: Optional[Text] =
    model.attributes.nn.to[List].stdlib.collect:
      case attribute: jlca.SourceDebugExtensionAttribute =>
        String(attribute.contents.nn, StandardCharsets.UTF_8).tt

    . headOption.getOrElse(Unset)

  class Method(model: jlc.MethodModel):
    def name: Text = model.methodName.nn.toString.tt

    def bytecode: Optional[Bytecode] = Optional(model.code().nn.get()).let: codeModel =>
      val code: jlca.CodeAttribute = codeModel match
        case attr: jlca.CodeAttribute => attr
        case _                        => panic(m"code attribute not present")

      val elements = code.elementList.nn.to[List]

      val labels: Map[jlc.Label, Int] =
        val builder = scala.collection.immutable.Map.newBuilder[jlc.Label, Int]
        var offset = 0

        elements.each:
          case instr: jlc.Instruction       => offset += instr.sizeInBytes
          case target: jlci.LabelTarget     => builder += target.label.nn -> offset
          case _                            => ()

        builder.result().to(Map)

      val stackMaps: Map[jlc.Label, List[Bytecode.Frame]] =
        val attr =
          code.attributes.nn.iterator.nn.asScala.collectFirst:
            case smt: jlca.StackMapTableAttribute => smt

        attr.fold(Map.empty[jlc.Label, List[Bytecode.Frame]]): smt =>
          smt.entries.nn.to[List].map: entry =>
            val frames =
              entry.stack.nn.to[List].stdlib.map(Bytecode.Frame.fromVerificationType).reverse.to(List)

            entry.target.nn -> frames

          . to[Map]

      def recur
        ( todo:  List[jlc.CodeElement],
          line:  Optional[Int],
          done:  List[Bytecode.Instruction],
          stack: Optional[List[Bytecode.Frame]],
          count: Int )
      :   List[Bytecode.Instruction] =

        todo match
          case Nil => done.reverse

          case next :: todo =>
            next match
              case instruction: jlc.Instruction =>
                val opcode = Bytecode.Opcode(instruction, labels)
                val stack2 = stack.let(opcode.transform(_))

                recur
                  ( todo,
                    Unset,
                    Bytecode.Instruction(opcode, line, stack2, count) :: done,
                    stack2,
                    count + instruction.sizeInBytes )

              case lineNo: jlci.LineNumber =>
                recur(todo, lineNo.line, done, stack, count)

              case other: jlci.LocalVariable =>
                recur(todo, line, done, stack, count)

              case other: jlci.LabelTarget =>
                val resetStack: Optional[List[Bytecode.Frame]] =
                  stackMaps.stdlib.get(other.label.nn).fold(stack)(identity)

                recur(todo, line, done, resetStack, count)

              case other =>
                panic(m"did not handle ${other.toString.tt}")


      val instructions = recur(elements, Unset, Nil, Nil, 0)

      Bytecode(sourceFile, instructions, code.maxStack, code.maxLocals)

  private lazy val model: jlc.ClassModel =
    jlc.ClassFile.of().nn.parse(data.asInstanceOf[scala.Array[Byte]]).nn
  lazy val methods: List[Method] = model.methods.nn.to[List].map(Method(_))
