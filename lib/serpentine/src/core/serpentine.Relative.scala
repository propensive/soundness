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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package serpentine

import scala.compiletime.*, ops.int.*

import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Relative:
  @targetName("Up")
  object ^

  @targetName("Self")
  object ? extends Ascent(0):
    type Topic = Zero
    type Limit = 0

  def of[topic <: Tuple, limit <: Int](ascent: Int, descent: Text*)
  : Relative of topic under limit =

      new Relative(ascent, descent.to(List)):
        type Topic = topic
        type Limit = limit


  def apply[filesystem, topic <: Tuple, limit <: Int](ascent: Int, descent: Text*)
  : Relative of topic on filesystem under limit =

      new Relative(ascent, descent.to(List)):
        type Plane = filesystem
        type Topic = topic
        type Limit = limit


  private def conversion[from, to](fn: from => to) =
    new Conversion[from, to]:
      def apply(from: from): to = fn(from)

  given decodable: [filesystem: Filesystem]
        =>  (Relative on filesystem) is Decodable in Text = text =>
    if text == filesystem.self then ? else
      text.cut(filesystem.separator).pipe: parts =>
        (if parts.last == t"" then parts.init else parts).pipe: parts =>
          if parts.isEmpty then Relative(0) else
            (if parts.head == filesystem.self then parts.tail else parts).pipe: parts =>
              val ascent = parts.takeWhile(_ == filesystem.parent).length
              val descent = parts.drop(ascent).reverse

              Relative(ascent, descent*)


  inline given conversion: [topic, ascent <: Int, filesystem]
               =>  Conversion[Relative of topic under ascent,
                              Relative of topic under ascent on filesystem] =

      conversion(_.on[filesystem])


  given encodable: [filesystem: Filesystem] => Relative on filesystem is Encodable in Text =
    relative =>
      if relative.descent.isEmpty then
        if relative.ascent == 0 then filesystem.self
        else List.fill(relative.ascent)(filesystem.parent).join(filesystem.separator)
      else
        val ascender = filesystem.parent+filesystem.separator
        relative
        . descent
        . reverse
        . join(ascender*relative.ascent, filesystem.separator, t"")

  given showable: [filesystem: Filesystem, relative <: Relative on filesystem]
  => relative is Showable =
       _.encode

  transparent inline given quotient: [filesystem, relative <: (Relative on filesystem) | Text]
                           => relative is Quotient =
    relative0 =>
      relative0 match
        case _: Relative =>
          val relative = relative0.asInstanceOf[Relative on filesystem]

          relative.descent match
            case Nil | _ :: Nil => None
            case _ :: _ :: Nil  => Some((relative.descent(1), relative.descent(0)))

            case _ =>
              Some((relative.descent.last, Relative(0, relative.descent.init*)))

        case _ => None

  : relative is Quotient of Text over (Relative on filesystem) | Text


case class Relative(ascent: Int, descent: List[Text] = Nil) extends Planar, Topical, Limited:
  type Topic <: Tuple
  type Limit <: Int

  def delta: Int = descent.length - ascent

  def self: Boolean = ascent == 0 && descent == Nil

  transparent inline def rename(lambda: (prior: Text) ?=> Text): Optional[Relative] =
    descent.prim.let(parent / lambda(using _))

  private inline def check[topic, filesystem](path: List[Text]): Unit =
    inline !![topic] match
      case _: (head *: tail) =>
        infer[head is Admissible on filesystem].check(path.head)
        check[tail, filesystem](path.tail).unit

      case EmptyTuple =>
        ()

      case _ =>
        path.each(infer[Text is Admissible on filesystem].check(_))

  inline def on[filesystem]: Relative of Topic under Limit on filesystem =
    summonFrom:
      case compliant: (Plane is Compliant on `filesystem`) =>
        this.asInstanceOf[Relative of Topic under Limit on filesystem]
      case _ =>
        check[Topic, filesystem](descent.to(List))
        this.asInstanceOf[Relative of Topic under Limit on filesystem]

  transparent inline def parent = inline !![Topic] match
    case head *: tail => Relative[Plane, tail.type, Limit](ascent, descent.tail*)
    case EmptyTuple   => Relative[Plane, Zero, S[Limit]](ascent)

    case _ =>
      if descent.isEmpty then Relative[Plane, Topic, S[Limit]](ascent + 1)
      else Relative[Plane, Topic, Limit](ascent, descent.tail*)


  transparent inline def / (child: Any): Relative of (child.type *: Topic) under Limit =
    summonFrom:
      case given (child.type is Admissible on Plane) =>
        Relative[Plane, child.type *: Topic, Limit]
          (ascent, infer[child.type is Navigable].follow(child) +: descent*)

      case _ =>
        Relative.of[child.type *: Topic, Limit]
          (ascent, infer[child.type is Navigable].follow(child) :: descent*)


// case class Relative(ascent: Int, descent: Text*):
//   type Plane
//   type Topic <: Tuple
//   type Limit <: Int

// object Relative:

//   given [ElementType] => (Relative by ElementType) is Addable by (Relative by ElementType) into
//           (Relative by ElementType) =
//     (left, right) =>
//       def recur(ascent: Int, descent: List[Text], ascent2: Int): Relative by ElementType =
//         if ascent2 > 0 then
//           if descent.isEmpty then recur(ascent + 1, Nil, ascent - 1)
//           else recur(ascent, descent.tail, ascent - 1)
//         else Relative.from(ascent, right.textDescent ++ descent, left.separator)

//       recur(left.ascent, left.textDescent, right.ascent)
