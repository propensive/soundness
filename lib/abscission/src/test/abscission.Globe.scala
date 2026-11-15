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
package abscission

import scala.reflect.Selectable.reflectiveSelectable

// What the tests shrink: a small troupe of players, with one of every kind of thing the
// analysis has a rule for, and beside each thing which is used, one which is not. `Globe` is
// its entry point, and leaves a transcript of the performance where the JVM keeps properties,
// since the closed world is loaded by a classloader which shares nothing else with the tests.
//
// It is written with the JDK's and the standard library's types alone, so that the world which
// is reached from it is small.
trait Player:
  def name: String
  def line: String
  def speak: String = name.concat(": ").nn.concat(line).nn
  def bow: String = Wardrobe.costume(name)

// Says a line after another has said it.
trait Echoed extends Player:
  abstract override def speak: String = super.speak.concat(" (echoed)").nn

class Hamlet extends Player:
  def name: String = "Hamlet"
  def line: String = "To be, or not to be"
  def soliloquy: String = Wardrobe.costume("Hamlet")

class Horatio extends Player:
  def name: String = "Horatio"
  def line: String = "Good night, sweet prince"

class Ghost extends Horatio, Echoed:
  override def name: String = "Ghost"

// Never cast: nothing makes one, so its initialization, and what that needs, is not kept.
class Yorick(skull: String) extends Hamlet:
  val jest: String = Wardrobe.costume(skull)
  override def line: String = "Alas"

object Wardrobe:
  val stock: String = "motley"
  def costume(name: String): String = name.concat(" in ").nn.concat(stock).nn

object Prompter:
  def cue(act: Int): String = "Act ".concat(act.toString).nn
  def interval(minutes: Int): String = minutes.toString.concat(" minutes").nn

final class Ducats(val amount: Int) extends scala.AnyVal:
  def plus(that: Ducats): Ducats = Ducats(amount + that.amount)
  def minus(that: Ducats): Ducats = Ducats(amount - that.amount)
  override def toString: String = amount.toString.concat(" ducats").nn

enum Humour:
  case Sanguine, Choleric, Melancholic, Phlegmatic

  def warm: Boolean = this == Sanguine || this == Choleric
  def dry: Boolean = this == Choleric || this == Melancholic

case class Cue(act: Int, scene: Int):
  def next: Cue = Cue(act, scene + 1)
  def previous: Cue = Cue(act, scene - 1)

sealed trait Part
case class Speech(text: String) extends Part
case class Dialogue(first: Part, second: Part) extends Part
case class Aside(text: String) extends Part

object Even:
  def unapply(value: Int): Option[Int] = if value%2 == 0 then Some(value/2) else None

trait Critic:
  def review(play: String): String

class Fool:
  def jest(times: Int): String = "hey nonny ".repeat(times).nn.trim.nn
  def caper: String = "caper"

object Stage:
  object Trapdoor:
    def depth: Int = 3

  class Curtain(val colour: String):
    def draw: String = colour.concat(" curtain").nn

  val boards: String = "oak"

class Building(val materials: String)

// Its box is used and it is not: it is kept for the box to be in, with nothing of its own.
object Playhouse extends Building(Wardrobe.costume("thatch")):
  val capacity: Int = Wardrobe.stock.length

  class Box(val seats: Int):
    def price: Int = seats*6

class Troupe:
  lazy val patron: String = "Lord Chamberlain"
  lazy val rival: String = Prompter.interval(15)

object Globe extends Runnable:
  inline def twice(inline value: Int): Int = value + value

  def words(part: Part): Int = part match
    case Speech(text)            => text.split(" ").nn.length
    case Dialogue(first, second) => words(first) + words(second)
    case Aside(text)             => text.length

  def seats(pit: Int, gallery: Int = 300)(boxes: Int = pit/10): Int = pit + gallery + boxes

  def perform(): String =
    val transcript: StringBuilder = new StringBuilder()
    def say(text: String): Unit = transcript.append(text).nn.append('\n')
    val players: scala.List[Player] = scala.List(Hamlet(), Ghost())
    players.foreach { player => say(player.speak) }
    say(Prompter.cue(3))
    say(Ducats(30).plus(Ducats(12)).toString)
    say(Humour.values.length.toString)
    say(Humour.Choleric.warm.toString)
    say(Humour.valueOf("Melancholic").toString)
    say(Cue(3, 1).next.toString)
    say((Cue(3, 1) == Cue(3, 1)).toString)
    say(words(Dialogue(Speech("to be"), Dialogue(Speech("or not"), Speech("to be")))).toString)

    8 match
      case Even(half) => say("half ".concat(half.toString).nn)
      case _          => say("odd")

    val critic: Critic = play => play.concat(" is a hit").nn
    say(critic.review("Hamlet"))
    val fool: { def jest(times: Int): String } = Fool()
    say(fool.jest(2))
    say(Stage.Curtain("red").draw)
    say(Playhouse.Box(4).price.toString)
    say(Stage.Trapdoor.depth.toString)
    say(Troupe().patron)
    say(twice(21).toString)
    say(seats(500)().toString)
    val bytes: java.io.ByteArrayOutputStream = java.io.ByteArrayOutputStream()
    val output: java.io.ObjectOutputStream = java.io.ObjectOutputStream(bytes)
    output.writeObject(Humour.Phlegmatic)
    output.writeObject(Cue(5, 2))
    output.close()

    val input: java.io.ObjectInputStream =
      java.io.ObjectInputStream(java.io.ByteArrayInputStream(bytes.toByteArray))

    say((input.readObject().nn eq Humour.Phlegmatic).toString)
    say(input.readObject().nn.toString)
    transcript.toString

  // What a test calls the closed world's own `Globe` through, since `Runnable` is the JDK's,
  // and so is the one type the two classloaders share.
  def run(): Unit =
    System.setProperty("abscission.globe", perform())
    ()

  def main(args: scala.Array[String]): Unit = run()
