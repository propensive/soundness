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
package cartouche

import scala.annotation.tailrec
import scala.caps

import capricious.*
import denominative.*
import murmuration.sortingAlgorithms.timsort
import randomization.seededRandomization
import rudiments.*
import symbolism.*
import vacuous.*

object Arranger:
  given greedy: Arranger = Greedy()

  // One pass of an `arrange` body. In the first pass `record` notes each caption and answers
  // provisionally; in the second it answers from the solved positions, matched to the calls by
  // their order. A pass is made by `arrange` for one run of the body and never shared.
  class Pass private[cartouche]
    ( val solved: Boolean, answers: Sequence[(Caption, Caption.Position)] )
  extends caps.Mutable:

    private var count: Int = 0
    private var captions: List[Caption] = Nil
    private var obstacles: List[Obstacle] = Nil
    private var extent: Optional[Obstacle.Box] = Unset
    private var divergence: Boolean = false

    // Whether the second pass asked for something the first did not: a call beyond those
    // recorded, or one whose caption differs. Such a call is answered provisionally.
    def diverged: Boolean = divergence

    private[cartouche] update def record(caption: Caption): Caption.Position =
      val ordinal = count
      count += 1

      if !solved then
        captions = caption :: captions
        Caption.provisional(caption)
      else
        val answer: Optional[(Caption, Caption.Position)] = answers.at(ordinal.z)

        answer.lay(diverge(caption)): pair =>
          if pair(0) == caption then pair(1) else diverge(caption)

    private update def diverge(caption: Caption): Caption.Position =
      divergence = true
      Caption.provisional(caption)

    private[cartouche] update def avoid(obstacle: Obstacle): Unit =
      if !solved then obstacles = obstacle :: obstacles

    private[cartouche] update def canvas(box: Obstacle.Box): Unit = if !solved then extent = box

    private[cartouche] def recorded: (List[Caption], List[Obstacle], Optional[Obstacle.Box]) =
      (captions.reverse, obstacles.reverse, extent)

  // One place a caption might go: its anchor, the side it lies on, its footprint (bare, and
  // padded by the caption's clearance), the leader it would need, and how far it is from where
  // it was asked for.
  private[cartouche] case class Candidate
    ( x:            Double,
      y:            Double,
      attachment:   Caption.Attachment,
      box:          Obstacle.Box,
      padded:       Obstacle.Box,
      leader:       Optional[Caption.Leader],
      displacement: Double ):

    def position(visible: Boolean): Caption.Position =
      Caption.Position(x, y, attachment, box, leader, visible)

    def leaderLine(width: Double): Optional[Obstacle.Line] =
      leader.let: leader => Obstacle.Line(leader.x1, leader.y1, leader.x2, leader.y2, width)

    // The collision of this candidate's leader with another's footprint.
    def crossing(other: Candidate, width: Double): Double =
      leaderLine(width).lay(0.0)(_.overlap(other.padded))

    // Everything this candidate and another have against each other: overlapping footprints,
    // and either's leader across the other's footprint.
    def conflict(other: Candidate, width: Double): Double =
      padded.overlap(other.padded) + crossing(other, width) + other.crossing(this, width)

  // The eight directions a displaced label may be moved in.
  private val directions: List[(Double, Double)] =
    List
      ( (1.0, 0.0), (-1.0, 0.0), (0.0, -1.0), (0.0, 1.0), (1.0, -1.0), (-1.0, -1.0), (1.0, 1.0),
        (-1.0, 1.0) )

  // Every place a caption may go, least displaced first: each side at the target, then each
  // side at each of the eight directions one step away, then two steps, and so on out to the
  // caption's reach. The enumeration is finite by construction, so the search over it is too.
  private[cartouche] def candidates
    ( caption: Caption, step: Double, rings: Int, leaderThreshold: Double )
  :   Sequence[Candidate] =

    val ringCount = if step <= 0.0 then 0 else rings.min((caption.reach/step).toInt)

    val offsets: List[(Double, Double)] =
      val displaced = List.range(1, ringCount + 1).fold(List[(Double, Double)]()): (acc, ring) =>
        val distance = ring*step

        directions.fold(acc): (acc2, direction) =>
          (direction(0)*distance, direction(1)*distance) :: acc2

      (0.0, 0.0) :: displaced.reverse

    val all = offsets.fold(List[Candidate]()): (acc, offset) =>
      caption.sides.fold(acc): (acc2, attachment) =>
        val (anchorX, anchorY) = caption.anchor(attachment, offset(0), offset(1))
        val box = caption.footprint(anchorX, anchorY, attachment)
        val displacement = scala.math.sqrt(offset(0)*offset(0) + offset(1)*offset(1))

        val leader: Optional[Caption.Leader] =
          if displacement <= leaderThreshold then Unset else
            val (edgeX, edgeY) = box.nearest(caption.x, caption.y)
            Caption.Leader(edgeX, edgeY, caption.x, caption.y)

        val padded = box.pad(caption.padding)
        Candidate(anchorX, anchorY, attachment, box, padded, leader, displacement) :: acc2

    all.reverse.to[Sequence]

  // How badly a candidate collides with what is already on the page: the area of its padded
  // footprint under each fixed obstacle, under each label already placed and under their
  // leaders; the area outside the canvas; and the length of its own leader across any placed
  // label. A leader may cross a fixed obstacle — pointing past a marker or a bar to its target
  // is what a leader is for — but not another label. Zero means the candidate is clear.
  private[cartouche] def score
    ( candidate:   Candidate,
      obstacles:   List[Obstacle],
      labels:      List[Obstacle.Box],
      leaders:     List[Obstacle.Line],
      canvas:      Optional[Obstacle.Box],
      leaderWidth: Double )
  :   Double =

    val fixed = obstacles.fold(0.0): (acc, obstacle) => acc + obstacle.overlap(candidate.padded)
    val placed = labels.fold(0.0): (acc, label) => acc + label.overlap(candidate.padded)
    val crossed = leaders.fold(0.0): (acc, leader) => acc + leader.overlap(candidate.padded)
    val outside = canvas.lay(0.0)(candidate.padded.excess(_))

    val leader = candidate.leaderLine(leaderWidth).lay(0.0): line =>
      labels.fold(0.0): (acc, label) => acc + line.overlap(label)

    fixed + placed + crossed + outside + leader

  // The best of a caption's candidates against what is placed: the first that is clear, since
  // they come least displaced first, or else the one that collides least.
  private[cartouche] def choose
    ( candidates:  Sequence[Candidate],
      obstacles:   List[Obstacle],
      labels:      List[Obstacle.Box],
      leaders:     List[Obstacle.Line],
      canvas:      Optional[Obstacle.Box],
      leaderWidth: Double )
  :   Optional[(Candidate, Double)] =

    def collision(candidate: Candidate): Double =
      score(candidate, obstacles, labels, leaders, canvas, leaderWidth)

    candidates.fold[Optional[(Candidate, Double)]](Unset): (best, candidate) =>
      best.lay((candidate, collision(candidate))): current =>
        if current(1) == 0.0 then current
        else
          val score = collision(candidate)
          if score < current(1) then (candidate, score) else current

  // The captions with their ordinals, highest priority first and otherwise in their own order.
  private[cartouche] def prioritized(captions: List[Caption]): List[(Caption, Int)] =
    val tagged: List[(Caption, Int)] = captions.indexed.map: (caption, ordinal) =>
      (caption, ordinal.n0)

    tagged.order: entry => -entry(0).priority

  private[cartouche] def countOf[element](list: List[element]): Int =
    list.fold(0): (count, _) => count + 1

  // Places each caption in turn, in priority order, at its best candidate given everything
  // placed before it. A caption that cannot be placed clear and falls back to hiding places
  // nothing, so it blocks nothing. Each accepted footprint and leader joins the obstacles for
  // those that follow, which is how a leader is avoided without another pass over the body.
  // Answers are in caption order, each with the collision score the candidate was accepted at.
  private[cartouche] def greedily
    ( captions:    List[Caption],
      lists:       Sequence[Sequence[Candidate]],
      obstacles:   List[Obstacle],
      canvas:      Optional[Obstacle.Box],
      leaderWidth: Double )
  :   Sequence[(Candidate, Double)] =

    val initial = (List[((Candidate, Double), Int)](), List[Obstacle.Box](), List[Obstacle.Line]())

    val (chosen, _, _) = prioritized(captions).fold(initial): (state, entry) =>
      val (acc, labels, leaders) = state
      val (caption, ordinal) = entry
      val candidates = lists.at(ordinal.z).or(Sequence.empty)

      val fallback: (Candidate, Double) =
        val position = Caption.provisional(caption)

        ( Candidate
            ( position.x, position.y, position.attachment, position.box,
              position.box.pad(caption.padding), Unset, 0.0 ),
          0.0 )

      val (candidate, collision) =
        choose(candidates, obstacles, labels, leaders, canvas, leaderWidth).or(fallback)

      val hidden = collision > 0.0 && caption.fallback == Caption.Fallback.Hide
      val labels2 = if hidden then labels else candidate.padded :: labels

      val leaders2 =
        if hidden then leaders else candidate.leaderLine(leaderWidth).lay(leaders)(_ :: leaders)

      (((candidate, collision), ordinal) :: acc, labels2, leaders2)

    chosen.order(_(1)).map(_(0)).to[Sequence]

  // Greedy placement: each caption, in priority order, takes the least displaced position that
  // is clear of everything placed before it, or the least collision if none is. Deterministic,
  // and bounded by the number of candidates: at most (1 + 8·rings) per side.
  case class Greedy
    ( step:            Double = 4.0,
      rings:           Int    = 8,
      leaderThreshold: Double = 6.0,
      leaderWidth:     Double = 1.0 )
  extends Arranger:

    def arrange
      ( captions: List[Caption], obstacles: List[Obstacle], canvas: Optional[Obstacle.Box] )
    :   List[Caption.Position] =

      val lists = captions.map(candidates(_, step, rings, leaderThreshold)).to[Sequence]
      val chosen = greedily(captions, lists, obstacles, canvas, leaderWidth)

      captions.indexed.map: (caption, ordinal) =>
        chosen.at(ordinal).lay(Caption.provisional(caption)): entry =>
          val (candidate, collision) = entry
          val hidden = collision > 0.0 && caption.fallback == Caption.Fallback.Hide

          if hidden then Caption.provisional(caption).copy(visible = false)
          else candidate.position(true)

  // Simulated annealing: starting from the greedy arrangement, captions are moved among their
  // candidates at random, with each move accepted if it lowers the total collision and
  // displacement, or with a probability that falls as the temperature cools. Unlike the greedy
  // sweep, a caption placed early can be moved to make room for one placed later. The random
  // moves are drawn from `seed`, so the result is the same for the same input; `iterations`
  // bounds the work exactly.
  case class Annealing
    ( seed:               Seed   = Seed(0L),
      iterations:         Int    = 4000,
      temperature:        Double = 1.0,
      cooling:            Double = 0.995,
      step:               Double = 4.0,
      rings:              Int    = 8,
      leaderThreshold:    Double = 6.0,
      leaderWidth:        Double = 1.0,
      displacementWeight: Double = 0.05 )
  extends Arranger:

    def arrange
      ( captions: List[Caption], obstacles: List[Obstacle], canvas: Optional[Obstacle.Box] )
    :   List[Caption.Position] =

      val count = countOf(captions)
      val lists = captions.map(candidates(_, step, rings, leaderThreshold)).to[Sequence]

      def sizeOf(list: Sequence[Candidate]): Int = list.fold(0): (n, _) => n + 1
      val sizes = lists.map(sizeOf)

      val start = greedily(captions, lists, obstacles, canvas, leaderWidth).map(_(0))
      val ordinals = List.range(0, count)

      // What one caption at one candidate contributes on its own: its collisions with the fixed
      // obstacles and the canvas, and its displacement.
      def solitary(candidate: Candidate): Double =
        score(candidate, obstacles, Nil, Nil, canvas, leaderWidth) +
          displacementWeight*candidate.displacement

      // Everything one caption at one candidate contributes to the total: its own terms and its
      // conflicts with every other caption, counted once, from this caption's side.
      def energy(ordinal: Int, candidate: Candidate, state: Sequence[Candidate]): Double =
        val pairs = ordinals.fold(0.0): (acc, other) =>
          if other == ordinal then acc
          else state.at(other.z).lay(acc): peer => acc + candidate.conflict(peer, leaderWidth)

        pairs + solitary(candidate)

      // The total, with each pair counted once.
      def total(state: Sequence[Candidate]): Double =
        ordinals.fold(0.0): (acc, ordinal) =>
          state.at(ordinal.z).lay(acc): candidate =>
            val later = ordinals.fold(0.0): (acc2, other) =>
              if other <= ordinal then acc2
              else state.at(other.z).lay(acc2): peer => acc2 + candidate.conflict(peer, leaderWidth)

            acc + later + solitary(candidate)

      def index(random: Random, bound: Int): Int =
        (random.unitInterval()*bound).toInt.min(bound - 1).max(0)

      @tailrec
      def anneal
        ( remaining: Int,
          heat:      Double,
          state:     Sequence[Candidate],
          current:   Double,
          best:      Sequence[Candidate],
          least:     Double,
          random:    Random )
      :   Sequence[Candidate] =

        if remaining <= 0 then best else
          val ordinal = index(random, count)
          val size = sizes.at(ordinal.z).or(1)
          val choice = index(random, size)

          val move: Optional[(Candidate, Candidate)] =
            state.at(ordinal.z).let: before =>
              lists.at(ordinal.z).let(_.at(choice.z)).let: after => (before, after)

          val (state2, current2) = move.lay((state, current)): pair =>
            val (before, after) = pair
            val delta = energy(ordinal, after, state) - energy(ordinal, before, state)
            val chance = if heat > 0.0 then scala.math.exp(-delta/heat) else 0.0
            val accepted = delta <= 0.0 || random.unitInterval() < chance
            if accepted then (state.define(ordinal.z, after), current + delta) else (state, current)

          val (best2, least2) = if current2 < least then (state2, current2) else (best, least)
          anneal(remaining - 1, heat*cooling, state2, current2, best2, least2, random)

      val settled: Sequence[Candidate] =
        if count == 0 then start else
          seed.stochastic:
            val random = summon[Random]
            val initial = total(start)
            anneal(iterations, temperature, start, initial, start, initial, random)

      // Captions that still collide and would rather hide do so, the least important first, so
      // that each decision sees the hiding already done.
      val ascending = prioritized(captions).reverse

      val visibility = ascending.fold(Map[Int, Boolean]()): (acc, entry) =>
        val (caption, ordinal) = entry

        val collides = settled.at(ordinal.z).lay(false): candidate =>
          val peers = ordinals.fold(0.0): (sum, other) =>
            if other == ordinal || !acc.at(other).or(true) then sum
            else settled.at(other.z).lay(sum): peer => sum + candidate.conflict(peer, leaderWidth)

          peers + score(candidate, obstacles, Nil, Nil, canvas, leaderWidth) > 0.0

        val hidden = collides && caption.fallback == Caption.Fallback.Hide
        acc.define(ordinal, !hidden)

      captions.indexed.map: (caption, ordinal) =>
        val visible = visibility.at(ordinal.n0).or(true)

        settled.at(ordinal).lay(Caption.provisional(caption)): candidate =>
          if visible then candidate.position(true)
          else Caption.provisional(caption).copy(visible = false)

// An algorithm that finds a position for every caption, given everything else on the page.
// The answers come back in the captions' order. Which algorithm arranges is the `Arranger` in
// scope: the companion's greedy default, or one imported by name from `arrangers`.
trait Arranger:
  def arrange
    ( captions: List[Caption], obstacles: List[Obstacle], canvas: Optional[Obstacle.Box] )
  :   List[Caption.Position]
