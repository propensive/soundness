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
package mosquito

import scala.annotation.targetName
import scala.compiletime.*
import scala.reflect.ClassTag

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object internal:
  // Element arithmetic for the inline instances below, dispatched as symbolism's operators
  // are: an `AddOp` (quantitative's macro-generated, unit-checked operations) first, else the
  // `Addable` summoned with its result open — quantitative's is a transparent inline given
  // whose `Result` the macro computes, so it cannot be summoned with the result pinned. Both
  // bind the instance at its own type, so a primitive's `inline def add` is one instruction.
  // The cast re-states the result type the instance's own parameter already determined.
  // The casts on the operands are symbolism's own (see its `+`): a pattern-bound instance's
  // `Self` and `Operand` are not known to be `a` and `b` — which the backticks keep as the
  // method's type parameters rather than fresh pattern variables.
  private[mosquito] inline def plus[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is AddOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case addable: (`a` is Addable by `b`) =>
      addable.add(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  private[mosquito] inline def minus[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is SubOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case subtractable: (`a` is Subtractable by `b`) =>
      subtractable.subtract(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  private[mosquito] inline def times[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is MulOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case multiplicable: (`a` is Multiplicable by `b`) =>
      multiplicable.multiply(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  private[mosquito] inline def over[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is DivOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case divisible: (`a` is Divisible by `b`) =>
      divisible.divide(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  // Negation has no `Op` counterpart in symbolism, so only the instance itself is summoned —
  // as a `summonFrom` pattern, not `summonInline`, which would type it as the trait and box
  // through the erased `negate`; the pattern binds it at its own type, whose `inline def
  // negate` is one instruction.
  private[mosquito] inline def negated[a, r](operand: a): r = summonFrom:
    case negatable: (`a` is Negatable) => negatable.negate(operand.asInstanceOf).asInstanceOf[r]

  // The product of the `i`th element of one array and the `j`th of another, for the cross
  // products: at this level, not nested in them, as nested inline methods are not supported.
  private[mosquito] inline def productAt[a, b, r]
    ( lefts: Array[a]^{}, rights: Array[b]^{}, i: Int, j: Int )
  :   r =
    times[a, b, r](lefts.readUnchecked(i), rights.readUnchecked(j))

  // One component of the seven-dimensional cross product: p1 - n1 + p2 - n2 + p3 - n3 over
  // three index pairs, each pair's product taken in both orders.
  private[mosquito] inline def fano[a, b, r]
    ( lefts: Array[a]^{}, rights: Array[b]^{},
      i1: Int, j1: Int, i2: Int, j2: Int, i3: Int, j3: Int )
  :   r =

    var acc: r = productAt[a, b, r](lefts, rights, i1, j1)
    acc = minus[r, r, r](acc, productAt[a, b, r](lefts, rights, j1, i1))
    acc = plus[r, r, r](acc, productAt[a, b, r](lefts, rights, i2, j2))
    acc = minus[r, r, r](acc, productAt[a, b, r](lefts, rights, j2, i2))
    acc = plus[r, r, r](acc, productAt[a, b, r](lefts, rights, i3, j3))
    acc = minus[r, r, r](acc, productAt[a, b, r](lefts, rights, j3, i3))
    acc

  // A tuple's elements as a typed array; `Vector.apply`'s runtime, kept out of line so that
  // the transparent constructor expands to one call.
  private[mosquito] def elements[element: ClassTag](tuple: Tuple): Array[element]^{} =
    val buffer = Array.allocate[element](tuple.size)
    val iterator = tuple.productIterator
    var i = 0

    while iterator.hasNext do
      buffer(i) = iterator.next().asInstanceOf[element]
      i += 1

    Array.freeze(buffer)

  object Vector:
    // The elements' common type gives the storage its class: a `Vector(1.0, 2.0)` is a
    // `double[]`, a vector of quantities (opaquely `Double`) likewise, so element reads and the
    // arithmetic below never box. A tuple of mixed types has no `ClassTag` and is rejected.
    //
    // Transparent, so that the call site's type is the *reduced* element type (`Double`), not
    // the applied match type `Tuple.Union[(Double, Double)]`: the latter, arriving in an
    // arithmetic instance through type inference, erases to `Object` and turns every element
    // read into a boxing `ScalaRunTime.array_apply`, while the same type written out reduces
    // and reads with `daload`. An ordinary method's result, however spelt, has the first form.
    // The tuple is a type parameter rather than a path (`tuple.type`), and the `ClassTag` is
    // summoned in the body rather than taken as a parameter: a constructed tuple such as
    // `Mono(42)` has a capture-annotated `EmptyTuple` tail, over which `Tuple.Union` does not
    // reduce in a parameter's type (the found `ClassTag[Tuple.Union[Int *: EmptyTuple.type]]`
    // fails to match it), while the expansion-site summon sees the reduced element.
    transparent inline def apply[tuple <: Tuple](tuple: tuple)
    :   Vector[Tuple.Union[tuple], Tuple.Size[tuple]] =

      // The elements share one type: a union of two (`Vector(1, 2.0)`) has a synthesized
      // `ClassTag` (of `Object`), so the tag alone no longer rejects it; the union must be
      // the head's type, which it is exactly when every element has that type.
      summonInline[Tuple.Union[tuple] =:= Tuple.Head[tuple]]

      new Vector[Tuple.Union[tuple], Tuple.Size[tuple]]
        ( elements[Tuple.Union[tuple]](tuple)(using summonInline[ClassTag[Tuple.Union[tuple]]]) )

    def take[element: ClassTag](list: List[element], size: Int)
    :   Optional[Vector[element, size.type]] =

      val buffer = Array.allocate[element](size)
      var i = 0
      var rest = list

      while i < size do
        rest match
          case Nil =>
            return Unset

          case head :: tail =>
            buffer(i) = head
            rest = tail
            i += 1

      new Vector[element, size.type](Array.freeze(buffer))

    // The arithmetic instances are inline, and their operations resolve the element's
    // operation inside the inline body (`plus` and its siblings above) rather than through the
    // instance received as a parameter: a parameter has the trait's type, through which `add`
    // is an erased interface call that boxes every element, while the instance found at the
    // expansion site has its own type — `Addable.DoubleAddition.type`, say — whose `inline def
    // add` is one instruction. The parameter still decides `result`. So `v + w` on two
    // `Vector[Double, 3]`s expands to a loop of `daload`, `dadd`, `dastore`.
    inline given addable
    :   [ value,
          size   <: Int,
          left   <: Vector[value, size],
          value2,
          right  <: Vector[value2, size],
          result ]
    =>  ( addable: value is Addable by value2 to result, classTag: ClassTag[result] )
    =>  left is Addable:

      type Operand = right
      type Result = Vector[result, size]

      inline def add(left: left, right: right): Vector[result, size] =
        val length = left.data.length

        val arr = Array.build[result](length): array =>
          var i = 0

          while i < length do
            array(i) =
              plus[value, value2, result](left.data.readUnchecked(i), right.data.readUnchecked(i))

            i += 1

        new Vector[result, size](arr)

    inline given negatable: [value, size <: Int, vector <: Vector[value, size], result]
    =>  ( negatable: value is Negatable to result, classTag: ClassTag[result] )
    =>  vector is Negatable:

      type Result = Vector[result, size]

      inline def negate(operand: vector): Vector[result, size] =
        val length = operand.data.length

        val arr = Array.build[result](length): array =>
          var i = 0

          while i < length do
            array(i) = negated[value, result](operand.data.readUnchecked(i))
            i += 1

        new Vector[result, size](arr)

    inline given subtractable
    :   [ value,
          size   <: Int,
          left   <: Vector[value, size],
          value2,
          right  <: Vector[value2, size],
          result ]
    =>  ( subtractable: value is Subtractable by value2 to result, classTag: ClassTag[result] )
    =>  left is Subtractable:

      type Self = left
      type Operand = right
      type Result = Vector[result, size]

      inline def subtract(left: left, right: right): Vector[result, size] =
        val length = left.data.length

        val arr = Array.build[result](length): array =>
          var i = 0

          while i < length do
            array(i) =
              minus[value, value2, result](left.data.readUnchecked(i), right.data.readUnchecked(i))

            i += 1

        new Vector[result, size](arr)

    // Multiplication and division each have two shapes — by a scalar and element-wise by a
    // vector of the same size — as separate instances distinguished by `Operand`. The scalar
    // instance's operand is unconstrained, but it applies only where the elements themselves
    // multiply by it, which no element does by a `Vector`, so a vector operand reaches only
    // the element-wise instance. The scalar product of two vectors is `dot`, not `*`.
    inline given scalarMultiplicable
    :   [ value, size <: Int, left <: Vector[value, size], scalar, result ]
    =>  ( multiplication: value is Multiplicable by scalar to result, classTag: ClassTag[result] )
    =>  left is Multiplicable:

      type Operand = scalar
      type Result = Vector[result, size]

      inline def multiply(left: left, right: scalar): Vector[result, size] =
        val length = left.data.length

        // A loop, not `indices.each`: the closure's fresh capability leaks from the expansion.
        val arr = Array.build[result](length): array =>
          var i = 0

          while i < length do
            array(i) = times[value, scalar, result](left.data.readUnchecked(i), right)
            i += 1

        new Vector[result, size](arr)

    inline given scalarDivisible
    :   [ value, size <: Int, left <: Vector[value, size], scalar, result ]
    =>  ( division: value is Divisible by scalar to result, classTag: ClassTag[result] )
    =>  left is Divisible:

      type Operand = scalar
      type Result = Vector[result, size]

      inline def divide(left: left, right: scalar): Vector[result, size] =
        val length = left.data.length

        val arr = Array.build[result](length): array =>
          var i = 0

          while i < length do
            array(i) = over[value, scalar, result](left.data.readUnchecked(i), right)
            i += 1

        new Vector[result, size](arr)

    inline given vectorMultiplicable
    :   [ value,
          size   <: Int,
          left   <: Vector[value, size],
          value2,
          right  <: Vector[value2, size],
          result ]
    =>  ( multiplication: value is Multiplicable by value2 to result, classTag: ClassTag[result] )
    =>  left is Multiplicable:

      type Operand = right
      type Result = Vector[result, size]

      inline def multiply(left: left, right: right): Vector[result, size] =
        val length = left.data.length

        val arr = Array.build[result](length): array =>
          var i = 0

          while i < length do
            array(i) =
              times[value, value2, result](left.data.readUnchecked(i), right.data.readUnchecked(i))

            i += 1

        new Vector[result, size](arr)

    inline given vectorDivisible
    :   [ value,
          size   <: Int,
          left   <: Vector[value, size],
          value2,
          right  <: Vector[value2, size],
          result ]
    =>  ( division: value is Divisible by value2 to result, classTag: ClassTag[result] )
    =>  left is Divisible:

      type Operand = right
      type Result = Vector[result, size]

      inline def divide(left: left, right: right): Vector[result, size] =
        val length = left.data.length

        val arr = Array.build[result](length): array =>
          var i = 0

          while i < length do
            array(i) =
              over[value, value2, result](left.data.readUnchecked(i), right.data.readUnchecked(i))

            i += 1

        new Vector[result, size](arr)

    // A single-line counterpart to the (column-shaped, multi-line) `Showable`, sharing the `∣`
    // element separator with `Matrix`'s rows. Unlike the `Showable` it needs neither the size as a
    // singleton nor a text metric, so it is available for any vector.
    given inspectable: [element: Inspectable, vector <: Vector[element, ?]]
    =>  vector is Inspectable = vector =>

      val builder: StringBuilder = new StringBuilder("⟨")
      var index = 0

      while index < vector.data.length do
        if index > 0 then builder.append(" ∣ ")
        builder.append(element.text(vector.data.readUnchecked(index)).s)
        index += 1

      builder.append("⟩").toString.tt

    given showable: [size <: Int: ValueOf, element: Showable] => Text is Measurable
    =>  Vector[element, size] is Showable =

      vector =>
        val items = vector.list.stdlib.map(_.show)
        val width = items.maxBy(_.length).length
        val size = valueOf[size]

        if size == 1 then t"( ${items(0)} )"
        else
          val top = t"⎛ ${items.head.pad(width, Rtl)} ⎞"
          val bottom = t"⎝ ${items.last.pad(width, Rtl)} ⎠"
          val middle = items.tail.init.map: item => t"⎜ ${item.pad(width, Rtl)} ⎟"

          // `items` came over the stdlib bridge, so this chain is stdlib throughout. An
          // unqualified `Nil` is proscenium's opaque empty list, which makes `:::` resolve to
          // the opaque extension and reject the stdlib `middle`.
          (top :: middle ::: bottom :: scala.Nil).join(t"\n")

  // The products are inline, with the element arithmetic resolved at the call site (`times`,
  // `minus`, `plus`) as the instances' is, so a cross product of `Double` vectors is six
  // `dmul`s and three `dsub`s over `daload`s. The typeclass parameters fix the result types.
  extension [left](left: Vector[left, 3])
    inline def cross[right](right: Vector[right, 3])
      ( using multiplication: left is Multiplicable by right,
              addition:       multiplication.Result is Addable by multiplication.Result,
              subtraction:    multiplication.Result is Subtractable by multiplication.Result,
              classTag:       ClassTag[subtraction.Result] )
    :   Vector[subtraction.Result, 3] =

      val lefts = left.data
      val rights = right.data

      val first =
        minus[multiplication.Result, multiplication.Result, subtraction.Result]
          ( productAt[left, right, multiplication.Result](lefts, rights, 1, 2),
            productAt[left, right, multiplication.Result](lefts, rights, 2, 1) )

      val second =
        minus[multiplication.Result, multiplication.Result, subtraction.Result]
          ( productAt[left, right, multiplication.Result](lefts, rights, 2, 0),
            productAt[left, right, multiplication.Result](lefts, rights, 0, 2) )

      val third =
        minus[multiplication.Result, multiplication.Result, subtraction.Result]
          ( productAt[left, right, multiplication.Result](lefts, rights, 0, 1),
            productAt[left, right, multiplication.Result](lefts, rights, 1, 0) )

      new Vector[subtraction.Result, 3](Array[subtraction.Result](first, second, third))


  extension [left](left: Vector[left, 7])
    @targetName("cross7")
    inline def cross[right](right: Vector[right, 7])
      ( using multiplication: left is Multiplicable by right,
              addition:       multiplication.Result is Addable by multiplication.Result,
              subtraction:    multiplication.Result is Subtractable by multiplication.Result,
              addEq:          addition.Result =:= multiplication.Result,
              subEq:          subtraction.Result =:= multiplication.Result,
              classTag:       ClassTag[multiplication.Result] )
    :   Vector[multiplication.Result, 7] =

      val lefts = left.data
      val rights = right.data

      // Each component is `fano` over one of the Fano plane's index triples.
      val c0 = fano[left, right, multiplication.Result](lefts, rights, 1, 3, 2, 6, 4, 5)
      val c1 = fano[left, right, multiplication.Result](lefts, rights, 2, 4, 3, 0, 5, 6)
      val c2 = fano[left, right, multiplication.Result](lefts, rights, 3, 5, 4, 1, 6, 0)
      val c3 = fano[left, right, multiplication.Result](lefts, rights, 4, 6, 5, 2, 0, 1)
      val c4 = fano[left, right, multiplication.Result](lefts, rights, 5, 0, 6, 3, 1, 2)
      val c5 = fano[left, right, multiplication.Result](lefts, rights, 6, 1, 0, 4, 2, 3)
      val c6 = fano[left, right, multiplication.Result](lefts, rights, 0, 2, 1, 5, 3, 4)

      new Vector[multiplication.Result, 7](Array[multiplication.Result](c0, c1, c2, c3, c4, c5, c6))


  extension [size <: Int, left](left: Vector[left, size])
    // Inline, so that a read from a `Vector[Double, n]` is a `daload`, not a boxing generic
    // array access: `data`'s element type is only concrete at the call site.
    inline def element(index: Int): left = left.data.readUnchecked(index)

    inline def apply(index: Int): left = left.data.readUnchecked(index)
    def list: List[left] = left.data.to[List]
    def iarray: Array[left]^{} = left.data
    def size(using ValueOf[size]): Int = valueOf[size]

    // Inline, with the element arithmetic resolved at the call site (`plus` and `times`), as
    // the arithmetic instances are: a `Vector[Double, n]` sums its squares without boxing.
    inline def norm
      ( using multiplicable:  left is Multiplicable by left,
              addable:        multiplicable.Result is Addable by multiplicable.Result
                              to multiplicable.Result,
              rootable:       multiplicable.Result is Rootable[2] to left )
    :   left =

      var sum: multiplicable.Result =
        times[left, left, multiplicable.Result]
          ( left.data.readUnchecked(0), left.data.readUnchecked(0) )

      var i = 1

      while i < left.data.length do
        val x = left.data.readUnchecked(i)

        sum =
          plus[multiplicable.Result, multiplicable.Result, multiplicable.Result]
            ( sum, times[left, left, multiplicable.Result](x, x) )

        i += 1

      sum.sqrt


    // Inline with an inline lambda: the function is applied in the loop body, not through a
    // `Function1` which boxes a primitive element on the way in and out.
    inline def map[left2: ClassTag](inline fn: left => left2): Vector[left2, size] =
      val length = left.data.length

      val arr = Array.build[left2](length): array =>
        var i = 0

        while i < length do
          array(i) = fn(left.data.readUnchecked(i))
          i += 1

      new Vector[left2, size](arr)


    inline def unitary[square]
      ( using multiplicable: left is Multiplicable by left to square,
              addable:       square is Addable by square to square,
              rootable:      square is Rootable[2] to left,
              divisible:     left is Divisible by left to Double )
    :   Vector[Double, size] =

      val magnitude: left = left.norm
      val length = left.data.length

      val arr = Array.build[Double](length): array =>
        var i = 0

        while i < length do
          array(i) = over[left, left, Double](left.data.readUnchecked(i), magnitude)
          i += 1

      new Vector[Double, size](arr)


    inline def dot[right](right: Vector[right, size])
      ( using multiply: left is Multiplicable by right,
              size:     ValueOf[size],
              addable:  multiply.Result is Addable by multiply.Result,
              equality: addable.Result =:= multiply.Result )
    :   multiply.Result =

      var sum: multiply.Result =
        times[left, right, multiply.Result](left.data.readUnchecked(0), right.data.readUnchecked(0))

      var i = 1

      while i < size.value do
        sum =
          plus[multiply.Result, multiply.Result, multiply.Result]
            ( sum,
              times[left, right, multiply.Result]
                ( left.data.readUnchecked(i), right.data.readUnchecked(i) ) )

        i += 1

      sum

  class Vector[value, size <: Int](val data: Array[value]^{}):
    override def equals(right: Any): Boolean = right.asMatchable match
      // The cast is unchecked (the alias erases to `Object`): `same` compares element by
      // element through the JVM's generic array access, whatever the two element kinds.
      case that: Vector[?, ?] => Array.same(data, that.data.asInstanceOf[Array[value]^{}])
      case _                  => false

    override def hashCode: Int =
      // Inlined `MurmurHash3.arrayHash`: `arrayHash` demands a pure `Array`, which the
      // capture-checked frozen form cannot supply without an unsafe cast.
      var hash = scala.util.hashing.MurmurHash3.arraySeed
      var index = 0

      while index < data.length do
        hash = scala.util.hashing.MurmurHash3.mix(hash, data.readUnchecked(index).##)
        index += 1

      scala.util.hashing.MurmurHash3.finalizeHash(hash, data.length)

    override def toString: String = data.stdlib.mkString("Vector(", ", ", ")")
