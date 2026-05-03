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
package mosquito

import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import larceny.*
import probably.*
import proscenium.*
import quantitative.*
import spectacular.*
import symbolism.*
import vacuous.*

given Decimalizer(4)

object Tests extends Suite(m"Mosquito tests"):
  def run(): Unit =
    test(m"Create a Tensor of Ints"):
      Tensor(1, 2, 3)
    . assert(_ == Tensor(1, 2, 3))

    test(m"A Tensor of Ints infers the correct size"):
      demilitarize:
        val tensor: Tensor[Int, 3] = Tensor(1, 3, 4)
      .map(_.message)
    . assert(_ == Nil)

    test(m"Type error if size is incorrect"):
      demilitarize:
        val tensor: Tensor[Int, 2] = Tensor(1, 3, 4)
    . assert(_.nonEmpty)

    test(m"Type error if type is incorrect"):
      demilitarize:
        val tensor: Tensor[String, 3] = Tensor(1, 3, 4)
    . assert(_.nonEmpty)

    test(m"Calculate integer dot-product"):
      Tensor(1, 2, 3).dot(Tensor(4, 3, 7))
    . assert(_ == 31)

    test(m"Calculate Double dot-product"):
      Tensor(0.1, 0.2, 0.3).dot(Tensor(0.4, 0.3, 0.7))
    . assert(_ === 0.31 +/- 0.000001)

    test(m"Calculate integer cross-product"):
      Tensor(1, 2, 3).cross(Tensor(4, 3, 7))
    . assert(_ == Tensor(5, 5, -5))

    test(m"Calculate Double cross-product"):
      Tensor(1.4, 2.4, 3.8).cross(Tensor(4.9, 3.6, 0.7))
    . assert(_ == Tensor(-12.0, 17.64, -6.72))

    test(m"Show Tensor 3-tensor"):
      Tensor(1, 3, 6).show
    . assert(_ == t"\u239b 1 \u239e\n\u239c 3 \u239f\n\u239d 6 \u23a0")

    test(m"Show Tensor 1-tensor"):
      Tensor(Mono(42)).show
    . assert(_ == t"( 42 )")

    test(m"Show Tensor 2-tensor"):
      Tensor(1, 2).show
    . assert(_ == t"\u239b 1 \u239e\n\u239d 2 \u23a0")

    test(m"Add two tensors"):
      Tensor(1, 2, 3) + Tensor(3, 4, 5)
    . assert(_ == Tensor(4, 6, 8))

    test(m"Subtract two tensors"):
      Tensor(5, 7, 9) - Tensor(1, 2, 3)
    . assert(_ == Tensor(4, 5, 6))

    test(m"Negate a tensor"):
      -Tensor(1, -2, 3)
    . assert(_ == Tensor(-1, 2, -3))

    suite(m"Element access and conversions"):
      test(m"element(0) on a 3-tensor"):
        Tensor(10, 20, 30).element(0)
      . assert(_ == 10)

      test(m"element(1) on a 3-tensor"):
        Tensor(10, 20, 30).element(1)
      . assert(_ == 20)

      test(m"element(2) on a 3-tensor"):
        Tensor(10, 20, 30).element(2)
      . assert(_ == 30)

      test(m"apply(i) accessor matches element(i)"):
        Tensor(10, 20, 30)(1)
      . assert(_ == 20)

      test(m"list conversion"):
        Tensor(1, 2, 3).list
      . assert(_ == List(1, 2, 3))

      test(m"iarray conversion"):
        Tensor("a", "b", "c").iarray.toList
      . assert(_ == List("a", "b", "c"))

      test(m"size of a 4-tensor"):
        Tensor(1, 2, 3, 4).size
      . assert(_ == 4)

    suite(m"Map operations"):
      test(m"Map increment over Ints"):
        Tensor(1, 2, 3).map(_ + 1)
      . assert(_ == Tensor(2, 3, 4))

      test(m"Map type-changing (Int to String)"):
        Tensor(1, 2, 3).map(_.toString)
      . assert(_ == Tensor("1", "2", "3"))

    suite(m"Vector magnitude operations"):
      test(m"Norm of a 3-4 right triangle"):
        Tensor(3.0, 4.0).norm
      . assert(_ === 5.0 +/- 0.000001)

      test(m"Norm of a 2-3-6 vector"):
        Tensor(2.0, 3.0, 6.0).norm
      . assert(_ === 7.0 +/- 0.000001)

      test(m"Unitary of a 3-4 vector x-component"):
        Tensor(3.0, 4.0).unitary.element(0)
      . assert(_ === 0.6 +/- 0.000001)

      test(m"Unitary of a 3-4 vector y-component"):
        Tensor(3.0, 4.0).unitary.element(1)
      . assert(_ === 0.8 +/- 0.000001)

    suite(m"Tensor.take and List.slide"):
      test(m"Tensor.take takes the first N elements"):
        Tensor.take(List(1, 2, 3), 3)
      . assert(_ == Tensor(1, 2, 3))

      test(m"Tensor.take returns Unset when list is too short"):
        Tensor.take(List(1, 2), 3)
      . assert(_ == Unset)

      test(m"Tensor.take of size 0 with empty list is defined"):
        Tensor.take(Nil: List[Int], 0)
      . assert(_ != Unset)

      test(m"List.slide produces a stream of 2-tensors"):
        List(1, 2, 3, 4).slide(2).to(List)
      . assert(_ == List(Tensor(1, 2), Tensor(2, 3), Tensor(3, 4)))

    suite(m"Tensors of various sizes"):
      test(m"1-tensor list"):
        Tensor(Mono(42)).list
      . assert(_ == List(42))

      test(m"2-tensor list"):
        Tensor(1, 2).list
      . assert(_ == List(1, 2))

      test(m"4-tensor list"):
        Tensor(1, 2, 3, 4).list
      . assert(_ == List(1, 2, 3, 4))

      test(m"5-tensor element access"):
        Tensor(10, 20, 30, 40, 50).element(4)
      . assert(_ == 50)

      test(m"5-tensor size"):
        Tensor(10, 20, 30, 40, 50).size
      . assert(_ == 5)

      test(m"4-tensor iarray length"):
        Tensor("a", "b", "c", "d").iarray.length
      . assert(_ == 4)

    suite(m"Vector identities"):
      val a = Tensor(1, 2, 3)
      val b = Tensor(4, 5, 6)

      test(m"Cross product is anti-commutative"):
        a.cross(b)
      . assert(_ == -b.cross(a))

      test(m"Dot product of orthogonal unit vectors is zero"):
        Tensor(1, 0, 0).dot(Tensor(0, 1, 0))
      . assert(_ == 0)

      test(m"Dot product is commutative"):
        a.dot(b)
      . assert(_ == b.dot(a))

    suite(m"Quantity operations"):
      test(m"Add two quantity tensors"):
        Tensor(1.0*Metre, 2.0*Metre, 3.0*Metre) + Tensor(3.0*Metre, 4.0*Metre, 5.0*Metre)
      . assert(_ == Tensor(4.0*Metre, 6.0*Metre, 8.0*Metre))

      test(m"Add two mixed-quantity tensors"):
        Tensor(1.0*Foot, 1.0*Foot, 1.0*Foot) + Tensor(3.0*Metre, 4.0*Metre, 5.0*Metre)
      . assert(_ == Tensor(3.3048*Metre, 4.3048*Metre, 5.3048*Metre))

      test(m"Map from m to m²"):
        Tensor(1.0*Metre, 2.0*Metre, 3.0*Metre, 4.0*Metre).map(_*Metre)
      . assert(_ == Tensor(1.0*Metre*Metre, 2.0*Metre*Metre, 3.0*Metre*Metre, 4.0*Metre*Metre))

    suite(m"Matrix tests"):
      val m1 = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val m2 = Matrix[3, 2]((7, 8), (9, 10), (11, 12))

      test(m"Access matrix elements"):
        m1(0, 0)
      . assert(_ == 1)

      test(m"Access matrix elements2"):
        m1(1, 1)
      . assert(_ == 5)

      test(m"Access matrix elements 3"):
        m1(1, 2)
      . assert(_ == 6)

      test(m"Multiply matrices"):
        m1*m2
      . assert(_ == Matrix[2, 2]((58, 64), (139, 154)))

      test(m"Scalar multiply matrices"):
        m1*10
      . assert(_ == Matrix[2, 3]((10, 20, 30), (40, 50, 60)))

      test(m"Scalar divide matrices"):
        m1/2
      . assert(_ == Matrix[2, 3]((0, 1, 1), (2, 2, 3)))

    suite(m"Determinant"):
      test(m"Determinant of 2x2 matrix"):
        Matrix[2, 2]((1, 2), (3, 4)).determinant
      . assert(_ == -2)

      test(m"Determinant of 2x2 zero matrix"):
        Matrix[2, 2]((0, 0), (0, 0)).determinant
      . assert(_ == 0)

      test(m"Determinant of 2x2 identity"):
        Matrix[2, 2]((1, 0), (0, 1)).determinant
      . assert(_ == 1)

      test(m"Determinant of 3x3 identity"):
        Matrix[3, 3]((1, 0, 0), (0, 1, 0), (0, 0, 1)).determinant
      . assert(_ == 1)

      test(m"Determinant of 3x3 known matrix"):
        Matrix[3, 3]((2, -3, 1), (2, 0, -1), (1, 4, 5)).determinant
      . assert(_ == 49)

      test(m"Determinant of 3x3 upper triangular = product of diagonal"):
        Matrix[3, 3]((2, 1, 5), (0, 3, 7), (0, 0, 4)).determinant
      . assert(_ == 24)

      test(m"Determinant of 3x3 singular matrix is zero"):
        Matrix[3, 3]((1, 2, 3), (2, 4, 6), (1, 1, 1)).determinant
      . assert(_ == 0)

      test(m"Determinant of 4x4 matrix"):
        Matrix[4, 4]
         ((1, 0, 2, -1),
          (3, 0, 0,  5),
          (2, 1, 4, -3),
          (1, 0, 5,  0))
        . determinant
      . assert(_ == 30)

      test(m"Determinant of Double 2x2 matrix"):
        Matrix[2, 2]((1.5, 2.0), (3.0, 4.0)).determinant
      . assert(_ === 0.0 +/- 0.000001)

      test(m"Type error if determinant called on non-square matrix"):
        demilitarize:
          Matrix[2, 3]((1, 2, 3), (4, 5, 6)).determinant
      . assert(_.nonEmpty)

    suite(m"Inverse"):
      test(m"Inverse of 2x2 identity is identity"):
        Matrix[2, 2]((1.0, 0.0), (0.0, 1.0)).inverse
      . assert(_ == Matrix[2, 2]((1.0, 0.0), (0.0, 1.0)))

      test(m"Inverse of 2x2 known matrix"):
        Matrix[2, 2]((1.0, 2.0), (3.0, 4.0)).inverse
      . assert(_ == Matrix[2, 2]((-2.0, 1.0), (1.5, -0.5)))

      test(m"Inverse of singular 2x2 matrix is Unset"):
        Matrix[2, 2]((1.0, 2.0), (2.0, 4.0)).inverse
      . assert(_ == Unset)

      test(m"M times M-inverse is identity (2x2)"):
        val m = Matrix[2, 2]((4.0, 7.0), (2.0, 6.0))
        val product = m*m.inverse.vouch
        val target = Matrix[2, 2]((1.0, 0.0), (0.0, 1.0))
        product.elements.indices.map(i => math.abs(product.elements(i) - target.elements(i))).max
      . assert(_ < 0.000001)

      test(m"M times M-inverse is identity (3x3)"):
        val m = Matrix[3, 3]((1.0, 2.0, 3.0), (0.0, 1.0, 4.0), (5.0, 6.0, 0.0))
        val product = m*m.inverse.vouch
        val target = Matrix[3, 3]((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0))
        product.elements.indices.map(i => math.abs(product.elements(i) - target.elements(i))).max
      . assert(_ < 0.000001)

      test(m"Inverse of singular 3x3 matrix is Unset"):
        Matrix[3, 3]((1.0, 2.0, 3.0), (2.0, 4.0, 6.0), (1.0, 1.0, 1.0)).inverse
      . assert(_ == Unset)

      test(m"Type error if inverse called on non-square matrix"):
        demilitarize:
          Matrix[2, 3]((1.0, 2.0, 3.0), (4.0, 5.0, 6.0)).inverse
      . assert(_.nonEmpty)

    suite(m"Seven-dimensional cross product"):
      val a7 = Tensor(1, 2, 3, 4, 5, 6, 7)
      val b7 = Tensor(2, 1, 4, 3, 6, 5, 0)

      test(m"e0 cross e1 = e3"):
        Tensor(1, 0, 0, 0, 0, 0, 0).cross(Tensor(0, 1, 0, 0, 0, 0, 0))
      . assert(_ == Tensor(0, 0, 0, 1, 0, 0, 0))

      test(m"7D cross product is anti-commutative"):
        a7.cross(b7)
      . assert(_ == -b7.cross(a7))

      test(m"7D cross of vector with itself is zero"):
        a7.cross(a7)
      . assert(_ == Tensor(0, 0, 0, 0, 0, 0, 0))

      test(m"7D cross product is orthogonal to first operand"):
        a7.dot(a7.cross(b7))
      . assert(_ == 0)

      test(m"7D cross product is orthogonal to second operand"):
        b7.dot(a7.cross(b7))
      . assert(_ == 0)

      test(m"7D Pythagorean identity |a x b|^2 = |a|^2 |b|^2 - (a.b)^2"):
        val ad = Tensor(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
        val bd = Tensor(2.0, 1.0, 4.0, 3.0, 6.0, 5.0, 0.0)
        val cross = ad.cross(bd)
        val crossNormSquared = cross.dot(cross)
        val aDotA = ad.dot(ad)
        val bDotB = bd.dot(bd)
        val aDotB = ad.dot(bd)
        crossNormSquared - (aDotA*bDotB - aDotB*aDotB)
      . assert(d => math.abs(d) < 0.000001)

      test(m"Type error if cross called on 4-tensor"):
        demilitarize:
          Tensor(1, 2, 3, 4).cross(Tensor(5, 6, 7, 8))
      . assert(_.nonEmpty)

    suite(m"Interesting types"):
      test(m"Dot product of a tensor of quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Inch, 3*Inch, 6*Inch)
        v1.dot(v2)
      . assert(_ == 22*Inch*Inch)

      test(m"Cross product of a tensor of quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Inch, 3*Inch, 6*Inch)
        v1.cross(v2)
      . assert(_ == Tensor(9*Inch, -28*Inch, 11*Inch))

      test(m"Sum of two tensors of quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Inch, 3*Inch, 6*Inch)
        v1 + v2
      . assert(_ == Tensor(7*Inch, 5*Inch, 7*Inch))

      test(m"Sum of two tensors of different quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Metre, 3*Metre, 6*Metre)

        val sum = v1 + v1
      . assert()

    suite(m"Row and column extraction"):
      val mat = Matrix[2, 3]((1, 2, 3), (4, 5, 6))

      test(m"row(0) of a 2x3 matrix"):
        mat.row(0)
      . assert(_ == Tensor(1, 2, 3))

      test(m"row(1) of a 2x3 matrix"):
        mat.row(1)
      . assert(_ == Tensor(4, 5, 6))

      test(m"column(0) of a 2x3 matrix"):
        mat.column(0)
      . assert(_ == Tensor(1, 4))

      test(m"column(2) of a 2x3 matrix"):
        mat.column(2)
      . assert(_ == Tensor(3, 6))

      test(m"row size matches column count"):
        mat.row(0).size
      . assert(_ == 3)

      test(m"column size matches row count"):
        mat.column(0).size
      . assert(_ == 2)

    suite(m"Transpose"):
      test(m"Transpose of 2x3 matrix"):
        Matrix[2, 3]((1, 2, 3), (4, 5, 6)).transpose
      . assert(_ == Matrix[3, 2]((1, 4), (2, 5), (3, 6)))

      test(m"Transpose round-trip is identity"):
        val mat = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
        val roundTripped = mat.transpose.transpose
        roundTripped == mat
      . assert(_ == true)

      test(m"Determinant invariant under transpose"):
        val mat = Matrix[3, 3]((2, -3, 1), (2, 0, -1), (1, 4, 5))
        mat.determinant - mat.transpose.determinant
      . assert(_ == 0)

    suite(m"Trace"):
      test(m"Trace of 3x3 matrix"):
        Matrix[3, 3]((1, 2, 3), (4, 5, 6), (7, 8, 10)).trace
      . assert(_ == 16)

      test(m"Trace of 2x2 zero is zero"):
        Matrix[2, 2]((0, 0), (0, 0)).trace
      . assert(_ == 0)

      test(m"Trace of identity matrix is dimension"):
        Matrix.identity[Int, 4].trace
      . assert(_ == 4)

    suite(m"Minor and cofactor"):
      val mat = Matrix[3, 3]((1, 2, 3), (4, 5, 6), (7, 8, 10))

      test(m"Minor at (0, 0) is determinant of submatrix"):
        mat.minor(0, 0)
      . assert(_ == 2)

      test(m"Minor at (0, 1)"):
        mat.minor(0, 1)
      . assert(_ == -2)

      test(m"Minor at (1, 2)"):
        mat.minor(1, 2)
      . assert(_ == -6)

      test(m"Cofactor at (0, 0) matches its minor"):
        mat.cofactor(0, 0)
      . assert(_ == 2)

      test(m"Cofactor at (0, 1) flips sign"):
        mat.cofactor(0, 1)
      . assert(_ == 2)

      test(m"Cofactor at (1, 2) flips sign"):
        mat.cofactor(1, 2)
      . assert(_ == 6)

    suite(m"Adjugate"):
      test(m"Adjugate of 2x2 matrix"):
        Matrix[2, 2]((1, 2), (3, 4)).adjugate
      . assert(_ == Matrix[2, 2]((4, -2), (-3, 1)))

      test(m"Adjugate of 3x3 matrix"):
        Matrix[3, 3]((1, 2, 3), (4, 5, 6), (7, 8, 10)).adjugate
      . assert(_ == Matrix[3, 3]((2, 4, -3), (2, -11, 6), (-3, 6, -3)))

      test(m"M * adjugate(M) = det(M) * I (3x3)"):
        val mat = Matrix[3, 3]((1, 2, 3), (4, 5, 6), (7, 8, 10))
        mat*mat.adjugate
      . assert(_ == Matrix[3, 3]((-3, 0, 0), (0, -3, 0), (0, 0, -3)))

    suite(m"Matrix addition and subtraction"):
      test(m"Add two 2x3 matrices"):
        Matrix[2, 3]((1, 2, 3), (4, 5, 6)) + Matrix[2, 3]((6, 5, 4), (3, 2, 1))
      . assert(_ == Matrix[2, 3]((7, 7, 7), (7, 7, 7)))

      test(m"Subtract two 2x3 matrices"):
        Matrix[2, 3]((10, 9, 8), (7, 6, 5)) - Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      . assert(_ == Matrix[2, 3]((9, 7, 5), (3, 1, -1)))

      test(m"Add matrices of quantities"):
        Matrix[2, 2]((1.0*Metre, 2.0*Metre), (3.0*Metre, 4.0*Metre))
        + Matrix[2, 2]((5.0*Metre, 6.0*Metre), (7.0*Metre, 8.0*Metre))
      . assert(_ == Matrix[2, 2]((6.0*Metre, 8.0*Metre), (10.0*Metre, 12.0*Metre)))

    suite(m"Matrix-vector multiplication"):
      test(m"2x3 matrix times 3-vector"):
        Matrix[2, 3]((1, 2, 3), (4, 5, 6))*Tensor(7, 8, 9)
      . assert(_ == Tensor(50, 122))

      test(m"Identity matrix times vector is the vector"):
        Matrix.identity[Int, 3]*Tensor(2, 3, 5)
      . assert(_ == Tensor(2, 3, 5))

    suite(m"Identity and zero constructors"):
      test(m"Identity 3x3 of Int"):
        Matrix.identity[Int, 3]
      . assert(_ == Matrix[3, 3]((1, 0, 0), (0, 1, 0), (0, 0, 1)))

      test(m"Identity 2x2 of Double"):
        Matrix.identity[Double, 2]
      . assert(_ == Matrix[2, 2]((1.0, 0.0), (0.0, 1.0)))

      test(m"Zero 2x3 of Double"):
        Matrix.zero[Double, 2, 3]
      . assert(_ == Matrix[2, 3]((0.0, 0.0, 0.0), (0.0, 0.0, 0.0)))

      test(m"M * identity equals M"):
        val mat = Matrix[3, 3]((1, 2, 3), (4, 5, 6), (7, 8, 9))
        val product = mat*Matrix.identity[Int, 3]
        product == mat
      . assert(_ == true)
