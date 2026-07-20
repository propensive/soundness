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

import soundness.*
import textMetrics.uniformMetric

given Decimalizer(4)

object Tests extends Suite(m"Mosquito tests"):
  def run(): Unit =
    test(m"Create a Vector of Ints"):
      Vector(1, 2, 3)
    . assert(_ == Vector(1, 2, 3))

    test(m"A Vector of Ints infers the correct size"):
      demilitarize:
        val vector: Vector[Int, 3] = Vector(1, 3, 4)
      .map(_.message)
    . assert(_ == Nil)

    test(m"Type error if size is incorrect"):
      demilitarize:
        val vector: Vector[Int, 2] = Vector(1, 3, 4)
    . assert(_.nonEmpty)

    test(m"Type error if type is incorrect"):
      demilitarize:
        val vector: Vector[String, 3] = Vector(1, 3, 4)
    . assert(_.nonEmpty)

    test(m"Calculate integer dot-product"):
      Vector(1, 2, 3).dot(Vector(4, 3, 7))
    . assert(_ == 31)

    test(m"Calculate Double dot-product"):
      Vector(0.1, 0.2, 0.3).dot(Vector(0.4, 0.3, 0.7))
    . assert(_ === 0.31 +/- 0.000001)

    test(m"Calculate integer cross-product"):
      Vector(1, 2, 3).cross(Vector(4, 3, 7))
    . assert(_ == Vector(5, 5, -5))

    test(m"Calculate Double cross-product"):
      Vector(1.4, 2.4, 3.8).cross(Vector(4.9, 3.6, 0.7))
    . assert(_ == Vector(-12.0, 17.64, -6.72))

    test(m"Show Vector 3-vector"):
      Vector(1, 3, 6).show
    . assert(_ == t"\u239b 1 \u239e\n\u239c 3 \u239f\n\u239d 6 \u23a0")

    test(m"Show Vector 1-vector"):
      Vector(Mono(42)).show
    . assert(_ == t"( 42 )")

    test(m"Show Vector 2-vector"):
      Vector(1, 2).show
    . assert(_ == t"\u239b 1 \u239e\n\u239d 2 \u23a0")

    test(m"Add two tensors"):
      Vector(1, 2, 3) + Vector(3, 4, 5)
    . assert(_ == Vector(4, 6, 8))

    test(m"Subtract two tensors"):
      Vector(5, 7, 9) - Vector(1, 2, 3)
    . assert(_ == Vector(4, 5, 6))

    test(m"Negate a vector"):
      -Vector(1, -2, 3)
    . assert(_ == Vector(-1, 2, -3))

    suite(m"Element access and conversions"):
      test(m"element(0) on a 3-vector"):
        Vector(10, 20, 30).element(0)
      . assert(_ == 10)

      test(m"element(1) on a 3-vector"):
        Vector(10, 20, 30).element(1)
      . assert(_ == 20)

      test(m"element(2) on a 3-vector"):
        Vector(10, 20, 30).element(2)
      . assert(_ == 30)

      test(m"apply(i) accessor matches element(i)"):
        Vector(10, 20, 30)(1)
      . assert(_ == 20)

      test(m"list conversion"):
        Vector(1, 2, 3).list
      . assert(_ == List(1, 2, 3))

      test(m"iarray conversion"):
        Vector("a", "b", "c").iarray.toList
      . assert(_ == List("a", "b", "c"))

      test(m"size of a 4-vector"):
        Vector(1, 2, 3, 4).size
      . assert(_ == 4)

    suite(m"Map operations"):
      test(m"Map increment over Ints"):
        Vector(1, 2, 3).map(_ + 1)
      . assert(_ == Vector(2, 3, 4))

      test(m"Map type-changing (Int to String)"):
        Vector(1, 2, 3).map(_.toString)
      . assert(_ == Vector("1", "2", "3"))

    suite(m"Vector magnitude operations"):
      test(m"Norm of a 3-4 right triangle"):
        Vector(3.0, 4.0).norm
      . assert(_ === 5.0 +/- 0.000001)

      test(m"Norm of a 2-3-6 vector"):
        Vector(2.0, 3.0, 6.0).norm
      . assert(_ === 7.0 +/- 0.000001)

      test(m"Unitary of a 3-4 vector x-component"):
        Vector(3.0, 4.0).unitary.element(0)
      . assert(_ === 0.6 +/- 0.000001)

      test(m"Unitary of a 3-4 vector y-component"):
        Vector(3.0, 4.0).unitary.element(1)
      . assert(_ === 0.8 +/- 0.000001)

    suite(m"Vector.take and List.slide"):
      test(m"Vector.take takes the first N elements"):
        Vector.take(List(1, 2, 3), 3)
      . assert(_ == Vector(1, 2, 3))

      test(m"Vector.take returns Unset when list is too short"):
        Vector.take(List(1, 2), 3)
      . assert(_ == Unset)

      test(m"Vector.take of size 0 with empty list is defined"):
        Vector.take(Nil: List[Int], 0)
      . assert(_ != Unset)

      test(m"List.slide produces a stream of 2-tensors"):
        List(1, 2, 3, 4).slide(2).to(List)
      . assert(_ == List(Vector(1, 2), Vector(2, 3), Vector(3, 4)))

    suite(m"Tensors of various sizes"):
      test(m"1-vector list"):
        Vector(Mono(42)).list
      . assert(_ == List(42))

      test(m"2-vector list"):
        Vector(1, 2).list
      . assert(_ == List(1, 2))

      test(m"4-vector list"):
        Vector(1, 2, 3, 4).list
      . assert(_ == List(1, 2, 3, 4))

      test(m"5-vector element access"):
        Vector(10, 20, 30, 40, 50).element(4)
      . assert(_ == 50)

      test(m"5-vector size"):
        Vector(10, 20, 30, 40, 50).size
      . assert(_ == 5)

      test(m"4-vector iarray length"):
        Vector("a", "b", "c", "d").iarray.length
      . assert(_ == 4)

    suite(m"Vector identities"):
      val a = Vector(1, 2, 3)
      val b = Vector(4, 5, 6)

      test(m"Cross product is anti-commutative"):
        a.cross(b)
      . assert(_ == -b.cross(a))

      test(m"Dot product of orthogonal unit vectors is zero"):
        Vector(1, 0, 0).dot(Vector(0, 1, 0))
      . assert(_ == 0)

      test(m"Dot product is commutative"):
        a.dot(b)
      . assert(_ == b.dot(a))

    suite(m"Quantity operations"):
      test(m"Add two quantity tensors"):
        Vector(1.0*Metre, 2.0*Metre, 3.0*Metre) + Vector(3.0*Metre, 4.0*Metre, 5.0*Metre)
      . assert(_ == Vector(4.0*Metre, 6.0*Metre, 8.0*Metre))

      test(m"Add two mixed-quantity tensors"):
        Vector(1.0*Foot, 1.0*Foot, 1.0*Foot) + Vector(3.0*Metre, 4.0*Metre, 5.0*Metre)
      . assert(_ == Vector(3.3048*Metre, 4.3048*Metre, 5.3048*Metre))

      test(m"Map from m to m²"):
        Vector(1.0*Metre, 2.0*Metre, 3.0*Metre, 4.0*Metre).map(_*Metre)
      . assert(_ == Vector(1.0*Metre*Metre, 2.0*Metre*Metre, 3.0*Metre*Metre, 4.0*Metre*Metre))

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
      val a7 = Vector(1, 2, 3, 4, 5, 6, 7)
      val b7 = Vector(2, 1, 4, 3, 6, 5, 0)

      test(m"e0 cross e1 = e3"):
        Vector(1, 0, 0, 0, 0, 0, 0).cross(Vector(0, 1, 0, 0, 0, 0, 0))
      . assert(_ == Vector(0, 0, 0, 1, 0, 0, 0))

      test(m"7D cross product is anti-commutative"):
        a7.cross(b7)
      . assert(_ == -b7.cross(a7))

      test(m"7D cross of vector with itself is zero"):
        a7.cross(a7)
      . assert(_ == Vector(0, 0, 0, 0, 0, 0, 0))

      test(m"7D cross product is orthogonal to first operand"):
        a7.dot(a7.cross(b7))
      . assert(_ == 0)

      test(m"7D cross product is orthogonal to second operand"):
        b7.dot(a7.cross(b7))
      . assert(_ == 0)

      test(m"7D Pythagorean identity |a x b|^2 = |a|^2 |b|^2 - (a.b)^2"):
        val ad = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
        val bd = Vector(2.0, 1.0, 4.0, 3.0, 6.0, 5.0, 0.0)
        val cross = ad.cross(bd)
        val crossNormSquared = cross.dot(cross)
        val aDotA = ad.dot(ad)
        val bDotB = bd.dot(bd)
        val aDotB = ad.dot(bd)
        crossNormSquared - (aDotA*bDotB - aDotB*aDotB)
      . assert(d => math.abs(d) < 0.000001)

      test(m"Type error if cross called on 4-vector"):
        demilitarize:
          Vector(1, 2, 3, 4).cross(Vector(5, 6, 7, 8))
      . assert(_.nonEmpty)

    suite(m"Interesting types"):
      test(m"Dot product of a vector of quantities"):
        val v1 = Vector(5*Inch, 2*Inch, Inch)
        val v2 = Vector(2*Inch, 3*Inch, 6*Inch)
        v1.dot(v2)
      . assert(_ == 22*Inch*Inch)

      test(m"Cross product of a vector of quantities"):
        val v1 = Vector(5*Inch, 2*Inch, Inch)
        val v2 = Vector(2*Inch, 3*Inch, 6*Inch)
        v1.cross(v2)
      . assert(_ == Vector(9*Inch, -28*Inch, 11*Inch))

      test(m"Sum of two tensors of quantities"):
        val v1 = Vector(5*Inch, 2*Inch, Inch)
        val v2 = Vector(2*Inch, 3*Inch, 6*Inch)
        v1 + v2
      . assert(_ == Vector(7*Inch, 5*Inch, 7*Inch))

      test(m"Sum of two tensors of different quantities"):
        val v1 = Vector(5*Inch, 2*Inch, Inch)
        val v2 = Vector(2*Metre, 3*Metre, 6*Metre)

        val sum = v1 + v1
      . assert()

    suite(m"Row and column extraction"):
      val mat = Matrix[2, 3]((1, 2, 3), (4, 5, 6))

      test(m"row(0) of a 2x3 matrix"):
        mat.row(0)
      . assert(_ == Vector(1, 2, 3))

      test(m"row(1) of a 2x3 matrix"):
        mat.row(1)
      . assert(_ == Vector(4, 5, 6))

      test(m"column(0) of a 2x3 matrix"):
        mat.column(0)
      . assert(_ == Vector(1, 4))

      test(m"column(2) of a 2x3 matrix"):
        mat.column(2)
      . assert(_ == Vector(3, 6))

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
        Matrix[2, 3]((1, 2, 3), (4, 5, 6))*Vector(7, 8, 9)
      . assert(_ == Vector(50, 122))

      test(m"Identity matrix times vector is the vector"):
        Matrix.identity[Int, 3]*Vector(2, 3, 5)
      . assert(_ == Vector(2, 3, 5))

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

    suite(m"Submatrix"):
      val m3 = Matrix[3, 3]((1, 2, 3), (4, 5, 6), (7, 8, 9))

      test(m"Submatrix removing row 0 column 0"):
        m3.submatrix(0, 0)
      . assert(_ == Matrix[2, 2]((5, 6), (8, 9)))

      test(m"Submatrix removing center"):
        m3.submatrix(1, 1)
      . assert(_ == Matrix[2, 2]((1, 3), (7, 9)))

      test(m"Submatrix removing last row and column"):
        m3.submatrix(2, 2)
      . assert(_ == Matrix[2, 2]((1, 2), (4, 5)))

      test(m"Submatrix of 2x3 matrix has shape 1x2"):
        Matrix[2, 3]((1, 2, 3), (4, 5, 6)).submatrix(0, 1)
      . assert(_ == Matrix[1, 2](Tuple1((4, 6))))

    suite(m"Frobenius norm"):
      test(m"Frobenius norm of [[3, 0], [0, 4]] = 5"):
        Matrix[2, 2]((3.0, 0.0), (0.0, 4.0)).frobeniusNorm
      . assert(_ === 5.0 +/- 0.000001)

      test(m"Frobenius norm of [[1, 2], [3, 4]]"):
        Matrix[2, 2]((1.0, 2.0), (3.0, 4.0)).frobeniusNorm
      . assert(_ === math.sqrt(30.0) +/- 0.000001)

      test(m"Frobenius norm of identity equals sqrt(n)"):
        Matrix.identity[Double, 4].frobeniusNorm
      . assert(_ === 2.0 +/- 0.000001)

    suite(m"Rank"):
      test(m"Rank of 3x3 identity is 3"):
        Matrix.identity[Double, 3].rank
      . assert(_ == 3)

      test(m"Rank of 3x3 zero is 0"):
        Matrix.zero[Double, 3, 3].rank
      . assert(_ == 0)

      test(m"Rank of singular 2x2"):
        Matrix[2, 2]((1.0, 2.0), (2.0, 4.0)).rank
      . assert(_ == 1)

      test(m"Rank of 3x3 matrix with linearly dependent columns"):
        Matrix[3, 3]((1.0, 2.0, 3.0), (4.0, 5.0, 6.0), (7.0, 8.0, 9.0)).rank
      . assert(_ == 2)

      test(m"Rank of non-square 2x3 with full row rank"):
        Matrix[2, 3]((1.0, 0.0, 2.0), (0.0, 1.0, 3.0)).rank
      . assert(_ == 2)

    suite(m"Eigenvalues and eigenvectors"):
      test(m"Eigenvalues of 2x2 diagonal matrix"):
        val sorted =
          Matrix[2, 2]((2.0, 0.0), (0.0, 3.0)).eigenvalues.let(_.list.sorted).vouch

        math.abs(sorted(0) - 2.0) + math.abs(sorted(1) - 3.0)
      . assert(_ < 0.000001)

      test(m"Eigenvalues of [[2, 1], [1, 2]] are 1 and 3"):
        val sorted =
          Matrix[2, 2]((2.0, 1.0), (1.0, 2.0)).eigenvalues.let(_.list.sorted).vouch

        math.abs(sorted(0) - 1.0) + math.abs(sorted(1) - 3.0)
      . assert(_ < 0.000001)

      test(m"Eigenvalues of identity are all 1"):
        val list = Matrix.identity[Double, 3].eigenvalues.let(_.list).vouch
        list.map(v => math.abs(v - 1.0)).max
      . assert(_ < 0.000001)

      test(m"Eigensystem of non-symmetric matrix is Unset"):
        Matrix[2, 2]((1.0, 2.0), (3.0, 4.0)).eigensystem
      . assert(_ == Unset)

      test(m"M * v = lambda * v for each eigenvector"):
        val mat = Matrix[2, 2]((2.0, 1.0), (1.0, 2.0))
        val pair = mat.eigensystem.vouch
        val vals = pair(0)
        val vecs = pair(1)
        val v0 = vecs.column(0)
        val v1 = vecs.column(1)
        val lambda0 = vals(0)
        val lambda1 = vals(1)
        val mv0 = mat*v0
        val mv1 = mat*v1
        val d0 = math.abs(mv0(0) - lambda0*v0(0)) + math.abs(mv0(1) - lambda0*v0(1))
        val d1 = math.abs(mv1(0) - lambda1*v1(0)) + math.abs(mv1(1) - lambda1*v1(1))
        math.max(d0, d1)
      . assert(_ < 0.000001)

      test(m"Eigenvectors are unit vectors"):
        val mat = Matrix[3, 3]((4.0, 1.0, 2.0), (1.0, 5.0, 3.0), (2.0, 3.0, 6.0))
        val vecs = mat.eigenvectors.vouch
        val norms = (0 until 3).map: column =>
          math.abs(vecs.column(column).norm - 1.0)

        norms.max
      . assert(_ < 0.000001)

    suite(m"Solve linear system"):
      test(m"Solve 2x2 system"):
        val mat = Matrix[2, 2]((2.0, 1.0), (1.0, 3.0))
        val rhs = Vector(3.0, 4.0)
        val solution = mat.solve(rhs).vouch
        math.abs(solution(0) - 1.0) + math.abs(solution(1) - 1.0)
      . assert(_ < 0.000001)

      test(m"Solve identity system returns RHS"):
        val solution = Matrix.identity[Double, 3].solve(Vector(7.0, 8.0, 9.0)).vouch
        solution
      . assert(_ == Vector(7.0, 8.0, 9.0))

      test(m"Solve 3x3 system"):
        val mat = Matrix[3, 3]((1.0, 1.0, 1.0), (0.0, 2.0, 5.0), (2.0, 5.0, -1.0))
        val rhs = Vector(6.0, -4.0, 27.0)
        val solution = mat.solve(rhs).vouch
        val expected = Vector(5.0, 3.0, -2.0)
        (0 until 3).map(i => math.abs(solution(i) - expected(i))).max
      . assert(_ < 0.000001)

      test(m"Solve verifies A * x = b"):
        val mat = Matrix[3, 3]((4.0, 1.0, 2.0), (1.0, 5.0, 3.0), (2.0, 3.0, 6.0))
        val rhs = Vector(7.0, 8.0, 9.0)
        val solution = mat.solve(rhs).vouch
        val recovered = mat*solution
        (0 until 3).map(i => math.abs(recovered(i) - rhs(i))).max
      . assert(_ < 0.000001)

      test(m"Solve singular system returns Unset"):
        Matrix[2, 2]((1.0, 2.0), (2.0, 4.0)).solve(Vector(3.0, 6.0))
      . assert(_ == Unset)

      test(m"Solve requires row swap (zero in pivot)"):
        val mat = Matrix[2, 2]((0.0, 1.0), (1.0, 0.0))
        val solution = mat.solve(Vector(2.0, 3.0)).vouch
        math.abs(solution(0) - 3.0) + math.abs(solution(1) - 2.0)
      . assert(_ < 0.000001)
