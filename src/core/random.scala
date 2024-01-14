/*
    Capricious, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package capricious

import wisteria.*
import hypotenuse.*
import rudiments.*

import scala.util as su
import scala.compiletime.*

import java.util as ju
import java.security as js

import language.experimental.genericNumberLiterals

object Seed:
  def apply(long: Long): Seed = Seed(long.bits.bytes)

case class Seed(value: Bytes):
  def entropy: Int = value.length*8
  def long: Long = Long(value)

trait RandomNumberGenerator:
  def make(): su.Random

package randomNumberGenerators:
  given unseeded: RandomNumberGenerator = () => su.Random(java.util.Random())
  given secureUnseeded: RandomNumberGenerator = () => su.Random(js.SecureRandom())
  given stronglySecure: RandomNumberGenerator = () => su.Random(js.SecureRandom.getInstanceStrong().nn)
  given seeded(using seed: Seed): RandomNumberGenerator = () => su.Random(ju.Random(seed.long))
  given secureSeeded(using seed: Seed): RandomNumberGenerator = () => su.Random(js.SecureRandom(seed.value.to(Array)))

object Arbitrary extends Derivation[Arbitrary]:
  given int: Arbitrary[Int] = _.long().toInt
  given long: Arbitrary[Long] = _.long()
  given char: Arbitrary[Char] = _.long().toChar
  given seed: Arbitrary[Seed] = _.long().pipe(Seed(_))
  given boolean: Arbitrary[Boolean] = _.long() < 0L
  given double(using distribution: Distribution): Arbitrary[Double] = distribution.transform(_)

  inline def join[DerivationType <: Product: ProductReflection]: Arbitrary[DerivationType] = random =>
    given seed: Seed = random[Seed]()
    stochastic(using summonInline[RandomNumberGenerator]):
      construct { [FieldType] => arbitrary => arbitrary.from(summon[Random]) }

  inline def split[DerivationType: SumReflection]: Arbitrary[DerivationType] = random =>
    given seed: Seed = random[Seed]()
    stochastic(using summonInline[RandomNumberGenerator]):
      delegate(variantLabels(random.long().abs.toInt%variantLabels.length)):
        [VariantType <: DerivationType] => arbitrary => arbitrary.from(summon[Random])

trait Arbitrary[+ValueType]:
  def apply()(using random: Random): ValueType = from(random)
  def from(random: Random): ValueType

object Random:
  lazy val global: Random = new Random(randomNumberGenerators.unseeded.make())
  def apply(seed: Seed)(using generator: RandomNumberGenerator): Random = new Random(generator.make())

class Random(private val generator: su.Random):
  def long(): Long = generator.nextLong()
  def gaussian(): Double = generator.nextGaussian()
  def unitInterval(): Double = generator.nextDouble()
  def apply[ValueType]()(using arbitrary: Arbitrary[ValueType]): ValueType = arbitrary.from(this)

  transparent inline def shuffle[ElementType](seq: Seq[ElementType]): Seq[ElementType] = generator.shuffle(seq)

def stochastic[ResultType](using generator: RandomNumberGenerator)(block: Random ?=> ResultType): ResultType =
  block(using new Random(generator.make()))

def arbitrary[ValueType]()(using Random)(using arbitrary: Arbitrary[ValueType]): ValueType =
  arbitrary()

def random[ValueType: Arbitrary]()(using arbitrary: Arbitrary[ValueType]): ValueType =
  given Random = Random.global
  arbitrary()
  

package randomDistributions:
  given gaussian: Distribution = Gaussian()
  given uniformUnitInterval: Distribution = UniformDistribution(0, 1)
  given uniformSymmetricUnitInterval: Distribution = UniformDistribution(-1, 1)
  given binary: Distribution = random => Double(random.long())

trait Distribution:
  def transform(random: Random): Double

case class UniformDistribution(start: Double, end: Double) extends Distribution:
  def transform(random: Random): Double = ((random.long().toDouble/2)/Long.MaxValue)*(end - start) + (end + start)/2

case class Gaussian(mean: Double = 0.0, standardDeviation: Double = 1.0) extends Distribution:
  def transform(random: Random): Double =
    val u0 = randomDistributions.uniformUnitInterval.transform(random)
    val u1 = randomDistributions.uniformUnitInterval.transform(random)
    
    (-log(u0).sqrt*cos(2*π*u1)*2*standardDeviation + mean).double
    
case class PolarGaussian(mean: Double = 0.0, standardDeviation: Double = 1.0) extends Distribution:
  def transform(random: Random): Double =
    @annotation.tailrec
    def recur(): F64 =
      val u0: F64 = F64(randomDistributions.uniformSymmetricUnitInterval.transform(random))
      val u1: F64 = F64(randomDistributions.uniformSymmetricUnitInterval.transform(random))
      val s: F64 = hyp(u0, u1)
      if s >= 1 || s == 0 then recur() else F64(standardDeviation)*u0*(-log(s)/s).sqrt*2 + mean

    recur().double

object Gamma:
  def approximate(mean: Double, variance: Double): Gamma =
    val scale: Double = variance/mean
    val shape: Int = (mean/scale + 0.5).toInt
    Gamma(shape, scale)

case class Gamma(shape: Int, scale: Double) extends Distribution:
  def mean: F64 = F64(shape*scale)
  def variance: F64 = mean*scale
  def variationCoefficient: F64 = F64(shape) ** -0.5
  def skewness: F64 = variationCoefficient*2.0

  def transform(random: Random): Double =
    def accumulate(sum: Double, count: Int): Double =
      if count == 0 then sum*scale else
        val gaussian = randomDistributions.gaussian.transform(random)
        accumulate(sum + gaussian*gaussian, count - 1)
    
    accumulate(0.0, shape)
