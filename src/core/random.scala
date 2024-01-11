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

import scala.util as su

import java.util as ju
import java.security as js

case class Seed(value: IArray[Byte])

trait RandomNumberGenerator:
  def make(): su.Random

package randomNumberGenerators:
  given unseeded: RandomNumberGenerator = () => su.Random(java.util.Random())
  given secureUnseeded: RandomNumberGenerator = () => su.Random(js.SecureRandom())
  given stronglySecure: RandomNumberGenerator = () => su.Random(js.SecureRandom.getInstanceStrong().nn)
  
  given seeded(using seed: Seed): RandomNumberGenerator = () =>
    su.Random(ju.Random(seed.value.foldLeft(0L)(_ << 8 | _ & 0xff)))
  
  given secureSeeded(using seed: Seed): RandomNumberGenerator = () =>
    su.Random(js.SecureRandom(seed.value.to(Array)))

object Randomizable extends Derivation[Randomizable]:
  given int: Randomizable[Int] = _.toInt
  given long: Randomizable[Long] = identity(_)
  given char: Randomizable[Char] = _.toChar
  given boolean: Randomizable[Boolean] = _ < 0L
  given double(using distribution: Distribution): Randomizable[Double] = distribution.transform(_)

  inline def join[DerivationType <: Product: ProductReflection]: Randomizable[DerivationType] = gen =>
    construct:
      [FieldType] => randomizable => randomizable.from(gen)

  inline def split[DerivationType: SumReflection]: Randomizable[DerivationType] = gen =>
    delegate(variantLabels(gen.toInt.abs%variantLabels.length)):
      [VariantType <: DerivationType] => randomizable => randomizable.from(gen)

// Note that `gen` is side-effecting, and is therefore not deterministic in concurrent environments
trait Randomizable[+ValueType]:
  def from(gen: => Long): ValueType

object Random:
  lazy val generalPurpose: Random =
    import randomNumberGenerators.unseeded
    Random()

  def apply()(using generator: RandomNumberGenerator): Random = new Random(generator.make())

class Random(private val rng: su.Random):
  def apply[ValueType]()(using randomizable: Randomizable[ValueType]): ValueType =
    randomizable.from(rng.nextLong())

  def gaussian(): Double = rng.nextGaussian()
  def unitInterval(): Double = rng.nextDouble()

  transparent inline def shuffle[ElementType](seq: Seq[ElementType]): Seq[ElementType] = rng.shuffle(seq)

def random[ValueType: Randomizable](): ValueType = Random.generalPurpose[ValueType]()

package randomDistributions:
  given gaussian: Distribution = Gaussian()
  given uniformUnitInterval: Distribution = UniformDistribution(0, 1)
  given uniformSymmetricUnitInterval: Distribution = UniformDistribution(-1, 1)
  given binary: Distribution = java.lang.Double.longBitsToDouble(_)

trait Distribution:
  def transform(gen: => Long): Double

case class UniformDistribution(start: Double, end: Double) extends Distribution:
  def transform(gen: => Long): Double = ((gen.toDouble/2)/Long.MaxValue)*(end - start) + (end + start)/2

case class Gaussian(mean: Double = 0.0, standardDeviation: Double = 1.0) extends Distribution:
  def transform(gen: => Long): Double =
    val u0 = randomDistributions.uniformUnitInterval.transform(gen)
    val u1 = randomDistributions.uniformUnitInterval.transform(gen)
    
    standardDeviation*(-2*log(u0)).sqrt*cos(2*π*u1) + mean
    
case class PolarGaussian(mean: Double = 0.0, standardDeviation: Double = 1.0) extends Distribution:
  def transform(gen: => Long): Double =
    @annotation.tailrec
    def recur(): Double =
      val u0 = randomDistributions.uniformSymmetricUnitInterval.transform(gen)
      val u1 = randomDistributions.uniformSymmetricUnitInterval.transform(gen)
      val s = u0*u0 + u1*u1
      if s >= 1 || s == 0 then recur() else standardDeviation*u0*(-2*log(s)/s).sqrt + mean

    recur()

object Gamma:
  def approximate(mean: Double, variance: Double): Gamma =
    val scale: Double = variance/mean
    val shape: Int = (mean/scale + 0.5).toInt
    Gamma(shape, scale)

case class Gamma(shape: Int, scale: Double) extends Distribution:
  def mean: Double = shape*scale
  def variance: Double = shape*scale*scale
  def variationCoefficient: Double = shape ** -0.5
  def skewness: Double = 2.0*variationCoefficient

  def transform(gen: => Long): Double =
    def accumulate(sum: Double, count: Int): Double =
      if count == 0 then sum*scale else
        val gaussian = randomDistributions.gaussian.transform(gen)
        accumulate(sum + gaussian*gaussian, count - 1)
    
    accumulate(0.0, shape)
