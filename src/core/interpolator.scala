/*

    Litterateur, version 0.4.0. Copyright 2019-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package litterateur

import contextual._

object MdInterpolator extends Interpolator {

  sealed trait MdInput extends Context
  case object BlockInput extends MdInput
  case object InlineInput extends MdInput

  type ContextType = MdInput
  type Output = Document
  type Input = String

  def evaluate(interpolation: RuntimeInterpolation): Document = {
    Markdown.parse(interpolation.parts.mkString)
  }

  def contextualize(interpolation: StaticInterpolation): Seq[ContextType] = {
    if(interpolation.parts.length == 1) Nil else interpolation.parts.sliding(2).map {
      case List(Literal(_, left), Literal(_, right)) =>
        if(left.last == '\n' && right.head == '\n') BlockInput else InlineInput
    }.to[Seq]
  }

  implicit val embedStrings = embed[String](
    Case(BlockInput, BlockInput)(identity),
    Case(InlineInput, InlineInput)(identity)
  )
}

object `package` extends InterpolatorPackage

trait InterpolatorPackage {
  implicit class MdStringContext(sc: StringContext) {
    val md = Prefix(MdInterpolator, sc)
  }
}
