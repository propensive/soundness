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

object `package` {
  implicit class MdStringContext(sc: StringContext) {
    val md: Document = Prefix(MdInterpolator, sc)
  }
}
