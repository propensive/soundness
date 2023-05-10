package rudiments

object Printable:
  given text: Printable[Text] = identity(_)

@capability
trait Printable[-TextType]:
  def print(text: TextType): Text

