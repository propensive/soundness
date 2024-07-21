package harlequin

import anticipation.*

sealed trait ProgrammingLanguage:
  def highlight(text: Text): SourceCode = SourceCode(this, text)

object Scala extends ProgrammingLanguage
object Java extends ProgrammingLanguage
