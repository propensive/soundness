package escritoire

import rudiments.*
import gossamer.*
import vacuous.*
import anticipation.*

trait ColumnSizing:
  def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int]
  def fit[TextType: Textual: ClassTag](lines: IArray[TextType], width: Int): IArray[TextType]

package columnSizing:
  object Prose extends ColumnSizing:
    def width[TextType](lines: IArray[TextType], maxWidth: Int, slack: Double)
        (using textual: Textual[TextType])
          : Optional[Int] =

      def longestWord(text: TextType, pos: Int, lastStart: Int, max: Int): Int =
        if pos < text.length then
          if summon[Textual[TextType]].unsafeChar(text, pos) == ' '
          then longestWord(text, pos + 1, pos + 1, max.max(pos - lastStart))
          else longestWord(text, pos + 1, lastStart, max)
        else max.max(pos - lastStart)
      
      lines.map(longestWord(_, 0, 0, 0)).max.max((slack*maxWidth).toInt)
    
    def fit[TextType: ClassTag](lines: IArray[TextType], width: Int)(using textual: Textual[TextType])
          : IArray[TextType] =
      
      def format(text: TextType, pos: Int, lineStart: Int, lastSpace: Int, lines: List[TextType])
            : List[TextType] =

        if pos < text.length then
          if textual.unsafeChar(text, pos) == ' '
          then format(text, pos + 1, lineStart, pos, lines)
          else if pos - lineStart >= width
          then format(text, pos + 1, lastSpace + 1, lastSpace, text.slice(lineStart, lastSpace) :: lines)
          else format(text, pos + 1, lineStart, lastSpace, lines)
        else if lineStart == pos then lines else text.slice(lineStart, pos) :: lines
      
      lines.flatMap(format(_, 0, 0, 0, Nil).reverse)


  case class Fixed(fixedWidth: Int, ellipsis: Text = t"…") extends ColumnSizing:
    def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int] =
      fixedWidth
    
    def fit[TextType](lines: IArray[TextType], width: Int)(using textual: Textual[TextType]): IArray[TextType] =
      given ClassTag[TextType] = summon[Textual[TextType]].classTag
      lines.map: line =>
        if line.length > width then line.take(width - ellipsis.length)+textual.make(ellipsis.s) else line

  case class Shorted(fixedWidth: Int, ellipsis: Text = t"…") extends ColumnSizing:
    def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int] =
      val naturalWidth = lines.map(_.length).max
      (maxWidth*slack).toInt.min(naturalWidth)
    
    def fit[TextType](lines: IArray[TextType], width: Int)(using textual: Textual[TextType]): IArray[TextType] =
      given ClassTag[TextType] = summon[Textual[TextType]].classTag
      lines.map: line =>
        if line.length > width then line.take(width - ellipsis.length)+textual.make(ellipsis.s) else line

  case class Collapsible(threshold: Double) extends ColumnSizing:
    def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int] =
      if slack > threshold then lines.map(_.length).max else Unset

    def fit[TextType: Textual](lines: IArray[TextType], width: Int): IArray[TextType] = lines
      
