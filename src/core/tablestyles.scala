package escritoire

package tableStyles:
  given default: TableStyle =
    TableStyle(1, '│', '│', '│', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '─', '─', '─')
 
  given horizontal: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ', ' ', '─', ' ', '─', '─', '─')
 
  given minimalist: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ')
 
  given horizontalGaps: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '─', '─', '─')
 
  given horizontalDots: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', '╌', ' ', ' ', '╌', ' ', ' ', '╌', ' ', '╌', '╌', '╌')
 
  given doubled: TableStyle =
    TableStyle(1, '║', '│', '║', '╔', '╤', '╗', '╚', '╧', '╝', '╟', '┼', '╢', '═', '─', '═')
 
  given rounded: TableStyle =
    TableStyle(1, '│', '│', '│', '╭', '┬', '╮', '╰', '┴', '╯', '├', '┼', '┤', '─', '─', '─')
 
  given dotted: TableStyle =
    TableStyle(1, '┊', '┊', '┊', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '╌', '╌', '╌')
 
  given outline: TableStyle =
    TableStyle(1, '┊', '┊', '┊', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '╌', '╌', '╌')
 
  given ascii: TableStyle =
    TableStyle(1, '|', '|', '|', '+', '+', '+', '+', '+', '+', '+', '+', '+', '-', '-', '-')
 
  given borderless: TableStyle =
    TableStyle(0, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

case class TableStyle
    ( pad:         Int,
      left:        Char,
      sep:         Char,
      right:       Char,
      topLeft:     Char,
      topSep:      Char,
      topRight:    Char,
      bottomLeft:  Char,
      bottomSep:   Char,
      bottomRight: Char,
      midLeft:     Char,
      midSep:      Char,
      midRight:    Char,
      topBar:      Char,
      midBar:      Char,
      bottomBar:   Char ):

  def cost(columns: Int): Int = columns*pad*2 + columns + 1
