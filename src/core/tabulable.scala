package escritoire

import gossamer.*

extension [RowType](data: Seq[RowType])
  def table[TextType](using textual: Textual[TextType], tabulable: Tabulable[RowType, TextType])
        : Tabulation[TextType] =

    tabulable.tabulate(data)

trait Tabulable[RowType, TextType]:
  def table(): Table[RowType, TextType]
  private lazy val tableValue: Table[RowType, TextType] = table()
  def tabulate(data: Seq[RowType]): Tabulation[TextType] = tableValue.tabulate(data)
