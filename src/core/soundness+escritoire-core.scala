package soundness

export escritoire.{Attenuation, BoxDrawing, BoxLine, Breaks, Column, ColumnAlignment, Columnar,
    Grid, LineCharset, Table, TableCell, TableError, TableRelabelling, TableRow, TableSection,
    TableStyle, Tabulable, Tabulation, TextAlignment, VerticalAlignment, table}

package columnAttenuation:
  export escritoire.columnAttenuation.{fail, ignore}

package tableStyles:
  export escritoire.tableStyles.{default, thinRounded, horizontal, midOnly, vertical, minimal}

package columnar:
  export escritoire.columnar.{Prose, Fixed, Shortened, Collapsible}
