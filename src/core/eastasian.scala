package hieroglyph

import rudiments.*

package textWidthCalculation:
  given eastAsianScripts: TextWidthCalculator with
    def width(text: Text): Int = text.s.foldLeft(0)(_ + width(_))
    def width(char: Char): Int = char.displayWidth
