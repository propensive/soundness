package gossamer

import rudiments.*
import hieronymus.*

package textWidthCalculation:
  given eastAsianScripts: TextWidthCalculator = new TextWidthCalculator:
    def width(text: Text): Int = text.displayWidth
    def width(char: Char): Int = char.displayWidth
