package savagery

import probably.*
import gossamer.*

object Tests extends Suite(t"Savagery tests"):
  def run(): Unit =
    test(t"Simple plus sign path"):
      Path().moveTo(0!0).lineUp(2).lineLeft(2).lineUp(1).lineRight(2).lineUp(2).lineRight(1)
          .lineDown(2).lineRight(2).lineDown(1).lineLeft(2).lineDown(2).closed.xml.show
    .assert(_ == t"")