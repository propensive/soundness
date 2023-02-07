package hieronymus

import probably.*
import rudiments.*
import gossamer.*

object Tests extends Suite(t"Hieronymus tests"):
  def run(): Unit =
    test(t"Check narrow character width"):
      'a'.displayWidth
    .assert(_ == 1)

    println(Unicode.eastAsianWidths.size)

    test(t"Check Japanese character width"):
      '身'.displayWidth
    .assert(_ == 2)

    test(t"Check displayWidth of string of Japanese text"):
      t"平ぱ記動テ使村方島おゃぎむ万離ワ学つス携".displayWidth
    .assert(_ == 40)