package cosmopolite

import languages.common.*

import probably.*

object Tests extends Suite("Cosmopolite Tests"):
  def run(using Runner): Unit =
    test("extract language from string (English)") {
      val two = en"two" & fr"deux"
      two[En]
    }.assert(_ == "two")
    
    test("extract language from string (French)") {
      val two = en"two" & fr"deux"
      two[Fr]
    }.assert(_ == "deux")
    
    test("extract default language") {
      val two = en"two" & fr"deux"
      given Language[Fr] = Language[Fr]("fr")
      two()
    }.assert(_ == "deux")