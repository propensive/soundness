package escapade

import probably.*

import escapes.*

object Tests extends Suite("Escapade tests"):
  def run(using Runner): Unit =
    test("normal string") {
      ansi"hello world".render
    }.assert(_ == "hello world")
    
    test("simple string substitution") {
      ansi"hello ${"world"}".render
    }.assert(_ == "hello world")
    
    test("bold text") {
      ansi"$Bold{bold} text".explicit
    }.assert(_ == "^[1mbold^[22m text")
    
    test("italic text") {
      ansi"$Italic{italic} text".explicit
    }.assert(_ == "^[3mitalic^[23m text")
    
    test("24-bit colored text") {
      ansi"${iridescence.colors.Tan}[text]".explicit
    }.assert(_ == "^[38;2;210;180;139mtext^[39m")
    
    test("non-escape insertion should not parse brackets") {
      val notAnEscape = 42
      ansi"${notAnEscape}[text]".explicit
    }.assert(_ == "42[text]")
