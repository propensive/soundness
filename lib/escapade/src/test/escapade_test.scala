                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package escapade

import soundness.*

import strategies.throwUnsafely
import textMetrics.uniform

import WebColors.{Red, Yellow, Green, Blue, Tan}

object Tests extends Suite(m"Escapade tests"):
  def run(): Unit =

    // ─── helpers ──────────────────────────────────────────────────────────

    def emit(teletype: Teletype): Text =
      teletype.render(termcapDefinitions.xtermTrueColor)

    def emit256(teletype: Teletype): Text =
      teletype.render(termcapDefinitions.xterm256)

    def plainRender(teletype: Teletype): Text =
      teletype.render(termcapDefinitions.basic)

    def emulate(teletype: Teletype, width: Int = 80, height: Int = 4): Pty =
      Pty(width, height).consume(emit(teletype))

    // chroma values we'll reference repeatedly
    val red    = Red.chroma
    val yellow = Yellow.chroma
    val green  = Green.chroma
    val blue   = Blue.chroma
    val tan    = Tan.chroma
    val white  = Chroma(255, 255, 255)
    val black  = Chroma(0, 0, 0)

    // ─── interpolator: plain rendering ────────────────────────────────────

    suite(m"Interpolator: plain text"):
      test(m"empty interpolator renders empty text"):
        emit(e"")
      . assert(_ == t"")

      test(m"plain text passes through"):
        emit(e"hello world")
      . assert(_ == t"hello world")

      test(m"plain text plain accessor"):
        e"hello world".plain
      . assert(_ == t"hello world")

      test(m"newline preserved in plain"):
        e"line\nbreak".plain
      . assert(_ == t"line\nbreak")

      test(m"tab preserved in plain"):
        e"|\t|".plain
      . assert(_ == t"|\t|")

      test(m"carriage return preserved in plain"):
        e"a\rb".plain
      . assert(_ == t"a\rb")

      test(m"backspace preserved in plain"):
        e"a\bb".plain
      . assert(_ == t"a\bb")

      test(m"unicode escape converted"):
        e"|@|".plain
      . assert(_ == t"|@|")

      test(m"backslash escape preserved"):
        e"a\\b".plain
      . assert(_ == t"a\\b")

      test(m"dollar escape preserved as literal dollar"):
        e"a$$b".plain
      . assert(_ == t"a$$b")

    // ─── interpolator: substitution ───────────────────────────────────────

    suite(m"Interpolator: substitution"):
      test(m"string substitution"):
        emit(e"hello ${"world"}")
      . assert(_ == t"hello world")

      test(m"text substitution"):
        emit(e"hello ${t"world"}")
      . assert(_ == t"hello world")

      test(m"int substitution"):
        emit(e"answer is ${42}")
      . assert(_ == t"answer is 42")

      test(m"teletype substitution preserves styling"):
        val inner: Teletype = e"$Bold(bold)"
        emulate(e"x${inner}y").buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"non-stylize substitution does not consume bracket"):
        val n = 42
        emit(e"${n}[text]")
      . assert(_ == t"42[text]")

      test(m"non-stylize substitution does not consume paren"):
        val n = 42
        emit(e"${n}(text)")
      . assert(_ == t"42(text)")

      test(m"two adjacent substitutions"):
        emit(e"${"a"}${"b"}")
      . assert(_ == t"ab")

    // ─── interpolator: bracket parsing ────────────────────────────────────

    suite(m"Interpolator: brackets"):
      test(m"parenthesis bracket"):
        emulate(e"$Bold(bold)").buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"square bracket"):
        emulate(e"$Bold[bold]").buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"curly bracket"):
        emulate(e"$Bold{bold}").buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"angle bracket"):
        emulate(e"$Bold<bold>").buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"guillemet bracket"):
        emulate(e"$Bold«bold»").buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"mismatched closing bracket type is ignored as text"):
        emulate(e"$Bold(a]b)").buffer.find(t"a]b").vouch.styles
      . assert(_.all(_.bold))

      test(m"mismatched closing bracket inside parens does not close span"):
        emulate(e"$Bold(a]b)c").buffer.find(t"c").vouch.styles
      . assert(_.all(!_.bold))

    // ─── interpolator: escapes ────────────────────────────────────────────

    suite(m"Interpolator: escapes"):
      test(m"escaped backslash before stylize is preserved"):
        emit(e"\\${Bold}(text)").contains(t"\\")
      . aspire(_ == false)

      test(m"escape before stylize cancels the markup"):
        // Per Ansi.parse: a backslash after a markup transform skips the
        // transform's bracket parsing, so the bracket stays as text.
        val out = emulate(e"x\\${Bold}(text)").buffer
        out.style(0.z, 0.z).bold
      . aspire(_ == false)



    // ─── interpolator: compile errors ─────────────────────────────────────

    suite(m"Interpolator: compile errors"):
      test(m"unclosed paren is a compile error"):
        demilitarize:
          e"$Bold(text"
        .  nonEmpty
      . aspire(_ == true)

      test(m"unclosed square bracket is a compile error"):
        demilitarize:
          e"$Bold[text"
        .  nonEmpty
      . aspire(_ == true)

      test(m"unclosed brace is a compile error"):
        demilitarize:
          e"$Bold{text"
        .  nonEmpty
      . aspire(_ == true)

      test(m"unclosed angle bracket is a compile error"):
        demilitarize:
          e"$Bold<text"
        .  nonEmpty
      . aspire(_ == true)

      test(m"properly closed parens compile cleanly"):
        demilitarize:
          e"$Bold(text)"
        .  isEmpty
      . assert(_ == true)

    // ─── style markers (one per marker) ───────────────────────────────────

    suite(m"Style markers: bold"):
      test(m"bold cells are bold"):
        emulate(e"a$Bold(b)c").buffer.style(1.z, 0.z).bold
      . assert(_ == true)

      test(m"text before bold is not bold"):
        emulate(e"a$Bold(b)c").buffer.style(0.z, 0.z).bold
      . assert(_ == false)

      test(m"text after bold is not bold"):
        emulate(e"a$Bold(b)c").buffer.style(2.z, 0.z).bold
      . assert(_ == false)

    suite(m"Style markers: italic"):
      test(m"italic cells are italic"):
        emulate(e"a$Italic(b)c").buffer.style(1.z, 0.z).italic
      . assert(_ == true)

      test(m"text before italic is not italic"):
        emulate(e"a$Italic(b)c").buffer.style(0.z, 0.z).italic
      . assert(_ == false)

      test(m"text after italic is not italic"):
        emulate(e"a$Italic(b)c").buffer.style(2.z, 0.z).italic
      . assert(_ == false)

    suite(m"Style markers: underline"):
      test(m"underline cells are underlined"):
        emulate(e"a$Underline(b)c").buffer.style(1.z, 0.z).underline
      . assert(_ == true)

      test(m"text after underline is not underlined"):
        emulate(e"a$Underline(b)c").buffer.style(2.z, 0.z).underline
      . assert(_ == false)

    suite(m"Style markers: strike"):
      test(m"strike cells are struck"):
        emulate(e"a$Strike(b)c").buffer.style(1.z, 0.z).strike
      . assert(_ == true)

      test(m"text after strike is not struck"):
        emulate(e"a$Strike(b)c").buffer.style(2.z, 0.z).strike
      . assert(_ == false)

    suite(m"Style markers: reverse"):
      test(m"reverse cells are reversed"):
        emulate(e"a$Reverse(b)c").buffer.style(1.z, 0.z).reverse
      . assert(_ == true)

      test(m"text after reverse is not reversed"):
        emulate(e"a$Reverse(b)c").buffer.style(2.z, 0.z).reverse
      . assert(_ == false)

    suite(m"Style markers: conceal"):
      test(m"conceal cells are concealed"):
        emulate(e"a$Conceal(b)c").buffer.style(1.z, 0.z).conceal
      . assert(_ == true)

      test(m"text after conceal is not concealed"):
        emulate(e"a$Conceal(b)c").buffer.style(2.z, 0.z).conceal
      . assert(_ == false)

    // ─── foreground colors ────────────────────────────────────────────────

    suite(m"Foreground colors"):
      test(m"24-bit color via Fg(Tan.chroma)"):
        emulate(e"${Fg(tan)}(text)").buffer.style(0.z, 0.z).foreground
      . assert(_ == tan)

      test(m"24-bit Red foreground"):
        emulate(e"${Fg(red)}(text)").buffer.style(0.z, 0.z).foreground
      . assert(_ == red)

      test(m"24-bit Yellow foreground"):
        emulate(e"${Fg(yellow)}(text)").buffer.style(0.z, 0.z).foreground
      . assert(_ == yellow)

      test(m"foreground via raw Chroma"):
        emulate(e"${Fg(Chroma(64, 128, 192))}(text)").buffer.style(0.z, 0.z).foreground
      . assert(_ == Chroma(64, 128, 192))

      test(m"foreground reverts to default after span"):
        emulate(e"${Fg(red)}(red)plain").buffer.style(3.z, 0.z).foreground
      . assert(_ == white)

      test(m"text before colored span has default foreground"):
        emulate(e"x${Fg(red)}(red)").buffer.style(0.z, 0.z).foreground
      . assert(_ == white)

    // ─── background colors ────────────────────────────────────────────────

    suite(m"Background colors"):
      test(m"24-bit Red background"):
        emulate(e"${Bg(red)}(text)").buffer.style(0.z, 0.z).background
      . assert(_ == red)

      test(m"background reverts to default after span"):
        emulate(e"${Bg(red)}(red)plain").buffer.style(3.z, 0.z).background
      . assert(_ == black)

      test(m"foreground and background combine"):
        val pty = emulate(e"${Fg(red)}(${Bg(blue)}(text))")
        (pty.buffer.style(0.z, 0.z).foreground, pty.buffer.style(0.z, 0.z).background)
      . assert(_ == ((red, blue)))

      test(m"Bg.highContrast is dark on light"):
        val brightWhite = Bg(Chroma(255, 255, 255)).highContrast.color
        brightWhite.underlying
      . assert(_ == 0)

      test(m"Bg.highContrast is light on dark"):
        val darkBlack = Bg(Chroma(0, 0, 0)).highContrast.color
        darkBlack.underlying
      . assert(_ == 16777215)

    // ─── color and style nesting (stack restoration) ──────────────────────

    suite(m"Nesting: stack restoration"):
      test(m"inner color overrides outer"):
        emulate(e"${Fg(red)}(${Fg(yellow)}(yellow))").buffer.find(t"yellow").vouch.styles
      . assert(_.all(_.foreground == yellow))

      test(m"outer color restored after inner closes"):
        emulate(e"${Fg(red)}(${Fg(yellow)}(yellow)red)").buffer.find(t"red").vouch.styles
      . assert(_.all(_.foreground == red))

      test(m"default restored after outermost closes"):
        emulate(e"${Fg(red)}(${Fg(yellow)}(yellow)red)tail").buffer.find(t"tail").vouch.styles
      . assert(_.all(_.foreground == white))

      test(m"three levels of nested color restore correctly"):
        emulate(e"${Fg(red)}(${Fg(yellow)}(${Fg(green)}(g)y)r)x").buffer.find(t"x").vouch.styles
      . assert(_.all(_.foreground == white))

      test(m"middle color of three-deep stack restores correctly"):
        emulate(e"${Fg(red)}(${Fg(yellow)}(${Fg(green)}(g)y)r)").buffer.find(t"y").vouch.styles
      . assert(_.all(_.foreground == yellow))

      test(m"outer color of three-deep stack restores correctly"):
        emulate(e"${Fg(red)}(${Fg(yellow)}(${Fg(green)}(g)y)r)").buffer.find(t"r").vouch.styles
      . assert(_.all(_.foreground == red))

      test(m"nested bold and italic both apply"):
        emulate(e"$Bold(b$Italic(bi)b)").buffer.find(t"bi").vouch.styles
      . assert(_.all { s => s.bold && s.italic })

      test(m"italic removed after inner span ends but bold remains"):
        emulate(e"$Bold(b$Italic(bi)b2)").buffer.find(t"b2").vouch.styles
      . assert(_.all { s => s.bold && !s.italic })

      test(m"nested background restores outer background"):
        emulate(e"${Bg(red)}(${Bg(blue)}(blue)red)").buffer.find(t"red").vouch.styles
      . assert(_.all(_.background == red))

      test(m"nested fg/bg combinations restore independently"):
        val out = emulate(e"${Fg(red)}(${Bg(blue)}(both)fg)tail")
        val tailStyle = out.buffer.style(8.z, 0.z)
        (tailStyle.foreground, tailStyle.background)
      . assert(_ == ((white, black)))

    // ─── escaping (TextEscapes) ───────────────────────────────────────────

    suite(m"Escaping"):
      test(m"\\n produces a newline character"):
        e"\n".plain
      . assert(_ == t"\n")

      test(m"\\t produces a tab character"):
        e"\t".plain
      . assert(_ == t"\t")

      test(m"\\r produces a carriage return"):
        e"\r".plain
      . assert(_ == t"\r")

      test(m"\\u escape converts to character"):
        e"A".plain
      . assert(_ == t"A")

      test(m"\\\\ produces a literal backslash"):
        e"\\".plain
      . assert(_ == t"\\")

    // ─── Teletype direct API ──────────────────────────────────────────────

    suite(m"Teletype: render"):
      test(m"empty Teletype renders to empty text"):
        Teletype.empty.render(termcapDefinitions.xtermTrueColor)
      . assert(_ == t"")

      test(m"basic termcap (no ansi) returns plain text"):
        plainRender(e"$Bold(text)")
      . assert(_ == t"text")

      test(m"basic termcap output has no escape characters"):
        plainRender(e"$Bold(${Fg(red)}(text))").contains(t"\u001b")
      . assert(_ == false)

      test(m"xtermTrueColor encodes 24-bit color"):
        emit(e"${Fg(red)}(x)").contains(t"38;2;255;0;0")
      . assert(_ == true)

      test(m"xterm256 encodes palette color"):
        emit256(e"${Fg(red)}(x)").contains(t"38;5;")
      . assert(_ == true)

      test(m"explicit shows escape as \\\\e"):
        e"$Bold(x)".explicit.contains(t"\\e")
      . assert(_ == true)

    suite(m"Teletype: append"):
      test(m"append text grows plain"):
        e"hello".append(t" world").plain
      . assert(_ == t"hello world")

      test(m"append teletype preserves left styling"):
        val left = e"$Bold(bold)"
        emulate(left.append(e"plain")).buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"append teletype preserves right styling"):
        val right = e"$Bold(bold)"
        emulate(e"plain".append(right)).buffer.find(t"bold").vouch.styles
      . assert(_.all(_.bold))

      test(m"+ operator behaves as append"):
        (e"a" + e"b").plain
      . assert(_ == t"ab")

    suite(m"Teletype: dropChars and takeChars"):
      test(m"dropChars from start"):
        e"abcdef".dropChars(2).plain
      . assert(_ == t"cdef")

      test(m"dropChars from end"):
        e"abcdef".dropChars(2, Rtl).plain
      . assert(_ == t"abcd")

      test(m"takeChars from start"):
        e"abcdef".takeChars(3).plain
      . assert(_ == t"abc")

      test(m"takeChars from end"):
        e"abcdef".takeChars(3, Rtl).plain
      . assert(_ == t"def")

      test(m"dropChars preserves styling on remaining range"):
        val tt = e"$Bold(abcdef)".dropChars(2)
        emulate(tt).buffer.find(t"cdef").vouch.styles
      . aspire(_.all(_.bold))

      test(m"takeChars preserves styling on remaining range"):
        val tt = e"$Bold(abcdef)".takeChars(3)
        emulate(tt).buffer.find(t"abc").vouch.styles
      . aspire(_.all(_.bold))

    // ─── Textual extension methods (from gossamer) ────────────────────────

    suite(m"Textual: indexing & length"):
      test(m"length"):
        e"hello".length
      . assert(_ == 5)

      test(m"length on empty"):
        e"".length
      . assert(_ == 0)

      test(m"chars first"):
        e"hello".chars(0)
      . assert(_ == 'h')

      test(m"chars"):
        e"abc".chars.toSeq
      . assert(_ == Seq('a', 'b', 'c'))

    suite(m"Textual: slicing"):
      test(m"keep"):
        e"hello world".keep(5).plain
      . assert(_ == t"hello")

      test(m"keep with Rtl"):
        e"hello world".keep(5, Rtl).plain
      . assert(_ == t"world")

      test(m"skip"):
        e"hello world".skip(6).plain
      . assert(_ == t"world")

      test(m"skip with Rtl"):
        e"hello world".skip(6, Rtl).plain
      . assert(_ == t"hello")

      test(m"tail"):
        e"hello".tail.plain
      . assert(_ == t"ello")

      test(m"init"):
        e"hello".init.plain
      . assert(_ == t"hell")

      test(m"before"):
        e"hello".before(Ter).plain
      . assert(_ == t"he")

      test(m"after"):
        e"hello".after(Ter).plain
      . assert(_ == t"lo")

      test(m"upto"):
        e"hello".upto(Ter).plain
      . assert(_ == t"hel")

      test(m"from"):
        e"hello".from(Ter).plain
      . assert(_ == t"llo")

      test(m"slices"):
        e"abcdefg".slices(3).map(_.plain)
      . assert(_ == List(t"abc", t"def", t"g"))

      test(m"snip"):
        val (a, b) = e"hello world".snip(5)
        (a.plain, b.plain)
      . assert(_ == ((t"hello", t" world")))

      test(m"reverse"):
        e"hello".reverse.plain
      . assert(_ == t"olleh")

    suite(m"Textual: searching"):
      test(m"contains text"):
        e"hello world".contains(t"world")
      . assert(_ == true)

      test(m"does not contain"):
        e"hello".contains(t"world")
      . assert(_ == false)

      test(m"contains char"):
        e"hello".contains('e')
      . assert(_ == true)

      test(m"seek substring (Ltr)"):
        e"hello world".seek(t"world")
      . assert(_ == Sept)

      test(m"seek missing returns Unset"):
        e"hello".seek(t"world")
      . assert(_ == Unset)

      test(m"seek substring Rtl finds last occurrence"):
        e"abcabc".seek(t"a", Rtl)
      . assert(_ == Quat)

      test(m"starts with prefix"):
        e"hello world".starts(t"hello")
      . assert(_ == true)

      test(m"does not start with prefix"):
        e"hello world".starts(t"world")
      . assert(_ == false)

      test(m"ends with suffix"):
        e"hello world".ends(t"world")
      . assert(_ == true)

      test(m"does not end with suffix"):
        e"hello world".ends(t"hello")
      . assert(_ == false)

      test(m"count text occurrences"):
        e"abcabcabc".count(t"a")
      . assert(_ == 3)

      test(m"count predicate"):
        e"hello".count(_ == 'l')
      . assert(_ == 2)

      test(m"where finds first matching index"):
        e"hello".where(_ == 'l')
      . assert(_ == Ter)

      test(m"before predicate"):
        e"hello".before(_ == 'l').plain
      . assert(_ == t"he")

      test(m"upto predicate"):
        e"hello".upto(_ == 'l').plain
      . assert(_ == t"hel")

    suite(m"Textual: case"):
      test(m"lower"):
        e"HELLO".lower.plain
      . assert(_ == t"hello")

      test(m"upper"):
        e"hello".upper.plain
      . assert(_ == t"HELLO")

      test(m"capitalize"):
        e"hello".capitalize.plain
      . assert(_ == t"Hello")

      test(m"uncapitalize"):
        e"Hello".uncapitalize.plain
      . assert(_ == t"hello")

    suite(m"Textual: trim, pad, fit"):
      test(m"trim"):
        e"  hello  ".trim.plain
      . assert(_ == t"hello")

      test(m"trim Ltr"):
        e"  hello  ".trim(Ltr).plain
      . assert(_ == t"hello  ")

      test(m"trim Rtl"):
        e"  hello  ".trim(Rtl).plain
      . assert(_ == t"  hello")

      test(m"strip prefix"):
        e"prehello".strip(t"pre").plain
      . assert(_ == t"hello")

      test(m"strip suffix Rtl"):
        e"hellopost".strip(t"post", Rtl).plain
      . assert(_ == t"hello")

      test(m"pad to width"):
        e"hi".pad(5).plain
      . assert(_ == t"hi   ")

      test(m"pad with custom char"):
        e"hi".pad(5, char = '_').plain
      . assert(_ == t"hi___")

      test(m"pad Rtl"):
        e"hi".pad(5, Rtl).plain
      . assert(_ == t"   hi")

      test(m"center"):
        e"hi".center(6).plain
      . assert(_ == t"  hi  ")

      test(m"fit truncates when too long"):
        e"hello world".fit(5).plain
      . assert(_ == t"hello")

      test(m"fit pads when too short"):
        e"hi".fit(5).plain
      . assert(_ == t"hi   ")

      test(m"blank on empty is true"):
        e"".blank
      . assert(_ == true)

      test(m"blank on whitespace is true"):
        e"   ".blank
      . assert(_ == true)

      test(m"blank on non-blank is false"):
        e"a".blank
      . assert(_ == false)

    suite(m"Textual: split"):
      test(m"words"):
        e"hello world how are you".words.map(_.plain)
      . assert(_ == List(t"hello", t"world", t"how", t"are", t"you"))

      test(m"lines"):
        e"line1\nline2\nline3".lines.map(_.plain)
      . assert(_ == List(t"line1", t"line2", t"line3"))

      test(m"unkebab"):
        e"hello-world-test".unkebab.map(_.plain)
      . assert(_ == List(t"hello", t"world", t"test"))

      test(m"unsnake"):
        e"hello_world_test".unsnake.map(_.plain)
      . assert(_ == List(t"hello", t"world", t"test"))

      test(m"uncamel"):
        e"helloWorldTest".uncamel.map(_.plain)
      . assert(_ == List(t"hello", t"world", t"test"))

    suite(m"Textual: predicates"):
      test(m"keep predicate"):
        e"hello world".keep(_.isLetter).plain
      . aspire(_ == t"helloworld")

      test(m"skip predicate"):
        e"hello world".skip(_.isLetter).plain
      . aspire(_ == t" ")

      test(m"erase removes characters"):
        e"hello".erase('l').plain
      . assert(_ == t"heo")

      test(m"translate"):
        e"hello".translate(c => if c == 'l' then 'L' else c).plain
      . assert(_ == t"heLLo")

      test(m"tr"):
        e"hello".tr('l', 'L').plain
      . assert(_ == t"heLLo")

    suite(m"Textual: transforms"):
      test(m"subscripts"):
        e"123".subscripts.plain
      . assert(_ == t"₁₂₃")

      test(m"superscripts"):
        e"123".superscripts.plain
      . assert(_ == t"¹²³")

    // ─── Cuttable ─────────────────────────────────────────────────────────

    suite(m"Cuttable"):
      test(m"cut on comma"):
        e"a,b,c".cut(t",").map(_.plain)
      . assert(_ == List(t"a", t"b", t"c"))

      test(m"cut on multi-char"):
        e"hello-world".cut(t"-").map(_.plain)
      . assert(_ == List(t"hello", t"world"))

      test(m"cut empty input"):
        e"".cut(t",").map(_.plain)
      . assert(_ == List(t""))

      test(m"cut preserves bold styling on parts"):
        emulate(e"$Bold(a,b,c)").buffer.find(t"a,b,c").vouch.styles.all(_.bold)
      . assert(_ == true)

    // ─── Joinable & Concatenable ──────────────────────────────────────────

    suite(m"Joinable"):
      test(m"join with separator"):
        List(e"a", e"b", e"c").join(e",").plain
      . assert(_ == t"a,b,c")

      test(m"join with bookends"):
        List(e"a", e"b", e"c").join(e"[", e",", e"]").plain
      . assert(_ == t"[a,b,c]")

      test(m"join preserves nested styling"):
        val parts = List(e"$Bold(a)", e"plain", e"$Italic(c)")
        emulate(parts.join(e",")).buffer.find(t"a").vouch.styles.all(_.bold)
      . assert(_ == true)

      test(m"empty list joins to empty"):
        List[Teletype]().join(e",").plain
      . assert(_ == t"")

      test(m"concat via concat instance"):
        Teletype.concatenable.concat(e"hello", e" world").plain
      . assert(_ == t"hello world")

    // ─── Teletypeable instances ───────────────────────────────────────────

    suite(m"Teletypeable instances"):
      test(m"text is teletypeable"):
        emit(t"hello".teletype)
      . assert(_ == t"hello")

      test(m"showable Int is teletypeable"):
        emit(42.teletype)
      . assert(_ == t"42")

      test(m"some option is teletypeable"):
        emit((Some(t"hello"): Option[Text]).teletype).contains(t"hello")
      . assert(_ == true)

      test(m"none option is teletypeable"):
        emit((None: Option[Text]).teletype).length
      . assert(_ > 0)

      test(m"message renders the text"):
        emit(m"hello world".teletype).contains(t"hello world")
      . assert(_ == true)

    // ─── Ribbon ───────────────────────────────────────────────────────────

    suite(m"Ribbon"):
      test(m"single-segment ribbon contains the text"):
        emulate(Ribbon(Bg(red)).fill(e"one"), width = 80).buffer.find(t"one").or(Unset)
      . assert(_ != Unset)

      test(m"three-segment ribbon contains all parts"):
        val pty = emulate(Ribbon(Bg(red), Bg(yellow), Bg(green)).fill(e"one", e"two", e"three"), width = 80)
        ( pty.buffer.find(t"one").or(Unset),
          pty.buffer.find(t"two").or(Unset),
          pty.buffer.find(t"three").or(Unset) )
      . assert { case (a, b, c) => a != Unset && b != Unset && c != Unset }

      test(m"first ribbon segment has correct background"):
        val pty = emulate(Ribbon(Bg(red), Bg(yellow)).fill(e"one", e"two"), width = 80)
        pty.buffer.find(t"one").vouch.styles.all(_.background == red)
      . assert(_ == true)

      test(m"second ribbon segment has correct background"):
        val pty = emulate(Ribbon(Bg(red), Bg(yellow)).fill(e"one", e"two"), width = 80)
        pty.buffer.find(t"two").vouch.styles.all(_.background == yellow)
      . assert(_ == true)

      test(m"zero-length ribbon with no parts is empty"):
        Ribbon().fill().plain
      . assert(_ == t"")

      test(m"zero-color ribbon with parts still preserves part text"):
        // A Ribbon with no colours but some parts silently drops the parts.
        // Aspirationally, the part text should be preserved (unstyled), not lost.
        Ribbon().fill(e"hello", e"world").plain
      . aspire(_ == t"hello world")

    // ─── csi helpers ──────────────────────────────────────────────────────

    suite(m"csi helpers"):
      test(m"cuu with no parameter"):
        csi.cuu()
      . assert(_ == t"\u001b[A")

      test(m"cuu with parameter"):
        csi.cuu(3)
      . assert(_ == t"\u001b[3A")

      test(m"cup with both parameters"):
        csi.cup(2, 5)
      . assert(_ == t"\u001b[2;5H")

      test(m"sgr with single parameter"):
        csi.sgr(1)
      . assert(_ == t"\u001b[1m")

      test(m"sgr with multiple parameters"):
        csi.sgr(1, 31)
      . assert(_ == t"\u001b[1;31m")

      test(m"dectcem on"):
        csi.dectcem(true)
      . assert(_ == t"\u001b[?25h")

      test(m"dectcem off"):
        csi.dectcem(false)
      . assert(_ == t"\u001b[?25l")

      test(m"hidden cursor reflected in pty"):
        Pty(80, 4).consume(csi.dectcem(false)).cursorVisible
      . assert(_ == false)

    // ─── Escape object ────────────────────────────────────────────────────

    suite(m"Escape"):
      test(m"escape with on/off pair toggles bold"):
        val esc = Escape(t"[1m", t"[22m")
        emulate(e"a${esc}(b)c").buffer.style(1.z, 0.z).bold
      . assert(_ == true)

      test(m"escape pair restores after span"):
        val esc = Escape(t"[1m", t"[22m")
        emulate(e"a${esc}(b)c").buffer.style(2.z, 0.z).bold
      . aspire(_ == false)

    // ─── Round-trip via Pty: comprehensive ────────────────────────────────

    suite(m"Round-trip"):
      test(m"plain.length matches pty char count"):
        val tt = e"hello $Bold(world)"
        val pty = emulate(tt)
        val visible = pty.buffer.find(t"hello world").or(Unset)
        visible != Unset
      . assert(_ == true)

      test(m"styled text is at the expected screen position"):
        emulate(e"abc$Bold(def)ghi").buffer.char(3.z, 0.z)
      . assert(_ == 'd')

    // ─── hyperlinks (OSC 8) ────────────────────────────────────────────────

    suite(m"Hyperlinks"):
      test(m"open emits OSC 8 with url"):
        emit(e"${Hyperlink(t"https://example.com")}[here]").contains(t"\e]8;;https://example.com\e\\")
      . assert(_ == true)

      test(m"close emits empty OSC 8 after the link"):
        emit(e"${Hyperlink(t"https://example.com")}[here] more").contains(t"\e]8;;\e\\")
      . assert(_ == true)

      test(m"link contents are the visible text"):
        e"a${Hyperlink(t"https://x.test")}[middle]b".plain
      . assert(_ == t"amiddleb")

      test(m"link at end of text emits trailing close"):
        emit(e"${Hyperlink(t"https://x.test")}[link]").contains(t"]8;;")
      . assert(_ == true)

      test(m"basic termcap drops hyperlink escapes"):
        plainRender(e"${Hyperlink(t"https://x.test")}[link]")
      . assert(_ == t"link")

      test(m"styled text inside a hyperlink keeps both"):
        val rendered = emit(e"${Hyperlink(t"https://x.test")}[$Bold(bold link)]")
        (rendered.contains(t"\e[1m"), rendered.contains(t"\e]8;;https://x.test"))
      . assert(_ == ((true, true)))

      test(m"hyperlink stored at correct position"):
        e"hi ${Hyperlink(t"https://x.test")}[click]".hyperlinks(3)
      . assert(_ == t"https://x.test")

    // ─── new style attributes ─────────────────────────────────────────────

    suite(m"New style attributes"):
      test(m"faint emits SGR 2"):
        emit(e"$Faint(x)").contains(t"\e[2m")
      . assert(_ == true)

      test(m"double-underline emits SGR 21"):
        emit(e"$DoubleUnderline(x)").contains(t"\e[21m")
      . assert(_ == true)

      test(m"blink-slow emits SGR 5"):
        emit(e"$BlinkSlow(x)").contains(t"\e[5m")
      . assert(_ == true)

      test(m"blink-fast emits SGR 6"):
        emit(e"$BlinkFast(x)").contains(t"\e[6m")
      . assert(_ == true)

      test(m"overline emits SGR 53"):
        emit(e"$Overline(x)").contains(t"\e[53m")
      . assert(_ == true)

      test(m"overline closes with SGR 55"):
        emit(e"a$Overline(b)c").contains(t"\e[55m")
      . assert(_ == true)

      test(m"plain renderer drops new attributes"):
        plainRender(e"a$Faint($Overline(b))c")
      . assert(_ == t"abc")

    // ─── concat invariant ──────────────────────────────────────────────────

    suite(m"Concat invariant"):
      test(m"plain text of append matches concatenation of plains"):
        val a = e"$Bold(hello) "
        val b = e"$Italic(world)"
        a.append(b).plain
      . assert(_ == t"hello world")

      test(m"appended teletype carries both left and right SGR codes"):
        val rendered = emit(e"$Bold(hello) ".append(e"$Italic(world)"))
        (rendered.contains(t"[1m"), rendered.contains(t"[3m"))
      . assert(_ == ((true, true)))

      test(m"styleAt returns the correct style for each position"):
        val tt = e"$Bold(abc)def"
        val boldBit = StyleWord.Bold
        ((tt.styleAt(0) & boldBit) != 0, (tt.styleAt(3) & boldBit) != 0)
      . assert(_ == ((true, false)))

      test(m"trailing style is reset after styled prefix"):
        e"$Bold(abc)def".trailingStyle
      . assert(_ == 0L)
