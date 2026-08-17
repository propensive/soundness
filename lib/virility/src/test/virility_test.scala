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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package virility

import soundness.*

object Tests extends Suite(m"Virility Tests"):
  def run(): Unit =
    suite(m"Escaping tests"):
      test(m"Hyphens become hyphen-minus escapes"):
        Roff.escape(t"list-files")
      . assert(_ == t"list\\-files")

      test(m"Backslashes become the rs escape"):
        Roff.escape(t"C:\\Users")
      . assert(_ == t"C:\\[rs]Users")

      test(m"A backslash before a hyphen escapes both independently"):
        Roff.escape(t"a\\-b")
      . assert(_ == t"a\\[rs]\\-b")

      test(m"Newlines become spaces"):
        Roff.escape(t"one\ntwo")
      . assert(_ == t"one two")

      test(m"Quoted arguments escape embedded double quotes"):
        Roff.quote(t"a\"b")
      . assert(_ == t"\"a\\[dq]b\"")

      test(m"Quoted arguments keep hyphens verbatim"):
        Roff.quote(t"2026-08-17")
      . assert(_ == t"\"2026-08-17\"")

    suite(m"Serialization tests"):
      test(m"A minimal document is a TH line with trailing arguments dropped"):
        Roff(t"grep", 1).serialize
      . assert(_ == t".TH \"GREP\" \"1\"\n")

      test(m"Unset middle arguments are kept when later ones are set"):
        Roff(t"grep", 1, Unset, Unset, t"User Commands").serialize
      . assert(_ == t".TH \"GREP\" \"1\" \"\" \"\" \"User Commands\"\n")

      test(m"A paragraph starting with a dot is protected"):
        Roff.Block.Paragraph(Roff.Inline.plain(t".profile is read at startup")).serialize
      . assert(_ == List(t".P", t"\\&.profile is read at startup"))

      test(m"A paragraph starting with a quote is protected"):
        Roff.Block.Paragraph(Roff.Inline.plain(t"'quoted' words")).serialize
      . assert(_ == List(t".P", t"\\&'quoted' words"))

      test(m"Bold and italic serialize as font alternations"):
        Roff.Block.Paragraph
         (List(Roff.Inline.bold(t"ls"), Roff.Inline.Plain(t" "), Roff.Inline.italic(t"file")))
        . serialize
      . assert(_ == List(t".P", t"\\fBls\\fP \\fIfile\\fP"))

      test(m"Examples pass through EX/EE with escaping"):
        Roff.Block.Example(List(t"grep -r pattern .", t".hidden")).serialize
      . assert(_ == List(t".EX", t"grep \\-r pattern .", t"\\&.hidden", t".EE"))

      test(m"A tagged paragraph emits TP, tag line and body line"):
        Roff.Block.Tagged
         (List(Roff.Inline.bold(t"--verbose")), Roff.Inline.plain(t"Print more detail."))
        . serialize
      . assert(_ == List(t".TP", t"\\fB\\-\\-verbose\\fP", t"Print more detail."))

      test(m"A tagged paragraph with no body emits only its tag"):
        Roff.Block.Tagged(List(Roff.Inline.bold(t"HOME")), Nil).serialize
      . assert(_ == List(t".TP", t"\\fBHOME\\fP"))

      test(m"Sections nest their blocks and indentation closes with RE"):
        Roff
         (t"demo", 1, t"2026-08-17", t"demo 1.0", t"User Commands",
          List
           (Roff.Block.Section
             (t"Name",
              List
               (Roff.Block.Paragraph(Roff.Inline.plain(t"demo - a demonstration")),
                Roff.Block.Indented
                 (List(Roff.Block.Paragraph(Roff.Inline.plain(t"indented"))))))))
        . serialize
      . assert(_ == t".TH \"DEMO\" \"1\" \"2026-08-17\" \"demo 1.0\" \"User Commands\"\n"
                    + t".SH \"Name\"\ndemo \\- a demonstration\n.RS\n.P\nindented\n.RE\n")
