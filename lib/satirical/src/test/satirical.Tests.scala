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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package satirical

import soundness.*
import chiaroscuro.Decomposable

import charEncoders.utf8


object Tests extends Suite(t"Satirical tests"):
  def run(): Unit =

    test(t"A simple package declaration"):
      t"package wasi:clocks;".read[Wit]
    . assert(_ == Wit(List(Package(w"wasi", w"clocks"))))


    println(Wit(List(Package(w"wasi", w"clocks"))).decompose)





    test(t"A versioned package declaration"):
      t"package wasi:clocks@1.2.0;".read[Wit]
    . assert(_ == Wit(List(Package(w"wasi", w"clocks", t"1.2.0"))))

    test(t"Two package declarations"):
      t"""
      package local:a {
          interface foo {}
      }

      package local:b {
          interface bar {}
      }
      """.read[Wit]
    . assert(_ == Wit(List
                   (Package(w"local", w"a", Unset, List(Interface(w"foo"))),
                    Package(w"local", w"b", Unset, List(Interface(w"bar"))))))


    test(t"Parse empty world"):
      t"world example-world {   }".read[Wit]
    . assert(_ == Wit(List(World(w"example-world", Nil))))

    test(t"Parse empty interface"):
      t"interface example-interface {   }".read[Wit]
    . assert(_ == Wit(List(Interface(w"example-interface", Nil))))

    test(t"Parse empty interface ignoring comment"):
      t"""// a comment
          // another
          interface example-interface {   }
      """.read[Wit].tap(println)
    . assert(_ == Wit(List(Interface(w"example-interface", Nil))))

    test(t"Parse empty world and interface"):
      t"""interface eg-int {   }
          world eg-world {}
      """.read[Wit]

    . assert(_ == Wit(List(Interface(w"eg-int", Nil), World(w"eg-world", Nil))))

    test(t"Parse package"):
      t"""package namespace:name;
          interface eg-int {   }
          world eg-world {}
      """.read[Wit]

    . assert(_ == Wit(List(Interface(w"eg-int", Nil), World(w"eg-world", Nil))))
