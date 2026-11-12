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
package gnossienne

import soundness.*

import errorDiagnostics.stackTracesDiagnostics

case class Person(@primary email: Text, name: Text)

case class Product(@primary code: Int, description: Text)

object Tests extends Suite("references", m"Gnossienne Tests"):
  def run(): Unit =
    val romeo = Person(t"romeo@example.com", t"Romeo")
    val juliet = Person(t"juliet@example.com", t"Juliet")
    val people = Set(romeo, juliet)

    val widget = Product(1001, t"Widget")
    val gadget = Product(1002, t"Gadget")
    val products = Set(widget, gadget)

    suite(m"Resolvable tests"):
      test(m"The indexed field is identified by name"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        resolvable.field
      . assert(_ == t"email")

      test(m"An indexed Int field is identified by name"):
        given resolvable: Product is Resolvable by Int = unsafely(Resolvable(products))
        resolvable.field
      . assert(_ == t"code")

      test(m"Resolve an entity from its key"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        unsafely(resolvable.resolve(t"juliet@example.com"))
      . assert(_ == juliet)

      test(m"Resolving an unknown key raises an error"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))

        unsafely:
          capture[Reference.Error](resolvable.resolve(t"tybalt@example.com")).reference
      . assert(_ == t"tybalt@example.com")

      test(m"An unresolvable reference reports why it failed"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))

        unsafely:
          capture[Reference.Error](resolvable.resolve(t"tybalt@example.com")).reason
      . assert(_ == Reference.Error.Reason.NotFound)

    suite(m"Reference tests"):
      test(m"A reference to an entity holds its key"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        romeo.ref.key
      . assert(_ == t"romeo@example.com")

      test(m"A reference resolves back to its entity"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        unsafely(romeo.ref())
      . assert(_ == romeo)

      test(m"A reference to an Int-keyed entity holds its key"):
        given resolvable: Product is Resolvable by Int = unsafely(Resolvable(products))
        widget.ref.key
      . assert(_ == 1001)

      test(m"An Int-keyed reference resolves back to its entity"):
        given resolvable: Product is Resolvable by Int = unsafely(Resolvable(products))
        unsafely(gadget.ref())
      . assert(_ == gadget)

      test(m"A reference constructed from a key resolves"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        unsafely(Reference[Person](t"juliet@example.com")())
      . assert(_ == juliet)

      test(m"A reference to a missing entity fails to resolve"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))

        unsafely:
          capture[Reference.Error](Reference[Person](t"tybalt@example.com")()).reference
      . assert(_ == t"tybalt@example.com")

    suite(m"Reference codec tests"):
      test(m"A reference encodes as its key"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        romeo.ref.encode
      . assert(_ == t"romeo@example.com")

      test(m"A reference decodes from its key"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        t"juliet@example.com".as[Reference to Person].key
      . assert(_ == t"juliet@example.com")

      test(m"A decoded reference resolves to its entity"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        unsafely(t"juliet@example.com".as[Reference to Person]())
      . assert(_ == juliet)

    suite(m"Error message tests"):
      test(m"A reference error explains what could not be found"):
        val error = Reference.Error(t"tybalt@example.com", Reference.Error.Reason.NotFound)
        error.message.text
      . assert(_ == t"the reference tybalt@example.com could not be resolved because no target "+
          t"with that reference was found in the store")
