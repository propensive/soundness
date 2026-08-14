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

object Tests extends Suite(m"Gnossienne Tests"):
  def run(): Unit =
    val jack = Person(t"jack@example.com", t"Jack")
    val jill = Person(t"jill@example.com", t"Jill")
    val people = Set(jack, jill)

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
        unsafely(resolvable.resolve(t"jill@example.com"))
      . assert(_ == jill)

      test(m"Resolving an unknown key raises an error"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))

        unsafely:
          capture[Reference.Error](resolvable.resolve(t"nobody@example.com")).reference
      . assert(_ == t"nobody@example.com")

      test(m"An unresolvable reference reports why it failed"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))

        unsafely:
          capture[Reference.Error](resolvable.resolve(t"nobody@example.com")).reason
      . assert(_ == Reference.Error.Reason.NotFound)

    suite(m"Reference tests"):
      test(m"A reference to an entity holds its key"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        jack.ref.key
      . assert(_ == t"jack@example.com")

      test(m"A reference resolves back to its entity"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        unsafely(jack.ref())
      . assert(_ == jack)

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
        unsafely(Reference[Person](t"jill@example.com")())
      . assert(_ == jill)

      test(m"A reference to a missing entity fails to resolve"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))

        unsafely:
          capture[Reference.Error](Reference[Person](t"nobody@example.com")()).reference
      . assert(_ == t"nobody@example.com")

    suite(m"Reference codec tests"):
      test(m"A reference encodes as its key"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        jack.ref.encode
      . assert(_ == t"jack@example.com")

      test(m"A reference decodes from its key"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        t"jill@example.com".as[Reference to Person].key
      . assert(_ == t"jill@example.com")

      test(m"A decoded reference resolves to its entity"):
        given resolvable: Person is Resolvable by Text = unsafely(Resolvable(people))
        unsafely(t"jill@example.com".as[Reference to Person]())
      . assert(_ == jill)

    suite(m"Error message tests"):
      test(m"A reference error explains what could not be found"):
        val error = Reference.Error(t"nobody@example.com", Reference.Error.Reason.NotFound)
        error.message.text
      . assert(_ == t"the reference nobody@example.com could not be resolved because no target "+
          t"with that reference was found in the store")
