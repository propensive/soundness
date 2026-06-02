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
package apoplexy

import soundness.*

case class Credentials(username: Text, password: Text)
case class NewPet(name: Text, tag: Optional[Text] = Unset)
case class Photo(url: Text, width: Optional[Int] = Unset, height: Optional[Int] = Unset)

object ApiTests extends Suite(m"Api client tests"):
  def run(): Unit =
    val api = Api(cp"/apoplexy/petstore.json")

    suite(m"navigation refines the path type"):
      test(m"a literal segment refines Locus"):
        val pets: Api at "/pets" = api.pets
        pets.request.path
      . assert(_ == t"/pets")

      test(m"a positional arg fills the following path template"):
        val one: Api at "/pets/{petId}" = api.pets(42)
        one.request.substitutions
      . assert(_ == Map(t"petId" -> t"42"))

      test(m"nested templated navigation"):
        val photos: Api at "/pets/{petId}/photos" = api.pets(42).photos
        photos.request.path
      . assert(_ == t"/pets/{petId}/photos")

    suite(m"the apply shortcut invokes the sole non-DELETE method"):
      test(m"GET sole method with query params (the user's example shape)"):
        api.pets(42).photos(width = 10, height = 20).request
      . assert: request =>
          request.method == Http.Get && request.path == t"/pets/{petId}/photos"
          && request.substitutions == Map(t"petId" -> t"42")
          && request.query == List(t"width" -> t"10", t"height" -> t"20")

      test(m"POST sole method with a positional body"):
        api.login(Credentials(t"jon", t"pw")).request
      . assert(request => request.method == Http.Post && request.path == t"/login" && request.body.present)

      test(m"an optional query parameter may be omitted"):
        api.pets(42).photos(width = 10).request.query
      . assert(_ == List(t"width" -> t"10"))

    suite(m"explicit terminals for multi-method endpoints"):
      test(m"GET /pets via explicit .get with a query parameter"):
        api.pets.get(limit = 10).request
      . assert(request => request.method == Http.Get && request.query == List(t"limit" -> t"10"))

      test(m"POST /pets via explicit .post with a body"):
        api.pets.post(NewPet(t"Milo", tag = t"cat")).request
      . assert(request => request.method == Http.Post && request.body.present)

      test(m"GET /pets/{petId} via explicit .get with no arguments"):
        api.pets(42).get.request
      . assert(request => request.method == Http.Get && request.path == t"/pets/{petId}")

      test(m"PUT /pets/{petId} via explicit .put with a body"):
        api.pets(42).put(NewPet(t"Rex")).request.method
      . assert(_ == Http.Put)

    suite(m"delete is always explicit"):
      test(m"DELETE /pets/{petId}"):
        api.pets(42).delete().request.method
      . assert(_ == Http.Delete)

      test(m"a DELETE-only endpoint with a path parameter"):
        api.sessions(t"abc").delete().request
      . assert(request => request.method == Http.Delete && request.substitutions == Map(t"token" -> t"abc"))

      test(m"a DELETE-only endpoint reached by a bare segment"):
        api.logout.delete().request.method
      . assert(_ == Http.Delete)

    suite(m"compile-time safety"):
      test(m"a nonexistent first segment is rejected"):
        demilitarize(api.unicorns).length
      . assert(_ > 0)

      test(m"an undeclared path is rejected"):
        demilitarize(api.pets(42).toys).length
      . assert(_ > 0)

      test(m"a path parameter of the wrong type is rejected"):
        demilitarize(api.pets(t"notAnInt")).length
      . assert(_ > 0)

      test(m"the apply shortcut is rejected on a multi-method endpoint"):
        demilitarize(api.pets(limit = 10)).length
      . assert(_ > 0)

      test(m"the apply shortcut cannot invoke a DELETE-only endpoint"):
        demilitarize(api.logout()).length
      . assert(_ > 0)

      test(m"a query parameter of the wrong type is rejected"):
        demilitarize(api.pets(42).photos(width = t"big")).length
      . assert(_ > 0)

      test(m"omitting a required query parameter is rejected"):
        demilitarize(api.pets(42).photos(height = 20)).length
      . assert(_ > 0)
