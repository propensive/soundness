/*
    Gesticulate, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gesticulate

import gossamer.*
import probably.*
import rudiments.*
import perforate.*

import errorHandlers.throwUnsafely

object Tests extends Suite(t"Gesticulate tests"):
  def run(): Unit =
    test(t"parse media type's type"):
      Media.parse(t"application/json").group
    .assert(_ == Media.Group.Application)
    
    test(t"parse media type's subtype"):
      Media.parse(t"application/json").subtype
    .assert(_ == Media.Subtype.Standard(t"json"))

    test(t"parse media type suffix"):
      Media.parse(t"application/epub+zip").suffixes
    .assert(_ == List(Media.Suffix.Zip))

    test(t"parse full media type"):
      Media.parse(t"application/json")
    .assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard(t"json")))

    test(t"parse full media type with parameter"):
      Media.parse(t"application/json; charset=UTF-8")
    .assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard(t"json"),
        parameters = List((t"charset", t"UTF-8"))))
    
    test(t"invalid media type"):
      capture(Media.parse(t"applicationjson"))
    .assert(_ == MediaTypeError(t"applicationjson",
        MediaTypeError.Nature.NotOneSlash))
