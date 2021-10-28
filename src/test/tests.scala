/*
    Gesticulate, version 0.1.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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
import eucalyptus.*
import rudiments.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

object Tests extends Suite(str"Gesticulate tests"):
  def run(using Runner): Unit =
    test(str"parse media type's type") {
      Media.parse(str"application/json").group
    }.assert(_ == Media.Group.Application)
    
    test(str"parse media type's subtype") {
      Media.parse(str"application/json").subtype
    }.assert(_ == Media.Subtype.Standard(str"json"))

    test(str"parse media type suffix") {
      Media.parse(str"application/epub+zip").suffixes
    }.assert(_ == List(Media.Suffix.Zip))

    test(str"parse full media type") {
      Media.parse(str"application/json")
    }.assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard(str"json")))

    test(str"parse full media type with parameter") {
      Media.parse(str"application/json; charset=UTF-8")
    }.assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard(str"json"),
        parameters = List((str"charset", str"UTF-8"))))
    
    test(str"invalid media type") {
      capture(Media.parse(str"applicationjson"))
    }.assert(_ == InvalidMediaTypeError(str"applicationjson",
        InvalidMediaTypeError.Nature.NotOneSlash))
