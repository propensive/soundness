/*
    Jovian, version 0.1.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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

import probably.*

object Tests extends Suite("Gesticulate tests"):
  def run(using Runner): Unit =
    test("parse media type's type") {
      Media.parse("application/json").group
    }.assert(_ == Media.Group.Application)
    
    test("parse media type's subtype") {
      Media.parse("application/json").subtype
    }.assert(_ == Media.Subtype.Standard("json"))
    
    test("parse media type suffix") {
      Media.parse("application/epub+zip").suffixes
    }.assert(_ == List(Media.Suffix.Zip))

