/*
    Serpentine, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import rudiments.*
import gossamer.*

import scala.quoted.*


class PathElement(val value: Text)

object PathElement:

  // FIXME: Do the checks
  def apply(value: Text): PathElement throws InvalidPathError = new PathElement(value)

  private val forbidden = Set('<', '>', ':', '"', '\\', '|', '?', '*')
  
  private val windowsForbidden = Set(t"con", t"prn", t"aux", t"nul", t"com1", t"com2", t"com3",
      t"com4", t"com5", t"com6", t"com7", t"com8", t"com9", t"lpt1", t"lpt2", t"lpt3", t"lpt4",
      t"lpt5", t"lpt6", t"lpt7", t"lpt8", t"lpt9")
  
  def make(sc: Expr[StringContext])(using Quotes): Expr[PathElement] =
    import quotes.reflect.*
    
    val str = sc match
      case '{StringContext($str: String)} =>
        str.value.get
      
      case _ =>
        throw Mistake("A StringContext should contain literals")

    str match
      case ""   => fail("a pathname cannot be empty")
      case "."  => fail("a pathname cannot be the string \".\"")
      case ".." => fail("a pathname cannot be the string \"..\"")

      case str =>
        if str.endsWith(".")
        then fail("the pathname cannot end with a '.' character on Windows")
        else if str.endsWith(" ")
        then fail("the pathname cannot end with a space on Windows")
        else if windowsForbidden.contains(Text(str).cut(t".").head.lower)
        then fail(s"the pathname $str (with or without an extension) is not valid on Windows")
        else if str.contains("/") then fail("a path cannot contain the '/' character")
        else if str.exists(_ < 32) then fail("a path cannot contain control characters")
        else
          val misused = forbidden.intersect(str.to(Set))
          if !misused.isEmpty
          then
            val detail = misused.to(List).map(_.toString).map(Text(_)).join(t"'", t"', '",
                t"' or '", t"'").s
            
            fail(s"a path cannot contain the characters $detail on Windows, and should not more generally")
          else '{PathElement(Text(${Expr(str)}))(using unsafeExceptions.canThrowAny)}

extension (inline sc: StringContext)
  inline def p(): PathElement = ${PathElement.make('sc)}
