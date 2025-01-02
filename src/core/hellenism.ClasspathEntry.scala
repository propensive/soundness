/*
    Hellenism, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hellenism

import java.net as jn

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

sealed trait ClasspathEntry

object ClasspathEntry:
  case class Directory(path: Text) extends ClasspathEntry:
    def apply[DirectoryType: SpecificDirectory](): DirectoryType = SpecificDirectory(path)

  case class Jar(path: Text) extends ClasspathEntry:
    def apply[FileType: SpecificFile](): FileType = SpecificFile(path)

  case class Url(url: Text) extends ClasspathEntry:
    def apply[UrlType: SpecificUrl](): UrlType = SpecificUrl(url)

  case object JavaRuntime extends ClasspathEntry

  def apply(url: jn.URL): Optional[ClasspathEntry] = url.getProtocol.nn.tt match
    case t"jrt" =>
      ClasspathEntry.JavaRuntime

    case t"file" =>
      val path: Text = url.nn.getPath.nn.tt
      if path.ends(t"/") then ClasspathEntry.Directory(path) else ClasspathEntry.Jar(path)

    case t"http" | t"https" =>
      ClasspathEntry.Url(url.toString.tt)

    case _ =>
      Unset
