/*
    Hellenism, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import fulminate.*
import rudiments.*
import serpentine.*
import vacuous.*

object Classpath:
  @targetName("child")
  infix def / (child: Name[ClasspathRef.Forbidden]): ClasspathRef = ClasspathRef(List(child))

  def apply(classloader: jn.URLClassLoader): Classpath =
    val entries = classloader.let(_.getURLs.nn.to(List)).or(Nil).map(_.nn).flatMap(ClasspathEntry(_).option)

    if entries.exists:
      case _: ClasspathEntry.Url => true
      case _                     => false
    then OnlineClasspath(entries)
    else LocalClasspath:
      entries.collect:
        case directory: ClasspathEntry.Directory      => directory
        case jar: ClasspathEntry.Jarfile              => jar
        case runtime: ClasspathEntry.JavaRuntime.type => runtime

trait Classpath:
  def entries: List[ClasspathEntry]
