/*
    Hellenism, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.quoted.*

import rudiments.*
import vacuous.*

object Hellenism extends Hellenism2:
  opaque type ClassRef = Class[?]

  object ClassRef:
    def apply(javaClass: Class[?]): ClassRef = javaClass
    inline def apply[ClassType <: AnyKind]: ClassRef = ${Hellenism.makeClass[ClassType]}

  extension (classRef: ClassRef)
    def classloader: Classloader = new Classloader(classRef.getClassLoader().nn)

    def classpathEntry: Optional[ClasspathEntry] =
      ClasspathEntry(classRef.getProtectionDomain.nn.getCodeSource.nn.getLocation.nn)

export Hellenism.ClassRef

trait Hellenism2:
  def makeClass[ClassType <: AnyKind: Type](using Quotes): Expr[ClassRef] =
    import quotes.reflect.*

    '{ClassRef(Class.forName(${Expr(TypeRepr.of[ClassType].classSymbol.get.fullName)}).nn)}
