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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package hellenism

import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import prepositional.*
import serpentine.*
import vacuous.*

object internal extends Hellenism2:
  opaque type ClassRef <: Class[?] = Class[?]

  object ClassRef:
    def apply(javaClass: Class[?]): ClassRef = javaClass
    inline def apply[template <: AnyKind]: ClassRef = ${hellenism.internal.makeClass[template]}


  extension (classRef: ClassRef)
    def classloader: Classloader = new Classloader(classRef.getClassLoader().nn)

    def classpathEntry: Optional[ClasspathEntry] =
      ClasspathEntry(classRef.getProtectionDomain.nn.getCodeSource.nn.getLocation.nn)


  def classpath(context: Expr[StringContext]): Macro[Resource] =
    import quotes.reflect.*

    val name: String = context.valueOrAbort.parts.head

    val path = safely(name.tt.as[Path on Classpath]).or:
      halt(m"hellenism: the path $name is not a valid classpath path")

    val relativeName = if name.startsWith("/") then name.substring(1) else name
    val contextLoader = Thread.currentThread.nn.getContextClassLoader()

    val stream =
      if contextLoader != null then contextLoader.getResourceAsStream(relativeName)
      else classOf[hellenism.internal.type].getResourceAsStream(name)

    Optional(stream).or:
      halt(m"hellenism: the path $name is not on the classpath")

    // Lift the path components as `String`s and convert to `Text` at runtime (`.tt`), rather than
    // splicing lifted opaque-`Text` trees into the `Path` descent. Under capture checking a spliced
    // `into opaque type Text` literal is rechecked into a spurious `Text^…` (which fails); building
    // the `Text`s at runtime from lifted `String`s keeps the opaque out of the rechecked tree.
    val rootString: String = path.root.s
    val descentStrings: List[String] = path.descent.map(_.s).to(List)

    val resource =
      ' {
          Resource:
            Path[Classpath, Classpath.type, Tuple]
              ( ${Expr(rootString)}.tt, ${Expr(descentStrings)}.map(_.tt) )
        }

    val locus = ConstantType(StringConstant(name))

    Refinement(TypeRepr.of[Resource], "Locus", TypeBounds(locus, locus)).asType.absolve match
      case '[type result <: Resource; result] => '{$resource.asInstanceOf[result]}


trait Hellenism2:
  def makeClass[template <: AnyKind: Type]: Macro[ClassRef] =
    import quotes.reflect.*

    '{ClassRef(Class.forName(${Expr(TypeRepr.of[template].classSymbol.get.fullName)}).nn)}
