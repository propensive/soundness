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
package xenophile

// PHASE-0 SPIKE (issue #1644): throwaway probes for the two risky assumptions behind the
// Kotlin ecosystem — (A) that a macro can read Kotlin `@Metadata` from classfiles on the
// compile classpath at expansion time, under mill/zinc; and (B) that quotes.reflect can emit
// a direct call to a Java static method (a Kotlin file-facade function). To be deleted and
// replaced by `KotlinDialect`/`KotlinInvoke` once proven.

import scala.jdk.CollectionConverters.*
import scala.quoted.*

import kotlin.metadata.jvm as kmj

object KotlinSpike:
  // Probe A: returns, as literal data, the classloader that could see `className`, and the
  // function names+JVM signatures discovered in its Kotlin metadata (following multi-file
  // facades into their parts).
  transparent inline def functions(inline className: String): List[String] =
    ${KotlinSpike.functionsImpl('className)}

  // Probe B: materializes `kotlin.TuplesKt.to(first, second)` as a direct JVM call. (The
  // obvious first target, `StringsKt.repeat`, is unreachable: kotlin-stdlib compiles its
  // multi-file facades with parts-INHERITANCE, so members are statics of package-private part
  // classes, resolved through the public facade by the JVM — a dispatch dotty's model cannot
  // express. Ordinary Kotlin libraries declare facade members directly, as `TuplesKt` does.)
  inline def pair(inline first: String, inline second: String): AnyRef =
    ${KotlinSpike.pairImpl('first, 'second)}

  private def loadClass(className: String)(using Quotes): (String, Class[?]) =
    import quotes.reflect.*

    def attempt(way: String)(load: => Class[?] | Null): Option[(String, Class[?])] =
      try Option(load).map((way, _)) catch case _: Throwable => None

    attempt("own-classloader"):
      val loader = KotlinSpike.getClass.getClassLoader
      if loader == null then null else Class.forName(className, false, loader)

    . orElse:
        attempt("context-classloader"):
          val loader = Thread.currentThread.nn.getContextClassLoader
          if loader == null then null else Class.forName(className, false, loader)

    . orElse(attempt("single-arg-forName")(Class.forName(className)))
    . getOrElse:
        report.errorAndAbort(s"spike: no classloader could load $className")

  private def kmPackages(cls: Class[?], loadPart: String => Class[?])(using Quotes)
  :   List[kotlin.metadata.KmPackage] =

    import quotes.reflect.*

    val annotation = cls.getAnnotation(classOf[kotlin.Metadata])

    if annotation == null
    then report.errorAndAbort(s"spike: no @Metadata on ${cls.getName}")

    kmj.KotlinClassMetadata.readStrict(annotation) match
      case facade: kmj.KotlinClassMetadata.FileFacade => List(facade.getKmPackage.nn)

      case facade: kmj.KotlinClassMetadata.MultiFileClassFacade =>
        facade.getPartClassNames.nn.asScala.to(List).flatMap: part =>
          kmj.KotlinClassMetadata.readStrict(loadPart(part.nn.replace("/", ".").nn).getAnnotation(classOf[kotlin.Metadata]).nn)
          match
            case part: kmj.KotlinClassMetadata.MultiFileClassPart => List(part.getKmPackage.nn)
            case other                                            => Nil

      case other =>
        report.errorAndAbort(s"spike: unexpected metadata kind $other for ${cls.getName}")

  private def functionsImpl(className: Expr[String])(using Quotes): Expr[List[String]] =
    import quotes.reflect.*

    val name = className.valueOrAbort
    val (way, cls) = loadClass(name)
    val packages = kmPackages(cls, part => loadClass(part)(using quotes)(1))

    val functions: List[String] = packages.flatMap: pkg =>
      pkg.getFunctions.nn.asScala.to(List).map: function =>
        val signature = kmj.JvmExtensionsKt.getSignature(function)
        s"${function.getName}:${if signature == null then "?" else signature.toString}"

    Expr(way :: functions.sorted)

  private def pairImpl(first: Expr[String], second: Expr[String])(using Quotes): Expr[AnyRef] =
    import quotes.reflect.*

    val cls = Symbol.requiredClass("kotlin.TuplesKt")
    val module = cls.companionModule

    module.methodMember("to") match
      case Nil =>
        report.errorAndAbort:
          s"spike: no `to` among: ${module.methodMembers.map(_.name).distinct.sorted.mkString(", ")}"

      case method :: _ =>
        Apply
          ( TypeApply
              ( Select(Ref(module), method),
                List(TypeTree.of[String], TypeTree.of[String]) ),
            List(first.asTerm, second.asTerm) )
        . asExprOf[AnyRef]
