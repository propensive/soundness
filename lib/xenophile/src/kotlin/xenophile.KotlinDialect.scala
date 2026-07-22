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

import scala.collection.mutable as scm
import scala.jdk.CollectionConverters.*

import anticipation.*
import gossamer.*
import kotlin.metadata.*
import kotlin.metadata.jvm.*
import rudiments.*
import vacuous.*

// The Kotlin grammar: self-resolving, per foreign type name, from the `@Metadata` annotation of
// the identically-named class on the compile classpath (which the macro classloader sees),
// parsed with kotlin-metadata-jvm. Alongside the ecosystem-neutral `Prototype`s it retains a
// side-table of each member's JVM-level identity — the declaring class (which, for a multi-file
// facade, is the *part* class), the method's name and descriptor — for `KotlinInvoke` to emit
// direct JVM calls from.
object KotlinDialect extends Dialect:
  case class JvmMember
    ( owner:      Text,
      jvmName:    Text,
      descriptor: Text,
      static:     Boolean,
      arity:      Int )

  private type Resolution = (Map[Text, Prototype], Map[Text, List[JvmMember]])

  // Never parses a text resource: this dialect exists only through `resolve`.
  def parse(source: Text): Map[Text, Map[Text, Prototype]] = Map()

  override def resolves: Boolean = true

  override def resolve(typeName: Text): Optional[Map[Text, Prototype]] =
    resolved(typeName).let(_(0))

  private[xenophile] def members(typeName: Text, name: Text): List[JvmMember] =
    resolved(typeName).let(_(1).at(name)).or(Nil)

  // Cached per type name for the lifetime of a compilation run, like the core `parsed` cache.
  private val cache: scm.HashMap[Text, Optional[Resolution]] = scm.HashMap()

  private def resolved(typeName: Text): Optional[Resolution] =
    cache.synchronized(cache.getOrElseUpdate(typeName, compute(typeName)))

  // Loads a class through this object's own classloader — the macro classloader, which sees the
  // unified downstream compile classpath — without running its static initializers.
  private def loadable(name: Text): Optional[Class[?]] =
    try
      val loader = KotlinDialect.getClass.getClassLoader
      if loader == null then Unset else Optional(Class.forName(name.s, false, loader))
    catch case _: Throwable => Unset

  private def compute(typeName: Text): Optional[Resolution] =
    loadable(typeName).let: cls =>
      metadataOf(cls).let: metadata =>
        try
          metadata match
            case metadata: KotlinClassMetadata.Class =>
              collate(functionsOf(typeName, metadata.getKmClass.nn, static = false))

            case metadata: KotlinClassMetadata.FileFacade =>
              collate(packageFunctions(typeName, metadata.getKmPackage.nn))

            case metadata: KotlinClassMetadata.MultiFileClassFacade =>
              // Members live in the part classes; each records its *own* class as the JVM owner.
              val parts = metadata.getPartClassNames.nn.asScala.to(List).map(_.nn.tt)

              collate:
                parts.flatMap: part =>
                  val binary = part.s.replace("/", ".").nn.tt

                  loadable(binary).let(metadataOf(_)).let:
                    case metadata: KotlinClassMetadata.MultiFileClassPart =>
                      packageFunctions(binary, metadata.getKmPackage.nn)

                    case _ =>
                      Nil

                  . or(Nil)

            case _ =>
              Unset

        catch case _: Throwable => Unset

  private def metadataOf(cls: Class[?]): Optional[KotlinClassMetadata] =
    // Plain Java classes (android.jar et al.) carry no `@Metadata`; a reflection-based fallback
    // for them is deliberately deferred.
    Optional(cls.getAnnotation(classOf[kotlin.Metadata])).let: annotation =>
      try KotlinClassMetadata.readStrict(annotation).nn
      catch case _: Throwable => Unset

  private def functionsOf(owner: Text, kmClass: KmClass, static: Boolean)
  :   List[(Text, KmFunction, JvmMethodSignature, Boolean)] =

    kmClass.getFunctions.nn.asScala.to(List).flatMap: function =>
      signatureOf(function).let { signature => List((owner, function, signature, static)) }.or(Nil)

  private def packageFunctions(owner: Text, kmPackage: KmPackage)
  :   List[(Text, KmFunction, JvmMethodSignature, Boolean)] =

    kmPackage.getFunctions.nn.asScala.to(List).flatMap: function =>
      signatureOf(function).let { signature => List((owner, function, signature, true)) }.or(Nil)

  // Suspending functions take a hidden continuation parameter, so their JVM shape diverges from
  // their Kotlin declaration; they are deferred, and omitted so the error is `no member`.
  private def signatureOf(function: KmFunction): Optional[JvmMethodSignature] =
    if Attributes.isSuspend(function) then Unset
    else Optional(JvmExtensionsKt.getSignature(function))

  private def collate(functions: List[(Text, KmFunction, JvmMethodSignature, Boolean)])
  :   Resolution =

    val prototypes: Map[Text, Prototype] =
      functions.groupBy(_(1).getName.nn.tt).view.mapValues: overloads =>
        prototype(overloads.head(1))

      . to(Map)

    val jvmMembers: Map[Text, List[JvmMember]] =
      functions.groupBy(_(1).getName.nn.tt).view.mapValues: overloads =>
        overloads.map: (owner, function, signature, static) =>
          JvmMember
            ( owner,
              signature.getName.nn.tt,
              signature.getDescriptor.nn.tt,
              static,
              function.getValueParameters.nn.size )

      . to(Map)

    (prototypes, jvmMembers)

  private def prototype(function: KmFunction): Prototype =
    val parameters = function.getValueParameters.nn.asScala.to(List).map: parameter =>
      foreignType(parameter.getType.nn)

    Prototype(parameters, foreignType(function.getReturnType.nn))

  // Maps a Kotlin metadata type to the ecosystem-neutral foreign form: named classes by their
  // fully-qualified Kotlin name, generic applications through `Applied`, and nullability as a
  // union with `"null"`. A type-parameter reference has no stable name at this level; it maps
  // to `"*"`, which no argument satisfies — generic members are deferred.
  private def foreignType(tpe: KmType): Foreign.Type =
    val base: Foreign.Type = tpe.classifier match
      case classifier: KmClassifier.Class =>
        val name = classifier.getName.nn.replace("/", ".").nn.tt

        val arguments = tpe.getArguments.nn.asScala.to(List).flatMap: projection =>
          Optional(projection.getType).let { tpe => List(foreignType(tpe)) }.or(Nil)

        if arguments.isEmpty then Foreign.Type.Named(name)
        else Foreign.Type.Applied(name, arguments)

      case classifier: KmClassifier.TypeAlias =>
        Foreign.Type.Named(classifier.getName.nn.replace("/", ".").nn.tt)

      case _ =>
        Foreign.Type.Named(t"*")

    if Attributes.isNullable(tpe)
    then Foreign.Type.Union(List(base, Foreign.Type.Named(t"null")))
    else base
