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
// facade, is the *part* class), the member's JVM name and descriptor, whether it is reached as
// a method or (for a `const`/`@JvmField` property with no getter) directly as a field, and
// which parameters declare default values — for the materializers (`KotlinInvoke`,
// `KotlinFacade`) to emit direct JVM calls from. Functions, `val`/`var` properties (getters and
// setters) and constructors (under the name `<init>`) are resolved; suspending functions are
// deferred (their JVM shape takes a hidden continuation parameter), and omitted so the
// diagnostic is `no member`.
object KotlinDialect extends Dialect:
  case class JvmMember
    ( owner:      Text,
      jvmName:    Text,
      descriptor: Text,
      static:     Boolean,
      property:   Boolean,
      field:      Boolean,
      arity:      Int,
      defaults:   List[Boolean] )

  private case class Entry(name: Text, member: JvmMember, prototype: Prototype)

  private case class Resolution
    ( prototypes: Map[Text, Prototype],
      members:    Map[Text, List[JvmMember]],
      setters:    Map[Text, JvmMember],
      typeParams: List[Int],
      companion:  Optional[Text] )

  // Never parses a text resource: this dialect exists only through `resolve`.
  def parse(source: Text): Map[Text, Map[Text, Prototype]] = Map()

  override def resolves: Boolean = true

  override def resolve(typeName: Text): Optional[Map[Text, Prototype]] =
    resolved(typeName).let(_.prototypes)

  private[xenophile] def members(typeName: Text, name: Text): List[JvmMember] =
    resolved(typeName).let(_.members.at(name)).or(Nil)

  private[xenophile] def memberPrototype(typeName: Text, name: Text): Optional[Prototype] =
    resolved(typeName).let(_.prototypes.at(name))

  // The setter of a `var` property, when the property is mutable.
  private[xenophile] def setter(typeName: Text, name: Text): Optional[JvmMember] =
    resolved(typeName).let(_.setters.at(name))

  // The declaration-order identifiers of a class's type parameters, for substituting a
  // `Facade`'s Scala type arguments into `#<id>` type-parameter references.
  private[xenophile] def typeParameters(typeName: Text): List[Int] =
    resolved(typeName).let(_.typeParams).or(Nil)

  // The simple name of the class's companion object, when it declares one.
  private[xenophile] def companionName(typeName: Text): Optional[Text] =
    resolved(typeName).let(_.companion)

  // Cached per type name for the lifetime of a compilation run, like the core `parsed` cache.
  private val cache: scm.HashMap[Text, Optional[Resolution]] = scm.HashMap()

  private def resolved(typeName: Text): Optional[Resolution] =
    cache.synchronized(cache.getOrElseUpdate(typeName, compute(typeName)))

  // Loads a class through this object's own classloader — the macro classloader, which sees the
  // unified downstream compile classpath — without running its static initializers. A nested
  // class arrives dot-separated (`Regex.Companion`); its binary name replaces the separators of
  // nesting levels with `$`, tried rightmost-first.
  private def loadable(name: Text): Optional[Class[?]] =
    def attempt(name: Text): Optional[Class[?]] =
      try
        val loader = KotlinDialect.getClass.getClassLoader
        if loader == null then Unset else Optional(Class.forName(name.s, false, loader))
      catch case _: Throwable => Unset

    def nested(name: Text): Optional[Class[?]] =
      attempt(name).or:
        val index = name.s.lastIndexOf('.')

        if index < 0 then Unset else
          nested(t"${name.s.substring(0, index).nn}$$${name.s.substring(index + 1).nn}")

    nested(name)

  private def compute(typeName: Text): Optional[Resolution] =
    loadable(typeName).let: cls =>
      metadataOf(cls).let: metadata =>
        try
          metadata match
            case metadata: KotlinClassMetadata.Class =>
              val kmClass = metadata.getKmClass.nn
              val identifiers = kmClass.getTypeParameters.nn.asScala.to(List).map(_.getId)
              val companion = Optional(kmClass.getCompanionObject).let(_.tt)

              val declared = functions(typeName, kmClass.getFunctions.nn.asScala.to(List), false)
              val fields = properties(typeName, kmClass.getProperties.nn.asScala.to(List))
              val entries = declared ++ fields ++ constructors(typeName, kmClass)

              val setters = propertySetters(typeName, kmClass.getProperties.nn.asScala.to(List))

              collate(entries, setters, identifiers, companion)

            case metadata: KotlinClassMetadata.FileFacade =>
              val kmPackage = metadata.getKmPackage.nn

              val entries =
                functions(typeName, kmPackage.getFunctions.nn.asScala.to(List), true)

              collate(entries, Map(), Nil, Unset)

            case metadata: KotlinClassMetadata.MultiFileClassFacade =>
              // Members live in the part classes; each records its *own* class as the JVM owner.
              val parts = metadata.getPartClassNames.nn.asScala.to(List).map(_.nn.tt)

              val entries = parts.flatMap: part =>
                val binary = part.s.replace("/", ".").nn.tt

                loadable(binary).let(metadataOf(_)).let:
                  case metadata: KotlinClassMetadata.MultiFileClassPart =>
                    functions(binary, metadata.getKmPackage.nn.getFunctions.nn.asScala.to(List),
                        true)

                  case _ =>
                    Nil

                . or(Nil)

              collate(entries, Map(), Nil, Unset)

            case _ =>
              Unset

        catch case _: Throwable => Unset

  private def metadataOf(cls: Class[?]): Optional[KotlinClassMetadata] =
    // Plain Java classes (android.jar et al.) carry no `@Metadata`; a reflection-based fallback
    // for them is deliberately deferred.
    Optional(cls.getAnnotation(classOf[kotlin.Metadata])).let: annotation =>
      try KotlinClassMetadata.readStrict(annotation).nn
      catch case _: Throwable => Unset

  private def functions(owner: Text, functions: List[KmFunction], static: Boolean): List[Entry] =
    functions.flatMap: function =>
      if Attributes.isSuspend(function) then Nil else
        Optional(JvmExtensionsKt.getSignature(function)).let: signature =>
          val parameters = function.getValueParameters.nn.asScala.to(List)

          val types = parameters.map: parameter =>
            foreignType(parameter.getType.nn)

          val defaults = parameters.map(Attributes.getDeclaresDefaultValue(_))

          List:
            Entry
              ( function.getName.nn.tt,
                JvmMember
                  ( owner, signature.getName.nn.tt, signature.getDescriptor.nn.tt, static,
                    false, false, parameters.length, defaults ),
                Prototype(types, foreignType(function.getReturnType.nn)) )

        . or(Nil)

  // A property reads through its getter when it has one; a `const` or `@JvmField` property
  // compiles to a bare (frequently static) field instead, read directly.
  private def properties(owner: Text, properties: List[KmProperty]): List[Entry] =
    properties.flatMap: property =>
      val result = foreignType(property.getReturnType.nn)

      Optional(JvmExtensionsKt.getGetterSignature(property)).let: signature =>
        List:
          Entry
            ( property.getName.nn.tt,
              JvmMember
                ( owner, signature.getName.nn.tt, signature.getDescriptor.nn.tt, false, true,
                  false, 0, Nil ),
              Prototype(Unset, result) )

      . or:
          Optional(JvmExtensionsKt.getFieldSignature(property)).let: signature =>
            List:
              Entry
                ( property.getName.nn.tt,
                  JvmMember
                    ( owner, signature.getName.nn.tt, signature.getDescriptor.nn.tt, false, true,
                      true, 0, Nil ),
                  Prototype(Unset, result) )

          . or(Nil)

  private def propertySetters(owner: Text, properties: List[KmProperty])
  :   Map[Text, JvmMember] =

    properties.flatMap: property =>
      Optional(JvmExtensionsKt.getSetterSignature(property)).let: signature =>
        List:
          property.getName.nn.tt ->
            JvmMember
              ( owner, signature.getName.nn.tt, signature.getDescriptor.nn.tt, false, true,
                false, 1, Nil )

      . or(Nil)

    . to(Map)

  private def constructors(owner: Text, kmClass: KmClass): List[Entry] =
    kmClass.getConstructors.nn.asScala.to(List).flatMap: constructor =>
      Optional(JvmExtensionsKt.getSignature(constructor)).let: signature =>
        val parameters = constructor.getValueParameters.nn.asScala.to(List)

        val types = parameters.map: parameter =>
          foreignType(parameter.getType.nn)

        val defaults = parameters.map(Attributes.getDeclaresDefaultValue(_))

        List:
          Entry
            ( t"<init>",
              JvmMember
                ( owner, signature.getName.nn.tt, signature.getDescriptor.nn.tt, false, false,
                  false, parameters.length, defaults ),
              Prototype(types, Foreign.Type.Named(owner)) )

      . or(Nil)

  private def collate
    ( entries:     List[Entry],
      setters:     Map[Text, JvmMember],
      identifiers: List[Int],
      companion:   Optional[Text] )
  :   Resolution =

    val prototypes: Map[Text, Prototype] =
      entries.groupBy(_.name).view.mapValues(_.head.prototype).to(Map)

    val jvmMembers: Map[Text, List[JvmMember]] =
      entries.groupBy(_.name).view.mapValues(_.map(_.member)).to(Map)

    Resolution(prototypes, jvmMembers, setters, identifiers, companion)

  // Maps a Kotlin metadata type to the ecosystem-neutral foreign form: named classes by their
  // fully-qualified Kotlin name, generic applications through `Applied`, nullability as a union
  // with `"null"`, and a type-parameter reference as `#<id>` — resolved against the receiver's
  // type arguments by `KotlinFacade`, and satisfiable by no argument elsewhere.
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

      case classifier: KmClassifier.TypeParameter =>
        Foreign.Type.Named(t"#${classifier.getId}")

      case _ =>
        Foreign.Type.Named(t"*")

    if Attributes.isNullable(tpe)
    then Foreign.Type.Union(List(base, Foreign.Type.Named(t"null")))
    else base
