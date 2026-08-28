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

import scala.collection.immutable.List as SList
import scala.collection.immutable.Map as SMap
import scala.collection.immutable.{::, Nil as SNil}
import scala.jdk.CollectionConverters.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import kotlin.metadata.*
import kotlin.metadata.jvm.*
import reliquary.*
import rudiments.*
import vacuous.*

// The atomization rules of `kotlin-metadata/1` (`kotlin.md`): the Kotlin declaration surface
// carried by the `@Metadata` annotation, read through the same published metadata library the
// Kotlin toolchain uses — never the bytecode.
//
// Keying is by membership: a Kotlin call site resolves members through the receiver, so a
// type's contract surface includes what it presents through its *Kotlin* supertype closure. A
// supertype carrying no metadata — a Java class, or a virtual builtin such as `kotlin.Any` —
// contributes nothing here: its surface is a classfile-level concern, not this carrier's.
object KotlinMetadataAtomizer:
  val id: Text = t"kotlin-metadata/1"

  private def malformed(detail: Text): Discipline.Error =
    import errorDiagnostics.emptyDiagnostics
    Discipline.Error(id, Discipline.Error.Reason.Malformed(detail))

  // --- canonical binary encoding -------------------------------------------------------------

  private def uvarint(out: java.io.ByteArrayOutputStream, value0: Long): Unit =
    var value = value0

    while value >= 0x80L do
      out.write(((value & 0x7f) | 0x80).toInt)
      value >>>= 7

    out.write(value.toInt)

  private def utf8(out: java.io.ByteArrayOutputStream, text: Text): Unit =
    val bytes = text.s.getBytes("UTF-8").nn
    uvarint(out, bytes.length.toLong)
    out.write(bytes)

  private def tag(out: java.io.ByteArrayOutputStream, char: Char): Unit = out.write(char.toInt)

  private def flag(out: java.io.ByteArrayOutputStream, value: Boolean): Unit =
    out.write(if value then 1 else 0)

  private def hash(encode: java.io.ByteArrayOutputStream => Unit): Data =
    val out = java.io.ByteArrayOutputStream()
    encode(out)
    Lira.Hash(Lira.Hash.Domain.Atom(id), Array.unsafeFrozen(out.toByteArray.nn))

  // --- type rendering -------------------------------------------------------------------------

  // Kotlin spelling with nullability marks; type parameters as `#<id>` (metadata ids are
  // already declaration-order indices, so binder names never enter a hash).
  private def render(tpe: KmType): Text =
    val base: String = tpe.classifier match
      case classifier: KmClassifier.Class =>
        val name = classifier.getName.nn.replace("/", ".").nn

        val arguments = tpe.getArguments.nn.asScala.to(SList).map: projection =>
          Optional(projection.getType).let { inner => render(inner).s }.or("*")

        if arguments.isEmpty then name else s"$name<${arguments.mkString(",")}>"

      case classifier: KmClassifier.TypeAlias =>
        classifier.getName.nn.replace("/", ".").nn

      case classifier: KmClassifier.TypeParameter => s"#${classifier.getId}"
      case _                                      => "*"

    if Attributes.isNullable(tpe) then Text(s"$base?") else Text(base)

  private def parameterTypes(function: KmFunction): Text =
    val types = function.getValueParameters.nn.asScala.to(SList).map: parameter =>
      render(parameter.getType.nn).s

    Text(types.mkString(","))

  // --- member atoms ---------------------------------------------------------------------------

  private def visible(visibility: Visibility): Boolean =
    visibility == Visibility.PUBLIC || visibility == Visibility.PROTECTED

  private def functionKey(owner: Text, function: KmFunction): Text =
    t"$owner#${function.getName.nn.tt}(${parameterTypes(function)})"

  private def functionAtom(owner: Text, function: KmFunction, static: Boolean): Atom =
    Atom(functionKey(owner, function), Atom.Class.Rigid, hash: out =>
      tag(out, 'f')
      utf8(out, owner)
      utf8(out, function.getName.nn.tt)
      flag(out, static)
      flag(out, Attributes.isSuspend(function))
      flag(out, Attributes.isOperator(function))
      flag(out, Attributes.isInfix(function))
      flag(out, Attributes.isInline(function))
      uvarint(out, Attributes.getVisibility(function).ordinal.toLong)
      uvarint(out, Attributes.getModality(function).ordinal.toLong)

      Optional(function.getReceiverParameterType)
      . lay(tag(out, '0')) { receiver => tag(out, '1'); utf8(out, render(receiver)) }

      val parameters = function.getValueParameters.nn.asScala.to(SList)
      uvarint(out, parameters.length.toLong)

      // Parameter names are named-argument surface, and a default's *existence* is call
      // surface; both fold (`kotlin.md` §6).
      parameters.foreach: parameter =>
        utf8(out, parameter.getName.nn.tt)
        flag(out, Attributes.getDeclaresDefaultValue(parameter))
        utf8(out, render(parameter.getType.nn))

      utf8(out, render(function.getReturnType.nn)))

  private def propertyAtom(owner: Text, property: KmProperty, static: Boolean): Atom =
    Atom(t"$owner.${property.getName.nn.tt}", Atom.Class.Rigid, hash: out =>
      tag(out, 'p')
      utf8(out, owner)
      utf8(out, property.getName.nn.tt)
      flag(out, static)
      flag(out, Attributes.isVar(property))
      uvarint(out, Attributes.getVisibility(property).ordinal.toLong)
      uvarint(out, Attributes.getModality(property).ordinal.toLong)

      Optional(property.getReceiverParameterType)
      . lay(tag(out, '0')) { receiver => tag(out, '1'); utf8(out, render(receiver)) }

      utf8(out, render(property.getReturnType.nn)))

  private def constructorAtom(owner: Text, constructor: KmConstructor): Atom =
    val types = constructor.getValueParameters.nn.asScala.to(SList).map: parameter =>
      render(parameter.getType.nn).s

    Atom(t"$owner#constructor(${Text(types.mkString(","))})", Atom.Class.Rigid, hash: out =>
      tag(out, 'c')
      utf8(out, owner)
      uvarint(out, Attributes.getVisibility(constructor).ordinal.toLong)

      val parameters = constructor.getValueParameters.nn.asScala.to(SList)
      uvarint(out, parameters.length.toLong)

      parameters.foreach: parameter =>
        utf8(out, parameter.getName.nn.tt)
        flag(out, Attributes.getDeclaresDefaultValue(parameter))
        utf8(out, render(parameter.getType.nn)))

  private def aliasAtom(owner: Text, alias: KmTypeAlias): Atom =
    Atom(t"$owner.${alias.getName.nn.tt}", Atom.Class.Rigid, hash: out =>
      tag(out, 'a')
      utf8(out, owner)
      utf8(out, alias.getName.nn.tt)
      utf8(out, render(alias.getUnderlyingType.nn)))

  // --- membership -----------------------------------------------------------------------------

  // The presented member set of one class: its own visible functions and properties, then those
  // of its Kotlin supertype closure, nearest first, the first occurrence of each selector
  // winning — an override shadows what it overrides.
  private def presented
    ( kmClass: KmClass,
      resolve: Text => Optional[KmClass] )
  :   (SList[KmFunction], SList[KmProperty]) =

    val seenFunctions = scala.collection.mutable.LinkedHashSet[String]()
    val seenProperties = scala.collection.mutable.LinkedHashSet[String]()
    val functions = scala.collection.mutable.ListBuffer[KmFunction]()
    val properties = scala.collection.mutable.ListBuffer[KmProperty]()
    val visited = scala.collection.mutable.HashSet[String]()

    def walk(current: KmClass): Unit =
      if visited.add(current.getName.nn) then
        current.getFunctions.nn.asScala.foreach: function =>
          if visible(Attributes.getVisibility(function))
          && seenFunctions.add(s"${function.getName}(${parameterTypes(function)})")
          then functions += function

        current.getProperties.nn.asScala.foreach: property =>
          if visible(Attributes.getVisibility(property))
          && seenProperties.add(property.getName.nn)
          then properties += property

        current.getSupertypes.nn.asScala.foreach: supertype =>
          supertype.classifier match
            case classifier: KmClassifier.Class =>
              resolve(classifier.getName.nn.tt).let(walk(_))

            case _ => ()

    walk(kmClass)
    (functions.to(SList), properties.to(SList))

  // --- class atoms ----------------------------------------------------------------------------

  private def classAtoms
    ( kmClass: KmClass,
      resolve: Text => Optional[KmClass] )
  :   SList[Atom] =

    val owner = kmClass.getName.nn.replace("/", ".").nn.tt
    val atoms = scala.collection.mutable.ListBuffer[Atom]()
    val (functions, properties) = presented(kmClass, resolve)

    functions.foreach { function => atoms += functionAtom(owner, function, false) }
    properties.foreach { property => atoms += propertyAtom(owner, property, false) }

    kmClass.getConstructors.nn.asScala.foreach: constructor =>
      if visible(Attributes.getVisibility(constructor))
      then atoms += constructorAtom(owner, constructor)

    val modality = Attributes.getModality(kmClass)

    // Iff the class is open to subclassing, the sorted key list of its abstract members folds
    // into its own atom — the rule of `tasty.md` §8 rule 5, transposed (`kotlin.md` §6).
    val abstracts =
      if modality == Modality.FINAL then SList() else
        functions.filter { function => Attributes.getModality(function) == Modality.ABSTRACT }
        . map { function => functionKey(owner, function).s }
        . ++ (properties
            .filter { property => Attributes.getModality(property) == Modality.ABSTRACT }
            .map { property => s"$owner.${property.getName}" })
        . sorted

    // A data class's constructor parameters fold into the class atom: `copy`'s signature
    // changes when any is added, so the addition is correctly major.
    val dataParameters =
      if !Attributes.isData(kmClass) then SList() else
        kmClass.getConstructors.nn.asScala.to(SList)
        . filter { constructor => !Attributes.isSecondary(constructor) }
        . flatMap: constructor =>
            constructor.getValueParameters.nn.asScala.to(SList).map: parameter =>
              s"${parameter.getName}:${render(parameter.getType.nn)}"

    atoms += Atom(owner, Atom.Class.Rigid, hash: out =>
      tag(out, 'K')
      utf8(out, owner)
      uvarint(out, Attributes.getKind(kmClass).ordinal.toLong)
      uvarint(out, modality.ordinal.toLong)
      uvarint(out, Attributes.getVisibility(kmClass).ordinal.toLong)
      flag(out, Attributes.isData(kmClass))
      flag(out, Attributes.isValue(kmClass))

      val typeParameters = kmClass.getTypeParameters.nn.asScala.to(SList)
      uvarint(out, typeParameters.length.toLong)

      typeParameters.foreach: parameter =>
        uvarint(out, parameter.getVariance.nn.ordinal.toLong)
        val bounds = parameter.getUpperBounds.nn.asScala.to(SList).map(render(_).s)
        utf8(out, Text(bounds.mkString(",")))

      // Supertypes keep declaration order (linearization is semantic); sealed subclasses and
      // abstract-member keys sort; enum entries keep declaration order (ordinals are surface).
      val supertypes = kmClass.getSupertypes.nn.asScala.to(SList).map(render(_).s)
      uvarint(out, supertypes.length.toLong)
      supertypes.foreach { supertype => utf8(out, Text(supertype)) }

      val sealedList = kmClass.getSealedSubclasses.nn.asScala.to(SList)
        . map(_.toString.replace("/", ".").nn)
        . sorted

      uvarint(out, sealedList.length.toLong)
      sealedList.foreach { subclass => utf8(out, Text(subclass)) }

      val entries = kmClass.getEnumEntries.nn.asScala.to(SList)
      uvarint(out, entries.length.toLong)
      entries.foreach { entry => utf8(out, Text(entry.toString)) }

      uvarint(out, abstracts.length.toLong)
      abstracts.foreach { key => utf8(out, Text(key)) }

      uvarint(out, dataParameters.length.toLong)
      dataParameters.foreach { parameter => utf8(out, Text(parameter)) })

    atoms.to(SList)

  private def packageAtoms(owner: Text, kmPackage: KmPackage): SList[Atom] =
    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    kmPackage.getFunctions.nn.asScala.foreach: function =>
      if visible(Attributes.getVisibility(function))
      then atoms += functionAtom(owner, function, true)

    kmPackage.getProperties.nn.asScala.foreach: property =>
      if visible(Attributes.getVisibility(property))
      then atoms += propertyAtom(owner, property, true)

    kmPackage.getTypeAliases.nn.asScala.foreach: alias =>
      if visible(Attributes.getVisibility(alias))
      then atoms += aliasAtom(owner, alias)

    atoms.to(SList)

  // --- entry point ----------------------------------------------------------------------------

  // Atomizes the metadata-carrying classes among `content`, resolving supertypes through the
  // release's own classes first and the classpath second.
  def atomize(content: SList[(Text, Data)], classpath: SList[Text])
  :   List[Atom] raises Discipline.Error =

    val own: SMap[String, Data] = content.map { (name, data) => (name.s, data) }.toMap

    val urls = classpath.map { entry => java.io.File(entry.s).toURI.nn.toURL.nn }

    val loader: ClassLoader =
      new java.net.URLClassLoader(urls.toArray, getClass.getClassLoader):
        override def findClass(name: String | Null): Class[?] =
          own.get(name.nn) match
            case scala.Some(bytes) =>
              defineClass(name, Array.unsafeJvm(bytes), 0, bytes.length).nn

            case _ => super.findClass(name).nn

    def load(binary: String): Optional[Class[?]] =
      try Class.forName(binary, false, loader).nn catch case _: Throwable => Unset

    // A metadata class name (`a/b/Outer.Inner`) resolves by trying each nesting split.
    def loadMetadataName(name: Text): Optional[Class[?]] =
      val dotted = name.s.replace("/", ".").nn

      def attempt(candidate: String): Optional[Class[?]] = load(candidate)

      def variants(name: String): SList[String] =
        val indices = name.indices.filter(name.charAt(_) == '.').reverse.to(SList)
        name :: indices.map { index => name.updated(index, '$') }

      def search(candidates: SList[String]): Optional[Class[?]] = candidates match
        case SNil => Unset

        case candidate :: rest =>
          attempt(candidate) match
            case cls: Class[?] => cls
            case _             => search(rest)

      search(variants(dotted))

    val metadataCache = scala.collection.mutable.HashMap[String, Optional[KmClass]]()

    def kmClassOf(metadataName: Text): Optional[KmClass] =
      metadataCache.getOrElseUpdate(metadataName.s,
        loadMetadataName(metadataName).let: cls =>
          Optional(cls.getAnnotation(classOf[kotlin.Metadata])).let: annotation =>
            try
              KotlinClassMetadata.readStrict(annotation).nn match
                case metadata: KotlinClassMetadata.Class => metadata.getKmClass.nn
                case _                                   => Unset

            catch case _: Exception => Unset)

    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    content.foreach: (binary, data) =>
      load(binary.s).let: cls =>
        Optional(cls.getAnnotation(classOf[kotlin.Metadata])).let: annotation =>
          val metadata =
            try KotlinClassMetadata.readStrict(annotation).nn catch case error: Exception =>
              abort(malformed(t"the metadata of $binary is unreadable"))

          metadata match
            case metadata: KotlinClassMetadata.Class =>
              atoms ++= classAtoms(metadata.getKmClass.nn, kmClassOf(_))

            case metadata: KotlinClassMetadata.FileFacade =>
              atoms ++= packageAtoms(binary, metadata.getKmPackage.nn)

            case metadata: KotlinClassMetadata.MultiFileClassPart =>
              val facade = metadata.getFacadeClassName.nn.replace("/", ".").nn.tt
              atoms ++= packageAtoms(facade, metadata.getKmPackage.nn)

            // The multi-file facade's parts carry its content, and a synthetic class carries
            // none: claimed, but contributing no atoms.
            case _ => ()

    atoms.toList.to(List)
