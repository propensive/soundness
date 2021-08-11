package xylophone

import wisteria.*
import rudiments.*

object XmlWriter extends Derivation[XmlWriter]:
  given XmlWriter[String] = str =>
    Ast.Element(XmlName("String"), List(Ast.Text(str)))

  given [T: XmlWriter]: XmlWriter[List[T]] = xs =>
    Ast.Element(XmlName("List"), xs.map(summon[XmlWriter[T]].write(_)))

  given XmlWriter[Int] = int =>
    Ast.Element(XmlName("Int"), List(Ast.Text(int.toString)))

  private val attributeAttribute = xmlAttribute()

  def join[T](caseClass: CaseClass[XmlWriter, T]): XmlWriter[T] = value =>
    val elements =
      caseClass.params
        .filter(!_.annotations.contains(attributeAttribute))
        .map { p => p.typeclass.write(p.deref(value)).copy(name = XmlName(p.label)) }

    val attributes =
      caseClass.params
        .filter(_.annotations.contains(attributeAttribute))
        .map { p => XmlName(p.label) -> textElements(p.typeclass.write(p.deref(value))) }
        .to(Map)

    Ast.Element(XmlName(caseClass.typeInfo.short), elements, attributes)

  def split[T](sealedTrait: SealedTrait[XmlWriter, T]): XmlWriter[T] = value =>
    sealedTrait.choose(value) { subtype =>
      val xml = subtype.typeclass.write(subtype.cast(value))
      Ast.Element(
        XmlName(sealedTrait.typeInfo.short),
        xml.children,
        xml.attributes.updated(XmlName("type"), xml.name.name),
        xml.namespaces
      )
    }
  
  private def textElements(value: Ast.Element): String =
    value.children.collect { case Ast.Text(txt) => txt }.join

trait XmlWriter[T]:
  def write(value: T): Ast.Element
  def contraMap[S](fn: S => T): XmlWriter[S] = value => write(fn(value))

