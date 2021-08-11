package xylophone

import wisteria.*
import rudiments.*

trait XmlReader[T]:
  def read(xml: Seq[Ast]): Option[T]
  def map[S](fn: T => Option[S]): XmlReader[S] = read(_).flatMap(fn(_))

object XmlReader extends Derivation[XmlReader]:
  given string: XmlReader[String] =
    childElements(_).collect { case Ast.Text(txt) => txt }.headOption
  
  given XmlReader[Int] = string.map(Int.unapply(_))
  
  def join[T](caseClass: CaseClass[XmlReader, T]): XmlReader[T] = seq =>
    val elems = childElements(seq)
    caseClass.constructMonadic { param =>
      elems
        .collect { case e: Ast.Element => e }
        .find(_.name.name == param.label)
        .flatMap { e => param.typeclass.read(Seq(e)) }
    }
  
  def split[T](sealedTrait: SealedTrait[XmlReader, T]): XmlReader[T] = seq =>
    seq.headOption match
      case Some(Ast.Element(_, children, attributes, _)) =>
        attributes
          .get(XmlName("type"))
          .flatMap { t => sealedTrait.subtypes.find(_.typeInfo.short == t) }
          .flatMap(_.typeclass.read(seq))
      case _ =>
        None
  
  private def childElements(seq: Seq[Ast]): Seq[Ast] =
    seq.collect { case e@Ast.Element(_, children, _, _) => children }.flatten
