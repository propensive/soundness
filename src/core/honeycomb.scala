package honeycomb

import scala.annotation.*
import scala.quoted.*

import language.dynamics

type Label = String & Singleton
type Content[Children <: Label] = Item[Children] | String
type Attributes = Map[String, String | Boolean]

trait Item[+Name <: Label]:
  def label: String
  def attributes: Attributes
  def children: Seq[Content[?]]
  def inline: Boolean
  def unclosed: Boolean
  def verbatim: Boolean

object Node:
  @targetName("make")
  def apply[T <: Label, C <: Label]
           (label: String,
            unclosed: Boolean,
            inline: Boolean,
            verbatim: Boolean,
            atts: Attributes,
            children: Seq[Content[C] | Seq[Content[C]]] = Nil): Node[T] =
    new Node(label, unclosed, inline, verbatim, atts, flatten(children))

  private def flatten[C <: Label](nodes: Seq[Content[C] | Seq[Content[C]]]): Seq[Content[C]] = nodes.flatMap {
    case seq: Seq[?]      => seq
    case node: Content[?] => Seq(node)
  }.asInstanceOf[Seq[Content[C]]]

case class Tag[+Name <: Label, Children <: Label, Atts <: Label]
              (label: Name, unclosed: Boolean = false, inline: Boolean = false, verbatim: Boolean = false)
extends Item[Name], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil

  inline def applyDynamicNamed(method: "apply")(inline attributes: (Atts, Any)*): Element[Name, Children] =
    ${Macro.read[Name, Children, Children]('label, 'unclosed, 'inline, 'verbatim, 'attributes)}

  def applyDynamic(method: "apply")(children: (Content[Children] | Seq[Content[Children]])*): Node[Name] =
    Node(label, unclosed, inline, verbatim, Map(), children)

case class TransTag[+Name <: Label, Children <: Label, Atts <: Label]
                   (label: Name, unclosed: Boolean = false, inline: Boolean = false, verbatim: Boolean = false)
extends Item[Name], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil

  inline def applyDynamicNamed(method: "apply")(inline attributes: (Atts, Any)*): Element[Name, Children] =
    ${Macro.read[Name, Children, Children]('label, 'unclosed, 'inline, 'verbatim, 'attributes)}

  def applyDynamic[Return <: Label]
                  (method: "apply")(children: (Content[Return] | Seq[Content[Return]])*): Node[Return] =
    Node(label, unclosed, inline, verbatim, Map(), children)

case class Element[+Name <: Label, Children <: Label]
                  (label: Name, unclosed: Boolean, inline: Boolean, verbatim: Boolean, attributes: Attributes)
extends Item[Name]:
  def children = Nil

  def apply(children: (Content[Children] | Seq[Content[Children]])*): Node[Name] =
    Node(label, unclosed, inline, verbatim, attributes, children)

case class Node[+Name <: Label](label: String, unclosed: Boolean, tagInline: Boolean, verbatim: Boolean, attributes: Attributes, children: Seq[Content[?]])
extends Item[Name]:
  lazy val inline: Boolean = tagInline && children.forall {
    case node: Item[?] => node.inline
    case text: String  => true
  }

object Macro:
  def read[Name <: Label: Type, Children <: Label: Type, Return <: Label: Type]
          (name: Expr[Name], unclosed: Expr[Boolean], inline: Expr[Boolean], verbatim: Expr[Boolean], attributes: Expr[Seq[(Label, Any)]])
          (using Quotes): Expr[Element[Name, Return]] =
    import quotes.reflect.{Singleton => _, *}

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[(String, String | Boolean)]] =
      exprs match
        case '{($key: k & Label, $value: v)} +: tail =>
          val att = key.valueOrError
          val exp: Expr[Attribute[k & Label, v, Name]] =
            Expr.summon[Attribute[k & Label, v, Name]].getOrElse {
              report.error(s"honeycomb: the attribute $att cannot take a value of type ${TypeRepr.of[v].show}")
              ???
            }
          
          '{($exp.rename.getOrElse($key), $exp.convert($value))} :: recur(tail)
        
        case _ =>
          Nil

    attributes match
      case Varargs(exprs) => '{Element($name, $unclosed, $inline, $verbatim, ${Expr.ofSeq(recur(exprs))}.to(Map))}

case class HtmlDoc(root: Item["html"])

object HtmlDoc:
  def serialize[T](doc: HtmlDoc, maxWidth: Int = -1)(using HtmlSerializer[T]): T =
    summon[HtmlSerializer[T]].serialize(doc, maxWidth)
