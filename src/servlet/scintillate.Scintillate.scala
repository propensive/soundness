package scintillate

import scala.quoted.*
import scala.annotation.*

import fulminate.*

class servlet extends MacroAnnotation:
  override def transform(using Quotes)
      (tree: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition])
          : List[quotes.reflect.Definition] =
    import quotes.reflect.*

    tree match
      case defDef@DefDef(name, params, returnType, Some(body)) =>
        if !(returnType.tpe <:< TypeRepr.of[HttpResponse[?]])
        then abandon(m"the return type ${returnType.show} is not a subtype of HttpResponse[?]")
        val ref = Ref(defDef.symbol).etaExpand(tree.symbol.owner).asExprOf[HttpRequest => HttpResponse[?]]
        val parents0 = List('{new JavaServletFn($ref)}.asTerm)
        val parents = List(TypeTree.of[HttpRequest])
        val newClassName = Symbol.freshName(name)
        val cls = Symbol.newClass(Symbol.spliceOwner, name, parents.map(_.tpe), _ => Nil, selfType = None)
        val clsDef = ClassDef(cls, parents, body = Nil)
        List(tree, clsDef)

      case other =>
        abandon(m"the @servlet annotation must be applied to a method")
