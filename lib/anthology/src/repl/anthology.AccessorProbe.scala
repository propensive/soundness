package anthology

import scala.quoted.*
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.quoted.PickledQuotes

// Probe: build `{ val session = ReplBridge.takePending(); def x: Int = ... }`
// via reflection and pickle to TASTy. Validates parameterless-def construction,
// a session val, a sibling reference, and the closed-block pickle round-trip.
object AccessorProbe:
  inline def pickle: List[String] = ${pickleMacro}

  def pickleMacro(using Quotes): Expr[List[String]] =
    import quotes.reflect.*

    val owner = Symbol.spliceOwner

    val getterSymbol = Symbol.newMethod(owner, "x", ByNameType(TypeRepr.of[Int]))

    val getter = DefDef(getterSymbol, _ => Some('{ReplBridge.fetchLive[Int]("x")}.asTerm))

    // The quote-built bodies carry `Inlined` nodes whose `call` references this
    // (`@experimental`) object; strip them so the recompiled tree refers only to
    // `ReplBridge` and stdlib symbols.
    val stripper = new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
        case Inlined(_, Nil, expansion) => transformTerm(expansion)(owner)
        case other                      => super.transformTerm(other)(owner)

    val block = Block(List(getter), '{()}.asTerm)
    val stripped = stripper.transformTerm(block)(owner)
    val tree = Inlined(None, Nil, stripped).asInstanceOf[tpd.Tree]
    val context = quotes.asInstanceOf[QuotesImpl].ctx

    Expr(PickledQuotes.pickleQuote(tree)(using context))
