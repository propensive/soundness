package frontier

import scala.quoted.*

import anticipation.*
import dendrology.*
import escapade.*
import fulminate.*
import gossamer.*
import iridescence.*
import prepositional.*
import proscenium.*
import spectacular.*
import stenography.*

import dotty.tools.dotc.*

object SafeInlined:
  def unapply(using Quotes)(scrutinee: quotes.reflect.ImplicitSearchFailure)
  : Option[(Option[quotes.reflect.Tree], List[quotes.reflect.Definition], quotes.reflect.Term)] =

      import quotes.reflect.*
      try
        scrutinee match
          case inlined: Inlined => Some((inlined.call, inlined.bindings, inlined.body))
          case _                => None
      catch case error: Throwable => None


object NoCandidates:
  def unapply(using Quotes)(scrutinee: quotes.reflect.ImplicitSearchFailure): Option[Text] =
    scrutinee match
      case _: dotty.tools.dotc.ast.untpd.SearchFailureIdent => Some(scrutinee.explanation.tt)
      case _ => None

object Frontier:

  given treeStyle: [text: Textual] => TextualTreeStyle[text] =
    TextualTreeStyle(t"   ", t" └─", t" ├─", t" │ ")

  private var counter = 0

  private def next(): Int =
    counter = counter + 1
    counter

  given realm: Realm = realm"frontier"

  def explain[target: Type]: Macro[target] =
    import quotes.reflect.*
    val id = next()
    def log(message: String, depth: Int): Unit = ()//)println(s"[$id]: ${"  "*depth}${message}")
    log(s"new macro invocation for ${Stenography.name[target]}", 0)

    val self = Symbol.requiredMethod("frontier.missingContext.explain")

    sealed trait Result

    given context: core.Contexts.Context = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl => quotes.ctx

    case class Candidate(name: Text, missing: List[Result]) extends Result
    case class Missing(name: Text, candidates: List[Candidate]) extends Result
    case class Found(name: Text, expr: Expr[Any]) extends Result

    def seek(repr: TypeRepr, exclusions: List[Symbol], depth: Int): Result =
      log(s"searching for ${Stenography.name(repr)}", depth)
      Implicits.searchIgnoring(repr)(self :: exclusions*) match
        case success: ImplicitSearchSuccess =>
          log(s"found ${Ref(success.tree.symbol).show}", depth)
          Found(t"${Ref(success.tree.symbol).show}: ${Stenography.name(repr)}", success.tree.asExpr)

        case failure: ImplicitSearchFailure =>
          log(s"${Stenography.name(repr).show} is missing", depth)

          failure match
            case x: dotty.tools.dotc.ast.untpd.SearchFailureIdent =>
              log(s"no candidates for ${Stenography.name(repr).show}", depth)
              Missing(Stenography.name(repr), Nil)

            case SafeInlined(call, bindings, body) =>
              log(s"found inline given for ${Stenography.name(repr).show}", depth)
              call match
                case Some(term) =>
                  log(s"found inline given at ${Ref(term.symbol).show}", depth)
                  term match
                    case TypeApply(left, right) =>
                      Candidate("???", List(seek(right.head.tpe, Nil, depth + 1)))

                case None =>
                  Missing(Stenography.name(repr), Nil)

            case Apply(fun, args) =>
              def resolve(methodType: TypeRepr): Missing = methodType match
                case MethodType(_, types, _) =>
                  log(s"found method ${Ref(fun.symbol).show}", depth)
                  val candidate = Candidate(Stenography.name(fun.symbol.termRef).show.skip(5, Rtl), types.map(seek(_, Nil, depth + 1)))

                  val candidates = seek(repr, fun.symbol :: exclusions, depth) match
                    case Missing(_, candidates) => candidate :: candidates
                    case _                      => Nil

                  Missing(Stenography.name(repr), candidates)

                case polytype@PolyType(_, _, _) =>
                  log(s"found polymethod ${Ref(fun.symbol).show}", depth)
                  resolve(fun.tpe.simplified)

                case other =>
                  halt(m"nothing: ${other.show}")

              resolve(fun.symbol.info)


    val result = seek(TypeRepr.of[target], Nil, 1)

    result match
      case Found(name, expr) =>
        log(s"returning $name for ${Stenography.name[target]}", 0)
        expr.asExprOf[target]
      case other =>
        log(s"did not find a result", 0)
        val Missing(_, results) = result
        report.errorAndAbort:
          given Result is Expandable =
            case Candidate(_, missing)  => missing
            case Missing(_, candidates) => candidates
            case Found(_, _)            => Nil

          TreeDiagram[Result](results*).render:
            case Found(name, _)     => e" ✓ found $Italic($name)"
            case Missing(name, _)   => e" ✗ requires $Italic($name)"
            case Candidate(name, _) => e" ◆ candidate $Italic($name)"
          . join
              ( e"contextual value not found\n\n ■ resolving ${Stenography.name[target]}\n",
                e"\n",
                e"\n" )
          . render(termcapDefinitions.xterm256)
          . s
