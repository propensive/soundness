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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package nomenclature

import scala.compiletime.*
import scala.quoted.*

import dotty.tools.dotc.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gigantism.*
import rudiments.*

object protointernal:
  def build(using Quotes)(todo: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    todo match
      case Nil => TypeRepr.of[Zero]

      case next :: todo =>
        next.asType.absolve match
          case '[next] => build(todo).asType.absolve match
            case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[next *: tupleType]

  def decompose(using Quotes)(repr: quotes.reflect.TypeRepr): Set[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    repr.dealias.asMatchable match
      case AndType(left, right) => decompose(left) ++ decompose(right)
      case other                => Set(other)

  def disintersection[intersection: Type]: Macro[Tuple] =
    import quotes.reflect.*

    build(decompose(TypeRepr.of[intersection].dealias).to(List)).asType.absolve match
      case '[type tupleType <: Tuple; tupleType] => '{null.asInstanceOf[tupleType]}

  // Collects the reference trees of every `Nominative` given instance in scope at the macro
  // expansion point. Modelled on `frontier.internal.every`: it repeatedly runs an implicit search
  // that ignores the symbols already found, folding ambiguous candidate pairs in, so that all
  // matching givens are recovered rather than just the single best one.
  def everyNominative(using Quotes)(): List[quotes.reflect.Term] =
    import quotes.reflect.*

    given context: core.Contexts.Context = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl => quotes.ctx

    def underlying(tree: Term): Symbol = tree match
      case Inlined(_, _, body) => underlying(body)
      case Apply(fun, _)       => underlying(fun)
      case TypeApply(fun, _)   => underlying(fun)
      case Block(_, expr)      => underlying(expr)
      case Typed(expr, _)      => underlying(expr)
      case other               => other.symbol

    def collect(ignored: List[Symbol], acc: List[Term]): List[Term] =
      Implicits.searchIgnoring(TypeRepr.of[Nominative])(ignored*).absolve match
        case success: ImplicitSearchSuccess =>
          val symbol = underlying(success.tree)

          if symbol.isNoSymbol || ignored.contains(symbol) then acc.reverse
          else collect(symbol :: ignored, success.tree :: acc)

        case failure: ImplicitSearchFailure =>
          failure.asInstanceOf[ast.tpd.Tree].tpe match
            case ambiguous: typer.Implicits.AmbiguousImplicits =>
              val tree1 = ambiguous.alt1.tree.asInstanceOf[Term]
              val tree2 = ambiguous.alt2.tree.asInstanceOf[Term]
              val symbol1 = underlying(tree1)
              val symbol2 = underlying(tree2)

              val unusable =
                symbol1.isNoSymbol || symbol2.isNoSymbol ||
                  ignored.contains(symbol1) || ignored.contains(symbol2)

              if unusable then acc.reverse
              else collect(symbol1 :: symbol2 :: ignored, tree2 :: tree1 :: acc)

            case _ =>
              acc.reverse

    collect(Nil, Nil)

  // Tests `name` against every rule in the `limit` intersection, returning `true` only if all
  // rules accept it. A rule whose parameter is not a string literal (and so cannot be evaluated at
  // compile time) causes the plane to be excluded rather than failing compilation.
  def planeAccepts(using Quotes)(name: Text, limit: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    decompose(limit).forall: repr =>
      repr.asMatchable match
        case AppliedType(_, List(param)) =>
          param.asMatchable match
            case ConstantType(StringConstant(text)) =>
              companion[Rule](repr.typeSymbol).check(name, text.tt)

            case _ =>
              false

        case _ =>
          false

  def extractor(context: Expr[StringContext]): Macro[Any] =
    import quotes.reflect.*

    val string = context.valueOrAbort.parts.head

    ConstantType(StringConstant(string)).asType match
      case '[type stringType <: Label; stringType] => '{NameExtractor[stringType]()}

      case _ =>
        panic(m"StringContext did not contains Strings")

  // Infers the most precise `Name` type for the literal `text`: the intersection of the `Self`
  // planes of every in-scope `Nominative` whose rules all accept it. If no plane in scope accepts
  // the identifier, compilation fails.
  def inferName[text <: Label: Type]: Macro[Any] =
    import quotes.reflect.*

    val name: Text = constant[text].tt

    val planes: List[TypeRepr] =
      everyNominative().flatMap: tree =>
        tree.asExpr match
          case '{ type self
                  type limit
                  type nominative <: Nominative { type Self = self; type Limit = limit }
                  $value: nominative } =>

            if planeAccepts(name, TypeRepr.of[limit]) then List(TypeRepr.of[self]) else Nil

          case _ =>
            Nil

      . foldLeft(List[TypeRepr]()): (acc, self) =>
          if acc.exists(_ =:= self) then acc else self :: acc

    if planes.nil
    then halt(914, m"the name $name is not a valid identifier in any namespace in scope")
    else planes.reduce(AndType(_, _)).asType.absolve match
      case '[type plane; plane] => '{${Expr(name)}.asInstanceOf[Name[plane]]}

  def makeName[system: Type](name: Expr[Text]): Macro[Name[system]] =
    import quotes.reflect.*

    Expr.summon[system is Nominative].absolve match
      case Some('{type limit; $nominative: (Nominative { type Limit = limit })}) =>
        val checks =
          decompose(TypeRepr.of[limit]).to(List).map(_.asType).foldLeft('{()}): (expr, next) =>
            next.absolve match
              case '[type param <: String; type rule <: Check[param]; rule] =>
                anteprotointernal.staticCompanion[rule].absolve match
                  case '{$rule: Rule} =>
                    TypeRepr.of[param].absolve match
                      case ConstantType(StringConstant(string)) =>
                        ' {
                            if $rule.check($name, ${Expr(string)}.tt) then $expr
                            else provide[Tactic[NameError]]:
                              raise(NameError($name, $rule, ${Expr(string)}))
                          }

        '{$checks; $name.asInstanceOf[Name[system]]}

      case None =>
        halt(89, m"Couldn't find a `Nominative` instance on ${TypeRepr.of[system].show}")


  def parse2[plane: Type, name <: String: Type](scrutinee: Expr[Name[plane]])
  :   Macro[Boolean] =

    parse[plane, name]
    '{${Expr(constant[name])}.tt == $scrutinee}


  def constant[text <: String: Type](using Quotes): text =
    import quotes.reflect.*

    TypeRepr.of[text].asMatchable.absolve match
      case ConstantType(StringConstant(value)) => value.tt.asInstanceOf[text]

  def companion[companion: Typeable](using Quotes)(symbol: quotes.reflect.Symbol): companion =
    Class.forName(s"${symbol.companionModule.fullName}$$").nn.getField("MODULE$").nn.get(null) match
      case module: `companion` => module
      case _                   => halt(704, m"The companion object did not have the expected type.")

  def parse[plane: Type, name <: String: Type]: Macro[Name[plane]] =
    import quotes.reflect.*

    val name: Text = constant[name].tt

    Expr.summon[plane is Nominative] match
      case
        Some
          ( ' {
                type limit
                type nominative <: Nominative { type Limit = limit }
                $value: nominative
              } ) =>

        decompose(TypeRepr.of[limit]).to(List).each: repr =>
          val text = repr.asMatchable match
            case AppliedType(_, List(param)) =>
              param.asMatchable match
                case ConstantType(StringConstant(text)) => text.tt
                case _                                  => halt(487, m"Bad type")

            case _ =>
              halt(487, m"Bad type")

          val rule = companion[Rule](repr.typeSymbol)

          if !rule.check(name, text)
          then halt(789, m"the name is not valid because it ${rule.describe(text)}")

      case _ =>
        halt(613, m"Could not access constraint")


    '{${Expr(name)}.asInstanceOf[Name[plane]]}
