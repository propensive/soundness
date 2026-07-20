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
package cataclysm

import scala.compiletime.asMatchable
import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import hellenism.*
import vacuous.*

// Compile-time machinery behind `Styles` and the `cssBindings.checkedBinding` given. A
// `Styles` marker carries a stylesheet's classpath path as its `Locus` type; the
// resource is read and parsed during macro expansion (cached per path), and a
// requested name is checked against the stylesheet's class and id names.
private[cataclysm] object protointernal:
  private val cache: scala.collection.mutable.HashMap[Text, (Set[Text], Set[Text])] =
    scala.collection.mutable.HashMap()

  // Every `type X = …` member of a (possibly nested) refinement type, as a map.
  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   scala.collection.immutable.Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => scala.collection.immutable.Map()

  // The path of the `Styles` stylesheet in scope, read from its `Locus` type.
  private def stylesPath(using quotes: Quotes): Text =
    import quotes.reflect.*

    val styles = Expr.summon[Styles].getOrElse:
      halt(m"cataclysm: no stylesheet is in scope; add a `given Styles at \"/path\"`")

    val members = refinements(styles.asTerm.tpe) ++ refinements(styles.asTerm.tpe.widen)

    val locus = members.get(t"Locus").getOrElse:
      halt(m"cataclysm: the stylesheet carries no path")

    locus.absolve match
      case ConstantType(StringConstant(path)) =>
        path.tt

      case _ =>
        halt(m"cataclysm: the stylesheet path is not a string literal")

  private def references(path: Text)(using Quotes): (Set[Text], Set[Text]) =
    cache.getOrElseUpdate(path, parseResource(path))

  private def parseResource(path: Text)(using Quotes): (Set[Text], Set[Text]) =
    val stream = Optional(getClass.getResourceAsStream(path.s)).or:
      halt(m"cataclysm: could not read the stylesheet at $path on the classpath")

    val content = scala.io.Source.fromInputStream(stream).mkString.tt

    val css = safely(CssParser.parse(Iterator(content))).or:
      halt(m"cataclysm: could not parse the stylesheet at $path")

    // Upcast the typed names to `Text` for comparison against the requested name.
    (Set.of(css.classes.stdlib.map(identity[Text])), Set.of(css.ids.stdlib.map(identity[Text])))

  // Decide whether `name` is a class or an id in the in-scope stylesheet, or halt.
  def attributeFor[name <: Label: Type]: Macro[Text] =
    import quotes.reflect.*

    // Capture checking can wrap the instantiated `name` in `@caps.internal.inferred`
    // `AnnotatedType`s; strip annotations (and dealias) before the structural match.
    def strip(repr: TypeRepr): TypeRepr = repr.dealias.asMatchable match
      case AnnotatedType(parent, _) => strip(parent)
      case other                    => other

    val target = strip(TypeRepr.of[name]).absolve match
      case ConstantType(StringConstant(value)) =>
        value.tt

      case _ =>
        halt(m"cataclysm: the class or id name must be a string literal")

    val (classes, ids) = references(stylesPath)

    val attribute =
      if classes.stdlib.contains(target) && ids.stdlib.contains(target)
      then halt(m"cataclysm: $target is both a class and an id in the stylesheet; rename one")
      else if classes.stdlib.contains(target) then "class"
      else if ids.stdlib.contains(target) then "id"
      else halt(m"cataclysm: $target is not a class or id in the stylesheet")

    '{${Expr(attribute)}.tt}

  // Lift a `cp"…"` resource's `Locus` path into a `Styles` marker carrying it.
  def styles(resource: Expr[Resource]): Macro[Styles] =
    import quotes.reflect.*

    val members = refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)

    val locus = members.get(t"Locus").getOrElse:
      halt(m"cataclysm: the resource carries no path (it has no `Locus`)")

    Refinement(TypeRepr.of[Styles], "Locus", TypeBounds(locus, locus)).asType.absolve match
      case '[type result <: Styles; result] => '{(new Styles {}).asInstanceOf[result]}
