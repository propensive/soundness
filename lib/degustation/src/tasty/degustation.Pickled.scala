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
package degustation

import scala.collection.immutable.Set

import anticipation.*
import gossamer.*
import murmuration.map
import vacuous.*

// The TASTy data model, as pickled: names, types, trees and modifiers exactly as the format lays
// them out (`TastyFormat`'s grammar), with no symbol table behind them. A reference to a local
// definition is the address of its definition node; a reference to anything else is a name and
// a prefix type, as the file spells it. Everything a semantic question needs — what a name
// refers to, what a type erases to, which class declares a selected member — is answered
// elsewhere, by `Universe`, over as many files as the question reaches.
//
// Shared subtrees (`SHAREDtype`, `SHAREDterm`) are resolved by the unpickler, so the model is a
// plain tree; where identity matters — the binder of a parameter reference, the recursive type a
// `RECthis` names, a definition a local reference points at — the node carries its address.
object Pickled:
  // One component of a signed name's erased signature: a fully-qualified erased type name, or
  // the length of a type-parameter section.
  enum ParamSig:
    case Erased(name: Text)
    case TypeParams(count: Int)

    def text: Text = this match
      case Erased(name)      => name
      case TypeParams(count) => count.toString.tt

  // A name from the name table, structured as the compiler's `NameKinds` structure it.
  private val Dollar: Text = "$".tt

  enum Name:
    case Simple(value: Text)
    case Qualified(prefix: Name, selector: Text)
    case Expanded(prefix: Name, selector: Text)
    case ExpandPrefix(prefix: Name, selector: Text)
    case Unique(separator: Text, number: Int, original: Optional[Name])
    case DefaultGetter(method: Name, index: Int)
    case SuperAccessor(underlying: Name)
    case InlineAccessor(underlying: Name)
    case ObjectClass(underlying: Name)
    case BodyRetainer(underlying: Name)
    case Signed(original: Name, target: Optional[Name], result: Text, params: List[ParamSig])

    // The name as the compiler's `fullName` spells it: `text`, except that a constructor's default
    // getter keeps `<init>` unmangled there (`<init>$default$1`).
    def spelled: Text = this match
      case DefaultGetter(method, index) if method.text == t"<init>" =>
        t"<init>$Dollar${"default"}$Dollar${index + 1}"

      case Signed(original, _, _, _) => original.spelled
      case other                     => other.text

    // The name without its signature, if it had one.
    def unsigned: Name = this match
      case Signed(original, _, _, _) => original
      case other                     => other

    // The erased signature a signed name carries, spelled as `(paramSigs)resultSig` — the same
    // spelling `degustation`'s member keys use — or `Unset` for an unsigned name.
    def signature: Optional[Text] = this match
      case Signed(_, _, result, params) =>
        val parameters = params.map(_.text).join(t",")
        t"($parameters)$result"

      case _ => Unset

    // The rendering the compiler's `Name#toString` gives, so that what a stack frame or a
    // classfile shows can be matched against it.
    def text: Text = this match
      case Simple(value)                  => value
      case Qualified(prefix, selector)    => t"${prefix.text}.$selector"
      case Expanded(prefix, selector)     => t"${prefix.text}$Dollar$Dollar$selector"
      case ExpandPrefix(prefix, selector) => t"${prefix.text}$Dollar$selector"

      case Unique(separator, number, original) =>
        // A fresh name with nothing to derive from, `$3$`, is the one shape the compiler renders
        // differently from `original`, separator, number.
        val fresh = if separator == Dollar then t"$Dollar$number$Dollar" else t"$separator$number"
        original.lay(fresh): name => t"${name.text}$separator$number"

      // A constructor's default getter is `$lessinit$greater$default$1` as a member name, though
      // the compiler's `fullName` spells it `<init>$default$1` (see `Universe.index`).
      case DefaultGetter(method, index) =>
        val prefix =
          if method.text == t"<init>" then t"${Dollar}lessinit${Dollar}greater" else method.text

        t"$prefix${Dollar}default$Dollar${index + 1}"

      // A prefix or suffix kind renders on the last part of a qualified name (`qualToString`):
      // `C$$inline$f`, not `inline$C$$f`.
      case SuperAccessor(underlying)  => qualified(underlying, last => t"super$Dollar$last")
      case InlineAccessor(underlying) => qualified(underlying, last => t"inline$Dollar$last")
      case ObjectClass(underlying)    => t"${underlying.text}$Dollar"

      case BodyRetainer(underlying) =>
        qualified(underlying, last => t"$last${Dollar}retainedBody")

      case Signed(original, _, _, _)  => original.text

    private def qualified(name: Name, last: Text => Text): Text = name match
      case Qualified(prefix, selector)    => t"${prefix.text}.${last(selector)}"
      case Expanded(prefix, selector)     => t"${prefix.text}$Dollar$Dollar${last(selector)}"
      case ExpandPrefix(prefix, selector) => t"${prefix.text}$Dollar${last(selector)}"
      case other                          => last(other.text)

  enum Constant:
    case UnitValue
    case FalseValue
    case TrueValue
    case NullValue
    case ByteValue(value: Byte)
    case ShortValue(value: Short)
    case CharValue(value: Char)
    case IntValue(value: Int)
    case LongValue(value: Long)
    case FloatValue(value: Float)
    case DoubleValue(value: Double)
    case StringValue(value: Text)
    case ClassValue(tpe: Type)

    // The fully-qualified class the constant's type is a reference to.
    def className: Text = this match
      case UnitValue      => t"scala.Unit"
      case FalseValue     => t"scala.Boolean"
      case TrueValue      => t"scala.Boolean"
      case NullValue      => t"scala.Null"
      case ByteValue(_)   => t"scala.Byte"
      case ShortValue(_)  => t"scala.Short"
      case CharValue(_)   => t"scala.Char"
      case IntValue(_)    => t"scala.Int"
      case LongValue(_)   => t"scala.Long"
      case FloatValue(_)  => t"scala.Float"
      case DoubleValue(_) => t"scala.Double"
      case StringValue(_) => t"java.lang.String"
      case ClassValue(_)  => t"java.lang.Class"

  enum Variance:
    case Invariant, Covariant, Contravariant

  // The parameter modifiers a methodic type carries.
  enum Clause:
    case Plain, Implicit, Contextual

  enum Type:
    // References to local definitions, by the address of the definition node.
    case TermRefDirect(address: Int)
    case TypeRefDirect(address: Int)
    case TermRefLocal(address: Int, prefix: Type)
    case TypeRefLocal(address: Int, prefix: Type)
    // References to package members, by fully-qualified name.
    case TermRefPkg(name: Text)
    case TypeRefPkg(name: Text)
    // References to non-local members, by name against a prefix; a term name may be signed.
    case TermRef(name: Name, prefix: Type)
    case TermRefIn(name: Name, prefix: Type, owner: Type)
    case TypeRef(name: Text, prefix: Type)
    case TypeRefIn(name: Text, prefix: Type, namespace: Type)
    case This(cls: Type)
    case Super(thisType: Type, underlying: Type)
    case Recursive(address: Int, parent: Type)
    case RecursiveThis(address: Int)
    case Refined(parent: Type, name: Text, info: Type)
    case Applied(constructor: Type, arguments: List[Type])
    // `high` absent means an alias of `low`.
    case Bounds(low: Type, high: Optional[Type], variances: List[Variance])
    case Annotated(underlying: Type, annotation: Tree)
    case And(left: Type, right: Type)
    case Or(left: Type, right: Type)
    case Match(bound: Type, scrutinee: Type, cases: List[Type])
    case MatchCase(pattern: Type, rhs: Type)
    case Flexible(underlying: Type)
    case ByName(underlying: Type)
    case Param(binder: Int, number: Int)
    case Method(address: Int, names: List[Text], params: List[Type], result: Type, clause: Clause)
    // Not pickled: the type of a term application whose function's type has yet to be resolved,
    // kept for the atomizer to apply once it has.
    case AppliedTerms(function: Type, arguments: List[Type])
    case Poly(address: Int, names: List[Text], bounds: List[Type], result: Type)
    case Lambda(address: Int, names: List[Text], bounds: List[Type], result: Type)
    case Literal(constant: Constant)
    case Bind(address: Int, name: Text, bounds: Type)
    // Not a pickled form: an opaque type's right-hand side as `typeOf` reads it, the alias with
    // the bounds it is seen through from outside its scope.
    case Opaque(alias: Type, bounds: Type)
    // Not a pickled form: a type lambda as a lambda type *tree* spells it, binding its parameters
    // as definitions (by address) that its body refers to as local types.
    case Abstraction(params: List[Int], names: List[Text], bounds: List[Type], result: Type)
    // Not a pickled form: a type pinned to the file it was read from, so that its local references
    // resolve there when it is carried into another file's types — a type argument substituted
    // into an alias, say.
    case Foreign(path: Text, tpe: Type)

  // A modifier flag, named as the format's tags are.
  enum Flag:
    case Private, Protected, Abstract, Final, Sealed, Case, Implicit, Given, Erased, Lazy
    case Override, Opaque, Inline, Macro, InlineProxy, Static, Object, Trait, Enum, Local
    case Synthetic, Artifact, Mutable, FieldAccessor, CaseAccessor, Covariant, Contravariant
    case HasDefault, Stable, Extension, ParamSetter, ParamAlias, Exported, Open, Invisible
    case Transparent, Infix, Tracked, Into, AbsOverride
    // Not a TASTy modifier: marks a class parameter among its class's members, as the compiler's
    // `ParamAccessor` flag does.
    case ParamAccessor

  case class Annotation(tpe: Type, tree: Tree)

  object Modifiers:
    val empty: Modifiers = Modifiers(Set(), Unset, List())

  case class Modifiers
    ( flags:         Set[Flag],
      privateWithin: Optional[Type],
      annotations:   List[Annotation] ):

    def has(flag: Flag): Boolean = flags.contains(flag)

  enum Selector:
    case Imported(name: Text)
    case Renamed(to: Text)
    case Bounded(tpe: Tree)

  object Tree:
    enum MatchKind:
      case Plain, Inline, Implicit, Sub

  enum Tree:
    case Package(path: Type, statements: List[Tree])
    case TypeDef(address: Int, name: Text, rhs: Tree, modifiers: Modifiers)
    case ValDef(address: Int, name: Name, tpt: Tree, rhs: Optional[Tree], modifiers: Modifiers)

    case DefDef
      ( address:   Int,
        name:      Name,
        clauses:   List[List[Tree]],
        returns:   Tree,
        rhs:       Optional[Tree],
        modifiers: Modifiers )

    case TypeParam(address: Int, name: Text, bounds: Tree, modifiers: Modifiers)
    case Param(address: Int, name: Name, tpt: Tree, modifiers: Modifiers)

    case Template
      ( typeParams: List[Tree],
        params:     List[Tree],
        parents:    List[Tree],
        self:       Optional[(Text, Tree)],
        body:       List[Tree] )

    case Import(qualifier: Tree, selectors: List[Selector])
    case Export(qualifier: Tree, selectors: List[Selector])

    case Ident(name: Name, tpe: Type)
    case Select(name: Name, qualifier: Tree)
    case SelectIn(name: Name, qualifier: Tree, owner: Type)
    case QualifiedThis(qualifier: Tree)
    case New(tpt: Tree)
    case Elided(tpe: Type)
    case Throw(expression: Tree)
    case NamedArg(name: Text, argument: Tree)
    case Apply(function: Tree, arguments: List[Tree])
    case ApplySigPoly(function: Tree, method: Type, arguments: List[Tree])
    case TypeApply(function: Tree, arguments: List[Tree])
    case SuperRef(qualifier: Tree, mixin: Optional[Tree])
    case Typed(expression: Tree, tpt: Tree)
    case Assign(lhs: Tree, rhs: Tree)
    case Block(statements: List[Tree], expression: Tree)
    case Inlined(expression: Tree, call: Optional[Tree], bindings: List[Tree])
    case Lambda(method: Tree, target: Optional[Tree])
    case If(inline: Boolean, condition: Tree, positive: Tree, negative: Tree)
    case Match(kind: Tree.MatchKind, scrutinee: Optional[Tree], cases: List[Tree])
    case Try(expression: Tree, cases: List[Tree], finalizer: Optional[Tree])
    case Return(method: Int, expression: Optional[Tree])
    case While(condition: Tree, body: Tree)
    case Repeated(elements: List[Tree], tpt: Tree)
    case SelectOuter(levels: Int, qualifier: Tree, underlying: Type)
    case Quote(body: Tree, tpe: Type)
    case Splice(expression: Tree, tpe: Type)
    case SplicePattern(pattern: Tree, tpe: Type, typeArguments: List[Tree], arguments: List[Tree])
    case Bind(address: Int, name: Text, tpe: Type, pattern: Tree, modifiers: Modifiers)
    case Alternative(patterns: List[Tree])
    case Unapply(function: Tree, implicits: List[Tree], tpe: Type, patterns: List[Tree])
    case QuotePattern(bindings: List[Tree], body: Tree, quotes: Tree, tpe: Type)
    case CaseDef(pattern: Tree, guard: Optional[Tree], rhs: Tree)
    case Hole(term: Boolean, index: Int, tpe: Type, arguments: List[Tree])

    // Type trees.
    case IdentTpt(name: Text, tpe: Type)
    case SelectTpt(name: Text, qualifier: Tree)
    case SingletonTpt(reference: Tree)
    case RefinedTpt(underlying: Tree, refinements: List[Tree])
    case AppliedTpt(constructor: Tree, arguments: List[Tree])
    case LambdaTpt(typeParams: List[Tree], body: Tree)
    case BoundsTpt(low: Tree, high: Optional[Tree], alias: Optional[Tree])
    case AnnotatedTpt(underlying: Tree, annotation: Tree)
    case MatchTpt(bound: Optional[Tree], scrutinee: Tree, cases: List[Tree])
    case ByNameTpt(underlying: Tree)
    case ExplicitTpt(tpt: Tree)

    // A path or constant in tree position: a term or type tree whose whole content is a type.
    case Path(tpe: Type)

    // Whether this is a type tree (one the compiler would read with `readTpt`).
    def isTypeTree: Boolean = this match
      case IdentTpt(_, _) | SelectTpt(_, _) | SingletonTpt(_) | RefinedTpt(_, _)  => true
      case AppliedTpt(_, _) | LambdaTpt(_, _) | BoundsTpt(_, _, _)                => true
      case AnnotatedTpt(_, _) | MatchTpt(_, _, _) | ByNameTpt(_) | ExplicitTpt(_) => true
      case _                                                                      => false
