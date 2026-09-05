package wisteria

import soundness.*
import dysasymptotics.linearAccess
import strategies.throwUnsafely

// A compile-time mirror of doc/modules/derivation.md's samples (the ultimatum_doccheck.scala
// precedent): a documented derivation that does not compile is worse than no example at all.
object DerivationTutorial:
  sealed trait Temporal

  enum Month:
    case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

  case class Date(day: Int, month: Month, year: Int) extends Temporal
  case class Time(hour: Int, minute: Int)
  case class DateTime(date: Date, time: Time) extends Temporal

  trait Presenting[value]:                 // consumer
    def present(value: value): Text

  trait Producing[+value]:                 // producer
    def produce(): value

  trait Presentation[value]:
    def present(value: value): Text

  object Presentation extends Derivation[Presentation]:
    given Presentation[Text] = identity(_)
    given Presentation[Int] = _.toString.tt

    inline def conjunction[derivation <: Product: ProductReflection]: Presentation[derivation] =
      value =>
        fields(value):
          [field] => field => t"$label=${contextual.present(field)}"
        . join(t"${typeName[derivation]}(", t", ", t")")

    inline def disjunction[derivation: SumReflection]: Presentation[derivation] =
      value =>
        variant(value):
          [variant <: derivation] => variant => contextual.present(variant)

  extension [value](value: value)
    def present(using presentation: Presentation[value]): Text = presentation.present(value)

  case class Person(name: Text, age: Int) derives Presentation

  Person(t"Ada", 36).present   // t"Person(name=Ada, age=36)"

  trait Labels[value]:
    def labels: List[Text]

  object Labels extends ProductDerivation[Labels]:
    given Labels[Text] = new Labels[Text] { def labels: List[Text] = Nil }
    given Labels[Int] = new Labels[Int] { def labels: List[Text] = Nil }

    inline def conjunction[derivation <: Product: ProductReflection]: Labels[derivation] =
      val fieldLabels = contexts[derivation]() { [field] => context => label }
      new Labels[derivation]:
        def labels: List[Text] = fieldLabels.to[List]

  case class Empty()

  Labels.derived[Person].labels    // List(t"name", t"age")
  Labels.derived[Empty].labels     // List()

  trait Parsing[value]:
    def parse(text: Text): value

  object Parsing extends ProductDerivation[Parsing]:
    given Parsing[Text] = identity(_)
    given Parsing[Int] = _.s.toInt

    inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
      text =>
        val columns = text.cut(t",")
        build[derivation]:
          [field] => parsing => parsing.parse(columns(Ordinal.zerary(index)).or(t""))

  Parsing.derived[Person].parse(t"Ada,36")   // Person(t"Ada", 36)

  object ParsingSums extends Derivation[Parsing]:
    inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
      Parsing.conjunction[derivation]

    inline def disjunction[derivation: SumReflection]: Parsing[derivation] =
      text =>
        text.cut(t":") match
          case List(prefix, rest) =>
            delegate[derivation](prefix):
              [variant <: derivation] => parsing => parsing.parse(rest)

  trait Equivalence[value]:
    def equal(left: value, right: value): Boolean

  object Equivalence extends Derivation[Equivalence]:
    given Equivalence[Int] = _ == _
    given Equivalence[Text] = _.lower == _.lower

    inline def conjunction[derivation <: Product: ProductReflection]: Equivalence[derivation] =
      (left, right) =>
        contexts[derivation]():
          [field] => equivalence =>
            val extract: derivation => field = dereference
            equivalence.equal(extract(left), extract(right))
        . all { boolean => boolean }

    inline def disjunction[derivation: SumReflection]: Equivalence[derivation] =
      (left, right) =>
        variant(left):
          [variant <: derivation] => leftValue =>
            complement(right).lay(false)(contextual.equal(leftValue, _))

  Equivalence.derived[Person].equal(Person(t"Ada", 36), Person(t"ADA", 36))   // true

  trait Naming[value]:
    def name(value: value): Text

  object Naming extends Derivation[Naming]:
    inline def conjunction[derivation <: Product: ProductReflection]: Naming[derivation] =
      value => typeName[derivation]

    inline def disjunction[derivation: SumReflection]: Naming[derivation] = value =>
      inline if choice[derivation] then
        variant(value):
          [variant <: derivation] => arm => t"${typeName[derivation]}.${contextual.name(arm)}"
      else scala.compiletime.error("cannot derive Naming for a sum whose variants carry data")

  Naming.derived[Month].name(Month.Mar)   // t"Month.Mar"

  object LenientParsing extends ProductDerivation[Parsing]:
    inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
      text =>
        val columns = text.cut(t",")
        build[derivation]:
          [field] => parsing =>
            columns(Ordinal.zerary(index)).let(parsing.parse(_))
            . or(default.or(panic(m"no column and no default for $label")))

  case class Settings(name: Text, retries: Int = 3)

  LenientParsing.derived[Settings].parse(t"primary")   // Settings(t"primary", 3)

  import arithmetic.addable

  case class Pair(label: Text, count: Int)

  Pair(t"foo", 10) + Pair(t"bar", 15)   // Pair(t"foobar", 25)

  enum Tree derives Presentation:
    case Leaf
    case Branch(left: Tree, value: Int, right: Tree)

  Tree.Branch(Tree.Leaf, 1, Tree.Leaf).present   // t"Branch(left=Leaf, value=1, right=Leaf)"

  trait Rendering[value]:
    def render(value: value): Text

  object Rendering:
    inline given derived[Value]: Rendering[Value] = item =>
      scala.compiletime.summonFrom:
        case presentation: Presentation[Value] => presentation.present(item)
        case given (Value is Showable)         => item.show
        case _                                 => item.toString.tt

  extension [value](value: value)
    def render(using rendering: Rendering[value]): Text = rendering.render(value)

  Person(t"Ada", 36).render   // through Presentation, which Person derives
  3.14.render                 // through Showable, which Double has
