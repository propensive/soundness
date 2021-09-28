/*
    Exoskeleton, version 2.0.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

// package exoskeleton

// import collection.immutable.ListMap
// import scala.util.*

// import scala.annotation.tailrec

// sealed abstract class CliException(message: String) extends Exception(message)
// case class MissingParameter(keys: ParamKey*) extends CliException(s"Argument not found")
// case class MissingSuffix() extends CliException("Suffix missing")
// case class MissingCommand() extends CliException("Command missing ")
// case class CouldNotParse(arg: Arg) extends CliException("Invalid argument")
// case class UnexpectedArg(arg: Arg) extends CliException(s"Unexpected argument: ${arg.string}")

// object Parser:
//   given Parser[String] = Some(_)
//   given Parser[Int] = Try(_).map(_.toInt).toOption
//   given Parser[Double] = Try(_).map(_.toDouble).toOption
//   given Parser[Float] = Try(_).map(_.toFloat).toOption
//   given Parser[Short] = Try(_).map(_.toShort).toOption
//   given Parser[Byte] = Try(_).map(_.toByte).toOption
//   given Parser[Char] = Try(_).filter(_.length == 1).map(_.head).toOption

// trait Parser[T]:
//   def parse(value: String): Option[T]

//   def parseList(args: Vector[Arg]): Either[CliException, T] = args match
//     case Vector()           => Left(MissingParameter())
//     case head +: Vector()   => parse(head.string).toRight(CouldNotParse(head))
//     case head +: extra +: _ => Left(UnexpectedArg(extra))

// object Show:
//   given Show[String] = identity(_)
//   given Show[Int] = _.toString
//   given Show[Double] = _.toString
//   given Show[Float] = _.toString
//   given Show[Short] = _.toString
//   given Show[Byte] = _.toString
//   given Show[Char] = _.toString

// trait Show[T]:
//   def show(value: T): String

// object Params:
//   def parse(completing: Boolean, current: Int, args: Seq[String]): Params =
//     @tailrec 
//     def parse(args: Vector[Arg], params: Params = Params()): Params = args match
//       case Vector() =>
//         params
//       case Arg(_, "--") +: t =>
//         params.copy(suffix = Some(t))
//       case h +: t if h.opt =>
//         parse(t.takeWhile(!_.opt), params.copy(opts = params.opts.updated(h, t.takeWhile(!_.opt))))
    
//     parse(args.to(Vector).zipWithIndex.map { case (v, i) => Arg(i, v) })

// sealed abstract class ParamKey(val keyString: String)
// case class ShortKey(char: Char) extends ParamKey(s"-$char")
// case class LongKey(name: String) extends ParamKey(s"--$name")

// case class Arg(index: Int, string: String):
//   def opt: Boolean = longOpt || shortOpt
//   def longOpt: Boolean = string.startsWith("--")
//   def shortOpt: Boolean = string.startsWith("-") && !string.startsWith("--")

// case class Params(prefix: Vector[Arg] = Vector(), opts: ListMap[Arg, Vector[Arg]] = ListMap(),
//                       suffix: Option[Vector[Arg]] = None):
//   def -<(extractor: Extractor[?]): ParsedParams { type Type = extractor.type } =
//     (new ParsedParams(this, Right(Map())) { type Type = extractor.type }) -< extractor

// abstract class ParsedParams(val unparsed: Params,
//                             val extracted: Either[CliException, Map[Extractor[?], Any]]) { self =>
//   type Type
//   def -<(extractor: Extractor[?]): ParsedParams { type Type = self.Type & extractor.type } =
//     extractor.extract(unparsed).map(_.asInstanceOf[(extractor.type, Params)]) match {
//       case Left(failure) =>
//         new ParsedParams(unparsed, Left(failure)) { type Type = self.Type & extractor.type }
//       case Right((value, newUnparsed)) =>
//         new ParsedParams(newUnparsed, extracted.map(_.updated(extractor, newUnparsed))) {
//           type Type = self.Type & extractor.type
//         }
//     }

//   def apply(extractor: Extractor[?])(using Type <:< extractor.type): extractor.Type =
//     extracted.right.get(extractor) match
//       case e: extractor.Type => e
// }

// trait Extractor[T]:
//   type Type = T
//   def extract(params: Params): Either[CliException, (T, Params)]

// case class Command[T: Parser]() extends Extractor[T]:
//   def extract(params: Params): Either[CliException, (T, Params)] = params.prefix.headOption.flatMap { arg =>
//     summon[Parser[T]].parse(arg.string).map((_, params.copy(prefix = params.prefix.tail)))
//   }.toRight(MissingCommand())

// case class Param[T: Parser](key: ParamKey, keys: ParamKey*) extends Extractor[T]:
//   private def keyMatches: Set[String] = (key +: keys).map(_.keyString).to(Set)
//   def extract(params: Params): Either[CliException, (T, Params)] = params.opts.find { case (key, value) =>
//     keyMatches.contains(key.string)
//   }.toRight(MissingParameter(key)).flatMap {
//     case (_, Vector(value)) =>
//       summon[Parser[T]].parse(value.string).toRight(CouldNotParse(value)).map { v =>
//         (v, params.copy(opts = params.opts.filter { case (k, _) => k.string != key.keyString }))
//       }
//     case (_, _ +: unexpected +: _) =>
//       Left(UnexpectedArg(unexpected))
//   }

// case class OptParam[T: Parser](key: ParamKey, keys: ParamKey*) extends Extractor[Option[T]]:
//   private def keyMatches: Set[String] = (key +: keys).map(_.keyString).to(Set)
//   def extract(params: Params): Either[CliException, (Option[T], Params)] =
//     (params.opts.find { case (k, _) => keyMatches.contains(k.string) }) match
//       case None          => Right((None, params))
//       case Some((k, vs)) => summon[Parser[T]].parseList(vs).map { value =>
//         (Some(value), params.copy(opts = params.opts - k))
//       }
  
// case class RepeatedParam[T: Parser](key: ParamKey, keys: ParamKey*) extends Extractor[List[T]]:
//   private def keyMatches: Set[String] = (key +: keys).map(_.keyString).to(Set)
  
//   def extract(params: Params): Either[CliException, (List[T], Params)] =
//     val (values, filtered) = params.opts.partition { (k, v) => keyMatches.contains(k.string) }
    
//     values.values.to(List).flatten.foldLeft(Right(Nil): Either[CliException, List[T]]) { (acc, next) =>
//       acc.flatMap { acc => summon[Parser[T]].parse(next.string).map(_ :: acc).toRight(CouldNotParse(next)) }
//     }.map { seq => (seq.reverse, params.copy(opts = filtered)) }

// case class Suffix[T: Parser]() extends Extractor[List[T]]:
//   def extract(params: Params): Either[CliException, (List[T], Params)] =
//     params.suffix.to(List).flatten.map { suffix =>
//       summon[Parser[T]].parse(suffix.string).toRight(CouldNotParse(suffix))
//     }.foldLeft(Right(Nil): Either[CliException, List[T]]) { (acc, next) =>
//       acc.flatMap { acc => next.map(_ :: acc) }
//     }.map { seq => (seq.reverse, params.copy(suffix = None)) }

// case object NoMoreParams extends Extractor[Unit]:
//   def extract(params: Params): Either[CliException, (Unit, Params)] = params.opts.to(List) match
//     case Nil           => Right(((), params))
//     case (arg, v) :: _ => Left(UnexpectedArg(arg))