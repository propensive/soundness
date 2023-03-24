/*
    Gossamer, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import rudiments.*
import turbulence.*

import scala.deriving.*
import scala.compiletime.*

import java.text as jt

import language.experimental.pureFunctions


trait Show[-T]:
  def show(value: T): Text

trait Debug[-T]:
  def show(value: T): Text

case class SimpleTExtractor(text: Text):
  def unapply(scrutinee: Text): Boolean = text == scrutinee

extension [T](value: T)
  def show(using show: Show[T]): Text = show.show(value)
  def debug(using debug: Debug[T]): Text = debug.show(value)
  def canon(using canonical: Canonical[T]): Text = canonical.serialize(value)

  inline def txt: Text = compiletime.summonFrom:
    case show: Show[T]      => show.show(value)
    case debug: Debug[T]    => debug.show(value)
  
  inline def report: Text = compiletime.summonFrom:
    case debug: Debug[T]    => debug.show(value)
    case show: Show[T]      => show.show(value)
    case _                  => Text(value.toString)

extension (inline ctx: StringContext)
  transparent inline def txt(inline parts: Any*): Text =
    ${Interpolation.Text.expand('{Interpolation.Text}, 'ctx, 'parts)}
 
  transparent inline def t(inline parts: Any*): Text =
    ${Interpolation.T.expand('{Interpolation.T}, 'ctx, 'parts)}

extension (ctx: StringContext)
  def t = SimpleTExtractor(ctx.parts.head.show)

object Debug:
  object any extends Debug[Any]:
    def show(value: Any): Text = Text(value.toString)

  given text: Debug[Text] = text =>
    val escaped = text.flatMap:
      case '\n'     => t"\\n"
      case '\u001b' => t"\\e"
      case '\t'     => t"\\t"
      case '\r'     => t"\\r"
      case '\\'     => t"\\\\"
      case '\"'     => t"\\\""
      case '\''     => t"\\\'"
      case '\b'     => t"\\b"
      case '\f'     => t"\\f"
      case ch       => if ch < 128 && ch >= 32 then ch.show
                       else String.format("\\u%04x", ch.toInt).nn.show

    t"t\"$escaped\""

  given [T: Debug]: Debug[Option[T]] =
    case None        => t"None"
    case Some(value) => t"Some(${value.debug})"

  given long: Debug[Long] = long => Text(s"${long}L")
  given Debug[Int] = int => Text(s"${int}")
  given Debug[Short] = short => Text(s"${short}.toShort")
  given Debug[Byte] = byte => Text(s"${byte}.toByte")
  given Debug[Char] = char => Text(s"'${char.show.debug.drop(2).drop(1, Rtl)}'")

  given Debug[Double] = double =>
    if double != double then t"Double.NaN"
    else if java.lang.Double.isInfinite(double)
    then t"Double.${if double < 0.0 then t"Negative" else t"Positive"}Infinity"
    else Showable(double).show
  
  given Debug[Float] = float =>
    if float != float then t"Float.NaN"
    else if java.lang.Float.isInfinite(float)
    then t"Float.${if float < 0.0f then t"Negative" else t"Positive"}Infinity"
    else t"${Showable(float).show}F"

  given Debug[Boolean] =
    case true  => t"true"
    case false => t"false"
  
  given Debug[ByteSize] = bs =>
    if bs.long > 10L*1024*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024*1024).show}.tb")
    else if bs.long > 10L*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024).show}.gb")
    else if bs.long > 10*1024*1024 then Text(s"${bs.long/1024*1024}.mb")
    else if bs.long > 10*1024 then Text(s"${bs.long/1024}.kb")
    else Text(s"${bs.long}.b")

  given [T: Debug]: Debug[Set[T]] = _.map(_.debug).join(t"Set(", t", ", t")")
  given [T: Debug]: Debug[List[T]] = _.map(_.debug).join(t"List(", t", ", t")")
  given [T: Debug]: Debug[Vector[T]] = _.map(_.debug).join(t"Vector(", t", ", t")")
  given [T: Debug]: Debug[Array[T]] = _.map(_.debug).join(t"Array(", t", ", t")")
  given [T: Debug]: Debug[IArray[T]] = _.map(_.debug).join(t"IArray(", t", ", t")")


  private transparent inline def deriveProduct[Labels <: Tuple](tuple: Tuple): List[Text] =
    inline tuple match
      case EmptyTuple => Nil
      case cons: (? *: ?) => cons match
        case head *: tail => inline erasedValue[Labels] match
          case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
            case label: String =>
              val value = summonInline[Debug[head.type]].show(head)
              Text(label+" = "+value) :: deriveProduct[tailLabels](tail)

  inline given derived[P](using mirror: Mirror.Of[P]): Debug[P] = inline mirror match
    case given Mirror.ProductOf[P & Product] => (value: P) => value.asMatchable match
      case value: Product =>
        val elements = deriveProduct[mirror.MirroredElemLabels](Tuple.fromProductTyped(value))
        val typeName = Text(valueOf[mirror.MirroredLabel])
        typeName+elements.join(Text("("), Text(", "), Text(")"))
    
    case given Mirror.SumOf[P] =>
      (value: P) =>
        ???

object Show:
  given Show[String] = Text(_)
  given Show[Text] = identity(_)
  given Show[Int] = Showable(_).show
  given Show[Short] = Showable(_).show
  given Show[Long] = Showable(_).show
  given Show[Byte] = Showable(_).show
  given Show[Uuid] = _.javaUuid.toString.show

  given (using format: DecimalFormat): Show[Double] = format.render(_)
  
  given Show[ByteSize] = bs =>
    if bs.long > 10L*1024*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024*1024).show}TB")
    else if bs.long > 10L*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024).show}GB")
    else if bs.long > 10*1024*1024 then Text(s"${bs.long/1024*1024}MB")
    else if bs.long > 10*1024 then Text(s"${bs.long/1024}kB")
    else Text(s"${bs.long}B")
  
  given Show[Char] = Showable(_).show
  given Show[Boolean] = if _ then Text("true") else Text("false")
  given Show[reflect.Enum] = Showable(_).show

  given Show[Pid] = pid => t"ᴾᴵᴰ${pid.value}"

  given [T: Show]: Show[Set[T]] = _.map(_.show).join(t"⦃", t", ", t"⦄")
  given [T: Show]: Show[List[T]] = _.map(_.show).join(t"⟦", t", ", t"⟧")
  given [T: Show]: Show[Vector[T]] = _.map(_.show).join(t"⟪", t", ", t"⟫")
  given [T: Show]: Show[Array[T]] = _.map(_.show).join(t"⌊", t", ", t"⌋ₘ")
  given [T: Show]: Show[IArray[T]] = _.map(_.show).join(t"⌊", t", ", t"⌋ᵢ")
  
  given [T: Show]: Show[LazyList[T]] with
    private def recur(value: LazyList[T]): Text =
      if value.toString() == "LazyList(<not computed>)" then t"〜"
      else if value.isEmpty then t"¶"
      else t"${value.head} ⌗ ${recur(value.tail)}"

    def show(value: LazyList[T]): Text = recur(value.take(3))
  
  given [T: Show]: Show[Option[T]] =
    case None    => Text("none")
    case Some(v) => v.show

object Canonical:
  def apply[T](read: Text -> T, write: T -> Text): Canonical[T] = new Canonical[T]:
    def serialize(value: T): Text = write(value)
    def deserialize(value: Text): T = read(value)

  given (using CanThrow[IncompatibleTypeError]): Canonical[Byte] =
    Canonical[Byte](_.as[Byte], _.show)
  
  given (using CanThrow[IncompatibleTypeError]): Canonical[Short] =
    Canonical[Short](_.as[Short], _.show)
  
  given (using CanThrow[IncompatibleTypeError]): Canonical[Int] =
    Canonical[Int](_.as[Int], _.show)
  
  given (using CanThrow[IncompatibleTypeError]): Canonical[Long] =
    Canonical[Long](_.as[Long], _.show)

trait Canonical[T]:
  def serialize(value: T): Text
  def deserialize(value: Text): T

extension (inline ctx: StringContext)
  transparent inline def enc(inline parts: Any*): Encoding =
    ${EncodingPrefix.expand('EncodingPrefix, 'ctx, 'parts)}

package decimalFormats:
  given exact: DecimalFormat = DecimalFormat(t"0.#################")
  given onePlace: DecimalFormat = DecimalFormat(t"0.#")
  given twoPlaces: DecimalFormat = DecimalFormat(t"0.##")
  given threePlaces: DecimalFormat = DecimalFormat(t"0.###")
  given fourPlaces: DecimalFormat = DecimalFormat(t"0.####")

case class DecimalFormat(pattern: Text):
  private lazy val decimalFormat: jt.DecimalFormat = jt.DecimalFormat(pattern.s)
  def render(value: Double): Text = Text(decimalFormat.format(value).nn)
