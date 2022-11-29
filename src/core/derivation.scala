package cellulose

import rudiments.*
import gossamer.*
import wisteria.*

import Arity.*

import language.experimental.captureChecking

trait Codec[T]:
  def serialize(value: T): List[IArray[Node]]
  def deserialize(value: List[IArray[Node]]): T
  def schema: Schema

  protected def readField(nodes: List[IArray[Node]]): Maybe[Text] =
    nodes.headOption.maybe.mm:
      case IArray(Node(Data(value, _, _, _), _)) => value
      case _                                     => Unset

object Codec extends Derivation[Codec]:
  
  def join[T](ctx: CaseClass[Codec, T]): Codec[T] = new Codec[T]:
    def schema: Schema =
      val entries: IArray[Schema.Entry] = ctx.params.map: param =>
        Schema.Entry(param.label.show, param.typeclass.schema)

      Struct(entries.to(List), Arity.One)
    
    def serialize(value: T): List[IArray[Node]] = List:
      ctx.params.flatMap: p =>
        val children = p.typeclass.serialize(p.deref(value)).map: children =>
          val index = children.zipWithIndex.foldLeft(Map[Text, Int]()):
            case (map, (child, idx)) => child.mm(_.id).fm(map)(map.updated(_, idx))

        p.typeclass.serialize(p.deref(value)).map: value =>
          Node(Data(p.label.show, value, Layout.empty, p.typeclass.schema))
        .filter(!_.empty)

    def deserialize(value: List[IArray[Node]]): T =
      val data = Data(t"item", value.head)
      ctx.construct { param => param.typeclass.deserialize(data(param.label.show).map(_.children)) }
  
  def split[T](ctx: SealedTrait[Codec, T]): Codec[T] = ???
  
  given Codec[Byte] with
    def schema = Field(Arity.One)
    def serialize(value: Byte): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[IArray[Node]]): Byte = readField(value).option.get.s.toInt.toByte
  
  given Codec[Short] with
    def schema = Field(Arity.One)
    def serialize(value: Short): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[IArray[Node]]): Short = readField(value).option.get.s.toShort

  given Codec[Long] with
    def schema = Field(Arity.One)
    def serialize(value: Long): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[IArray[Node]]): Long = readField(value).option.get.s.toLong

  given Codec[Char] with
    def schema = Field(Arity.One)
    def serialize(value: Char): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[IArray[Node]]): Char = unsafely(readField(value).option.get(0))

  given Codec[Int] with
    def schema = Field(Arity.One)
    def serialize(value: Int): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[IArray[Node]]): Int = readField(value).option.get.s.toInt

  given Codec[Text] with
    def schema = Field(Arity.One)
    def serialize(value: Text): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[IArray[Node]]): Text = readField(value).option.get

  given Codec[Boolean] with
    def schema = Field(Arity.One)
    def serialize(value: Boolean): List[IArray[Node]] = List(IArray(Node(Data(if value then t"yes" else t"no"))))

    def deserialize(value: List[IArray[Node]]): Boolean = readField(value).option.get match
      case t"yes" => true
      case t"no"  => false
      case _      => false
  
  given maybe[T](using codec: Codec[T]): Codec[Maybe[T]] = new Codec[Maybe[T]]:
    def schema: Schema = summon[Codec[T]].schema.optional
  
    def serialize(value: Maybe[T]): List[IArray[Node]] = value match
      case Unset               => List()
      case value: T @unchecked => codec.serialize(value)
    
    def deserialize(value: List[IArray[Node]]): Maybe[T] =
      val actualValues = value.filter(!_.isEmpty)
      if actualValues.isEmpty then Unset else codec.deserialize(actualValues)
    
  given list[T](using codec: Codec[T]): Codec[List[T]] = new Codec[List[T]]:
    def schema: Schema = codec.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def serialize(value: List[T]): List[IArray[Node]] = value.map { (value: T) => codec.serialize(value).head }

    def deserialize(value: List[IArray[Node]]): List[T] =
      value.filter(!_.isEmpty).map { v => codec.deserialize(List(v)) }
  
extension [T](value: T)(using codec: Codec[T])
  def codl: Doc = Doc(IArray.from(codec.serialize(value).flatten), codec.schema, 0)