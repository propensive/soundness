package cellulose

import rudiments.*
import gossamer.*
import wisteria.*

import Arity.*

import language.experimental.captureChecking

trait Codec[T]:
  def serialize(value: T): IArray[Node]
  def deserialize(value: IArray[Node]): T
  def schema: Schema

  protected def readField(nodes: IArray[Node]): Maybe[Text] = nodes match
    case IArray(Node(Data(value, _, _, _), _)) => value
    case _                                     => Unset

object Codec extends Derivation[Codec]:
  
  def join[T](ctx: CaseClass[Codec, T]): Codec[T] = new Codec[T]:
    def schema: Schema =
      val entries: IArray[Schema.Entry] = ctx.params.map: param =>
        Schema.Entry(param.label.show, param.typeclass.schema)

      Struct(entries.to(List), Arity.One)
    
    def serialize(value: T): IArray[Node] =
      ctx.params.map: p =>
        val children = p.typeclass.serialize(p.deref(value))
        
        val index = children.zipWithIndex.foldLeft(Map[Text, Int]()):
          case (map, (child, idx)) => child.mm(_.id).fm(map)(map.updated(_, idx))

        Node(Data(p.label.show, p.typeclass.serialize(p.deref(value)), Layout.empty, p.typeclass.schema))
      .filter(!_.empty)

    def deserialize(value: IArray[Node]): T =
      val dict = value.map:
        case Node(Data(key, children, _, _), _) => key -> children
        case _ => throw Mistake("Should never match")
      .to(Map)
      
      ctx.construct { param => param.typeclass.deserialize(dict.get(param.label.show).getOrElse(IArray())) }
  
  def split[T](ctx: SealedTrait[Codec, T]): Codec[T] = ???
  
  given Codec[Byte] with
    def schema = Field(Arity.One)
    def serialize(value: Byte): IArray[Node] = IArray(Node(Data(value.show)))
    def deserialize(value: IArray[Node]): Byte = readField(value).option.get.s.toInt.toByte
  
  given Codec[Short] with
    def schema = Field(Arity.One)
    def serialize(value: Short): IArray[Node] = IArray(Node(Data(value.show)))
    def deserialize(value: IArray[Node]): Short = readField(value).option.get.s.toShort

  given Codec[Long] with
    def schema = Field(Arity.One)
    def serialize(value: Long): IArray[Node] = IArray(Node(Data(value.show)))
    def deserialize(value: IArray[Node]): Long = readField(value).option.get.s.toLong

  given Codec[Char] with
    def schema = Field(Arity.One)
    def serialize(value: Char): IArray[Node] = IArray(Node(Data(value.show)))
    def deserialize(value: IArray[Node]): Char = unsafely(readField(value).option.get(0))

  given Codec[Int] with
    def schema = Field(Arity.One)
    def serialize(value: Int): IArray[Node] = IArray(Node(Data(value.show)))
    def deserialize(value: IArray[Node]): Int = readField(value).option.get.s.toInt

  given Codec[Text] with
    def schema = Field(Arity.One)
    def serialize(value: Text): IArray[Node] = IArray(Node(Data(value.show)))
    def deserialize(value: IArray[Node]): Text = readField(value).option.get

  given Codec[Boolean] with
    def schema = Field(Arity.One)
    def serialize(value: Boolean): IArray[Node] = IArray(Node(Data(if value then t"yes" else t"no")))

    def deserialize(value: IArray[Node]): Boolean = value match
      case IArray(Node(Data(t"yes", _, _, _), _)) => true
      case IArray(Node(Data(t"no", _, _, _), _))  => false
      case _                                         => false
  
  given maybe[T](using codec: Codec[T]): Codec[Maybe[T]] = new Codec[Maybe[T]]:
    def schema: Schema = summon[Codec[T]].schema.optional
  
    def serialize(value: Maybe[T]): IArray[Node] = value match
      case Unset               => IArray()
      case value: T @unchecked => codec.serialize(value)
    
    def deserialize(value: IArray[Node]): Maybe[T] =
      if value.isEmpty then Unset else codec.deserialize(value)
    
  given list[T](using codec: Codec[T]): Codec[List[T]] = new Codec[List[T]]:
    def schema: Schema = codec.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => Struct(List(Schema.Entry(t"item", struct)), Arity.Many)

    def serialize(value: List[T]): IArray[Node] = codec.schema match
      case Field(_, _)  => IArray.from(value.flatMap(codec.serialize))
      case Struct(_, _) => IArray.from(value.map { item => Node(Data(t"item", codec.serialize(item))) })

    def deserialize(value: IArray[Node]): List[T] = schema match
      case Field(_, _) => value.to(List).map:
        case node@Node(Data(key, _, _, _), _) => codec.deserialize(IArray(node))
        case _ => throw Mistake("Should never match")
      case Struct(_, _) => value.to(List).map:
        case Node(Data(_, children, _, _), _) => codec.deserialize(children)
        case _ => throw Mistake("Should never match")
  
extension [T](value: T)(using codec: Codec[T])
  def codl: Doc = Doc(codec.serialize(value), codec.schema, 0)