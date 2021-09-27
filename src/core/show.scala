package gossamer

import wisteria.*

trait Show[-T]:
  def show(value: T): Txt

object Show extends Derivation[Show]:
  given Show[String] = Txt(_)
  given Show[Int] = num => Txt(num.toString)
  given Show[Short] = num => Txt(num.toString)
  given Show[Long] = num => Txt(num.toString)
  given Show[Byte] = num => Txt(num.toString)
  given Show[Char] = ch => Txt(ch.toString)
  given Show[Boolean] = if _ then Txt("true") else Txt("false")

  def join[T](ctx: CaseClass[Show, T]): Show[T] = value =>
    if ctx.isObject then ctx.typeInfo.short.text
    else ctx.params.map {
      param => param.typeclass.show(param.deref(value)).s
    }.join(str"${ctx.typeInfo.short}(", ", ", ")").text
  
  def split[T](ctx: SealedTrait[Show, T]): Show[T] = value =>
    ctx.choose(value) { subtype => subtype.typeclass.show(subtype.cast(value)) }

extension [T](value: T) def show(using show: Show[T]) = show.show(value)