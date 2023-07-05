package rudiments

object Inequality:
  inline given numeric: Inequality[Boolean, Int | Double | Char | Byte | Short | Float | Long] with
    inline def compare
        (inline left: Boolean, inline right: Int | Double | Char | Byte | Short | Float | Long,
            inline strict: Boolean)
        : Boolean =
      ${Rudiments.inequality('left, 'right, 'strict)}

trait Inequality[-LeftType, -RightType]:
  inline def compare(inline left: LeftType, inline right: RightType, inline strict: Boolean): Boolean

extension [LeftType](inline left: LeftType)
  @targetName("lt")
  inline def <
      [RightType]
      (right: RightType)(using inline inequality: Inequality[LeftType, RightType])
      : Boolean =
    inequality.compare(left, right, true)
  
  @targetName("lte")
  inline def <=
      [RightType]
      (right: RightType)(using inline inequality: Inequality[LeftType, RightType])
      : Boolean =
    inequality.compare(left, right, false)
  