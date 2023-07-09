package anticipation

object Add:
  given Add[Int, Int] with
    type Result = Int
    def apply(left: Int, right: Int, subtract: Boolean): Int =
      if subtract then left - right else left + right
  
  given Add[Byte, Byte] with
    type Result = Int
    def apply(left: Byte, right: Byte, subtract: Boolean): Int =
      if subtract then left - right else left + right
  
  given Add[Double, Double] with
    type Result = Double
    def apply(left: Double, right: Double, subtract: Boolean): Double =
      if subtract then left - right else left + right
  
  given Add[Float, Float] with
    type Result = Float
    def apply(left: Float, right: Float, subtract: Boolean): Float =
      if subtract then left - right else left + right
  
  given Add[Short, Short] with
    type Result = Int
    def apply(left: Short, right: Short, subtract: Boolean): Int =
      if subtract then left - right else left + right
  
  given Add[Long, Long] with
    type Result = Long
    def apply(left: Long, right: Long, subtract: Boolean): Long =
      if subtract then left - right else left + right

trait Add[-LeftType, -RightType]:
  type Result
  def apply(left: LeftType, right: RightType, subtract: Boolean): Result

object Multiply:
  given Multiply[Int, Int] with
    type Result = Int
    def apply(left: Int, right: Int): Int = left*right
  
  given Multiply[Byte, Byte] with
    type Result = Int
    def apply(left: Byte, right: Byte): Int = left*right
  
  given Multiply[Double, Double] with
    type Result = Double
    def apply(left: Double, right: Double): Double = left*right
  
  given Multiply[Float, Float] with
    type Result = Float
    def apply(left: Float, right: Float): Float = left*right
  
  given Multiply[Short, Short] with
    type Result = Int
    def apply(left: Short, right: Short): Int = left*right
  
  given Multiply[Long, Long] with
    type Result = Long
    def apply(left: Long, right: Long): Long = left*right

trait Multiply[-LeftType, -RightType]:
  type Result
  def apply(left: LeftType, right: RightType): Result
