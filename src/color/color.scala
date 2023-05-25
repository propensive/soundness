package anticipation

trait Color[ColorType]:
  def red(color: ColorType): Int
  def green(color: ColorType): Int
  def blue(color: ColorType): Int
