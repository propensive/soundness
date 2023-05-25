package anticipation

trait RgbColor[ColorType]:
  def rgb(color: ColorType): Int
  def red(color: ColorType): Int = rgb(color)&255
  def green(color: ColorType): Int = (rgb(color) >> 8)&255
  def blue(color: ColorType): Int = (rgb(color) >> 16)&255

extension [ColorType](color: ColorType)
  def asRgb24Int(using rgbColor: RgbColor[ColorType]): Int = rgbColor.rgb(color)
