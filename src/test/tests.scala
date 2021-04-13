package iridesce

import probably.*

object Main extends Suite("Iridesce tests"):
  
  given Profile = Profile.Daylight
  given Tolerance[Double] = (a, b) => math.abs(a - b) < 0.1
  
  def run(using Runner): Unit =
    for color <- Colors.all do
      test("sRGB to L*a*b*") {
        color.debug
        color.cielab.srgb.debug
      }.assert(_ ~~ color)

      test("HSV to sRGB and back") {
        color.hsv.debug
        color.hsv.srgb.hsv.debug
      }.assert(_ ~~ color.hsv)
      
      test("sRGB to CMY and back") {
        color.debug
        color.cmy.srgb.debug
      }.assert(_ ~~ color)
      
      test("sRGB to CMYK and back") {
        color.debug
        color.cmyk.srgb.debug
      }.assert(_ ~~ color)
      
      test("sRGB to XYZ and back") {
        color.debug
        color.xyz.srgb.debug
      }.assert(_ ~~ color)
      
      test("sRGB to HSL and back") {
        color.debug
        color.xyz.srgb.debug
      }.assert(_ ~~ color)