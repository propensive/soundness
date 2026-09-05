// The tutorial's tests stand alone rather than inside a Suite's run(): a suite for them, and a
// runner whose report is never rendered, so an assertion can run without a test harness.
import soundness.*
import stdios.javaLangSystemStdio
import environments.javaBaseEnvironment

given Testable = Testable(m"tutorial")

given TestPalette = new TestPalette:
  def background: Color in Srgb = Srgb(0, 0, 0)
  def foreground: Color in Srgb = Srgb(1, 1, 1)
  def unaccented: Color in Srgb = Srgb(1, 1, 1)
  def informative: Color in Srgb = Srgb(1, 1, 1)
  def subdued: Color in Srgb = Srgb(1, 1, 1)
  def positive: Color in Srgb = Srgb(1, 1, 1)
  def negative: Color in Srgb = Srgb(1, 1, 1)
  def warning: Color in Srgb = Srgb(1, 1, 1)
  def critical: Color in Srgb = Srgb(1, 1, 1)
  def benchmark: Color in Srgb = Srgb(1, 1, 1)
  def mixed: Color in Srgb = Srgb(1, 1, 1)
  def cold: Color in Srgb = Srgb(1, 1, 1)
  def warm: Color in Srgb = Srgb(1, 1, 1)
  def hot: Color in Srgb = Srgb(1, 1, 1)
  def accented: Color in Srgb = Srgb(1, 1, 1)
  def highlight: Color in Srgb = Srgb(1, 1, 1)
  def detail: Color in Srgb = Srgb(1, 1, 1)
  def pass: Color in Srgb = Srgb(1, 1, 1)
  def fail: Color in Srgb = Srgb(1, 1, 1)
  def aspirePass: Color in Srgb = Srgb(1, 1, 1)
  def aspireFail: Color in Srgb = Srgb(1, 1, 1)

given Runner[Report] = Runner()
