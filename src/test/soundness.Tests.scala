package soundness

object Tests extends Suite(m"Soundness tests"):
  def run(): Unit =
    rudiments.Tests()
    kaleidoscope.Tests()
