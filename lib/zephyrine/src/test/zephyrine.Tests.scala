package zephyrine

import soundness.*

import randomization.unseeded

object Tests extends Suite(t"Zephyrine tests"):
  val bytes = Bytes.fill(1000)(_.toByte)
  def run(): Unit =
    for i <- 1 to 100 do
      val stream = Stream(bytes).shred(1.0, 1.0)
      println(stream.toList.length)
      test(t"check the stream"):
        ()
      .assert()
