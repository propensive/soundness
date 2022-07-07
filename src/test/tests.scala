package imperial

import probably.*
import gossamer.*
import turbulence.*
import rudiments.*
import stdouts.stdout
import anticipation.integration.javaIo
import eucalyptus.*

given Log(Everything |-> SystemOut)

given Environment({
  case t"HOME" => Some(t"/home/work")
  case _       => None
})

object Tests extends Suite(t"Imperial tests"):
  def run(using Runner): Unit =
    println("Hello")
    test(t"Home directory"):
      Home().getAbsolutePath
    .assert(_ == t"/home/work")

    test(t"Cache directory"):
      Home.Cache().getAbsolutePath
    .assert(_ == t"/home/work/.cache")
    
    test(t"~/.local/bin path"):
      Home.Local.Bin().getAbsolutePath
    .assert(_ == t"/home/work/.local/bin")
