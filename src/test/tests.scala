package exoskeleton.tests

import exoskeleton._
import guillotine.{Exit => _, _}

import environments.enclosing
import probably._

object Main extends Tests {

  def simulate(shell: Shell, keys: String): String =
    sh"etc/capture ${shell.shell} ${keys}".exec[String]



  def tests() = {
    println(simulate(Bash, "cmd f"))
    test("Complete") {
      val out = simulate(Bash, "cmd on")
      println(out)
      out
    }.assert(_ == "")
  }
}

object Counter extends Application {
  def complete(cli: Cli): Completions = {
    Completions(List(
      CompDef("one", Some("first option")),
      CompDef("two", Some("second option")),
      CompDef("three", Some("third option")),
      CompDef("four", Some("fourth option")),
      CompDef("five", Some("fifth option"))
    ))
  }

  def main(ctx: Context): Io[Exit] = Io(Exit(0))
}