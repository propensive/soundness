/*
    Exoskeleton, version 2.0.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import collection.JavaConverters.*

import scala.util.*

import java.io.*

def arguments(using Shell): IArray[String] = summon[Shell].args

trait Application:
  def main(using Shell): Exit
  def complete(cli: Cli): Completions
  
  final def main(args: IArray[String]): Unit =
    val argList = args.to(List)
    val env = System.getenv().nn.asScala.toMap
    val props = System.getProperties().nn.asScala.toMap

    val shell = Shell(args, env, props)
    
    argList match
      case "{exoskeleton}" :: ShellType(shell) :: AsInt(current) :: "--" :: args =>
        val cli = Cli(args.head, args.tail, env, props, current - 1)
        val completions: Completions = try complete(cli) catch Throwable => Completions(Nil)
        shell.serialize(cli, completions).foreach(println)
        System.exit(0)
      case "{exoskeleton-generate}" :: _ =>
        try Generate.install()(using Shell(args.tail, env, props))
        catch case InstallError() | EnvError(_) =>
          println("Installation failed")
          Exit(1)
      case list =>
        val result = try main(using shell) catch case e: Exception =>
          e.printStackTrace()
          Exit(2)

        System.out.nn.flush()
        System.err.nn.flush()
        
        System.exit(result.status)

object Counter extends Application:
  def complete(cli: Cli): Completions =
    Completions(List(
      Choice("one", "first option"),
      Choice("two", s"second option"),
      Choice("three", "third option"),
      Choice("four", "fourth option"),
      Choice("five", "fifth option")
    ), title = "Here are the choices:")

  def main(using Shell): Exit =
    println("Do nothing")
    println(System.getenv("FPATH"))
    Exit(0)