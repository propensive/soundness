/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Exoskeleton, version 1.0.0. Copyright 2019-20 Jon Pretty, Propensive OÜ.                                  ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package exoskeleton

import collection.JavaConverters._

import scala.util._

import java.io._

trait Application {
  def main(context: Context): Io[Exit]
  def complete(cli: Cli): Completions
  
  final def main(args: Array[String]): Unit = {
    val argList = args.to[List]
    val env = System.getenv().asScala.toMap
    val props = System.getProperties().asScala.toMap

    val context = Context(argList, env, props)
    
    val result = argList match {
      case "{completion}" :: Shell(shell) :: AsInt(current) :: "--" :: args =>
        Log.info("----------")
        Log.info(s"   shell: ${shell.shell}")
        Log.info(s"    args: ${args.mkString("[\"", "\", \"", "\"]")}")
        Log.info(s" current: $current")
        val cli = Cli(args.head, args.tail, env, props, current - 1)
        shell.serialize(cli, complete(cli)).foreach(println)
        Io.stdout.flush()
        Io.stderr.flush()
        System.exit(0)
      case "{completion-generate}" :: args =>
        Generate.install(Context(args, env, props))
      case list =>
        val result = main(context).exec()
        Io.stdout.flush()
        Io.stderr.flush()
        result match {
          case Failure(err) =>

          case Success(Exit(n)) =>
            System.exit(0)
        }
    }
  }
}

object Io {

  private def capture(stream: PrintStream, replace: PrintStream => Unit): PrintStream = {
    replace(new PrintStream({ _ => () }))
    stream
  }

  private[exoskeleton] val stdout: PrintStream = capture(System.out, System.setOut)
  private[exoskeleton] val stderr: PrintStream = capture(System.err, System.setErr)

  def apply[T](blk: => T): Io[T] = () => Try(blk)
  def pure[T](blk: T): Io[T] = () => Success(blk)
  def unit: Io[Unit] = () => Success(())

  object out {
    def print(string: String): Io[Unit] = Io(stdout.println(string))
    def println(string: String): Io[Unit] = Io(stdout.println(string))
  }
  
  object err {
    def print(string: String): Io[Unit] = Io(stderr.println(string))
    def println(string: String): Io[Unit] = Io(stderr.println(string))
  }

  def from[T](value: => Try[T]): Io[T] = new Io[T] { private[exoskeleton] def exec(): Try[T] = value }
}

abstract class Io[+T]() { io =>
  private[exoskeleton] def exec(): Try[T]
  
  def flatMap[S](fn: T => Io[S]): Io[S] = () => io.exec().flatMap(fn(_).exec())
  def map[S](fn: T => S): Io[S] = () => io.exec().map(fn)
}