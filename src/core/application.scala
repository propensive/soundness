/*
    Exoskeleton, version 0.4.0. Copyright 2017-22 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import rudiments.*

import scala.util.*

import java.io.*

def arguments(using CliShell): List[Text] = summon[CliShell].args

trait Application:
  def main(using CliShell): Exit
  def complete(cli: Cli): Completions
  
//   final def main(argsArray: IArray[String]): Unit =
//     val args = argsArray.to(List).map(_.show)
//     val env = System.getenv().nn.asScala.toMap.map { (k, v) => Text(k) -> Text(v) }
//     val props = System.getProperties().nn.asScala.toMap.map(Text(_) -> Text(_))

//     val shell = CliShell(args, env, props)
    
//     args match
//       case t"{exoskeleton}" :: ShellType(shell) :: Int(current) :: t"--" :: args =>
//         val cli = Cli(args.head, args.tail, env, props, current - 1)
//         val completions: Completions = try complete(cli) catch Throwable => Completions(Nil)
//         shell.serialize(cli, completions).foreach(Out.println)
//         System.exit(0)
      
//       case t"{exoskeleton-generate}" :: _ =>
//         try Generate.install()(using Shell(args.tail.map(Text(_)), env, props))
//         catch case e: (InstallError | EnvError) =>
//           Out.println(t"Installation failed")
//           Exit(1)
      
//       case list =>
//         val result = try main(using shell) catch case e: Exception =>
//           e.printStackTrace()
//           Exit(2)

//         System.out.nn.flush()
//         System.err.nn.flush()
        
//         System.exit(result.status)