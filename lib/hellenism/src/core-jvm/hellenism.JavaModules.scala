                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package hellenism

import java.lang.module as jlm
import java.net as jn
import java.nio.file as jnf

import anticipation.*
import gossamer.*
import vacuous.*

// What the running Java runtime is made of: its modules, the packages each exports to every
// other module, and the classes in a package. The modules and their exports are read from the
// boot layer, and the classes from the runtime image, the `jrt:/` filesystem, where a class
// file is at `/modules/<module>/<package as a path>/<class>.class`. This is mixed into
// `Classpath.Entry.JavaRuntime`, whose members these become; Scala Native has no runtime image,
// and mixes in an empty trait of the same name.
private[hellenism] trait JavaModules:
  // The names of the runtime's modules, in alphabetical order.
  def modules: List[Text] =
    val names = scala.collection.mutable.ArrayBuffer.empty[String]
    ModuleLayer.boot().nn.modules().nn.forEach: module => names += module.nn.getName.nn
    List.from(names.sorted.iterator.map(_.tt))

  // The packages `module` exports to every module, in alphabetical order: a qualified export,
  // which names the modules it is for, is not among them. Empty if there is no such module.
  def exports(module: Text): List[Text] =
    val found = ModuleLayer.boot().nn.findModule(module.s).nn

    if !found.isPresent then Nil else
      val names = scala.collection.mutable.ArrayBuffer.empty[String]

      found.get.nn.getDescriptor.nn.exports().nn.forEach: exported =>
        val exported0: jlm.ModuleDescriptor.Exports = exported.nn
        if !exported0.isQualified then names += exported0.source.nn

      List.from(names.sorted.iterator.map(_.tt))

  // The fully-qualified names of the classes of `package` in `module`, in alphabetical order,
  // a nested class as the runtime names it (`java.util.Map$Entry`). Empty if the module has no
  // such package.
  def classes(module: Text, `package`: Text): List[Text] =
    val image = jnf.FileSystems.getFileSystem(jn.URI.create("jrt:/")).nn
    val directory = image.getPath("/modules", module.s, `package`.s.replace('.', '/').nn).nn

    if !jnf.Files.isDirectory(directory) then Nil else
      val names = scala.collection.mutable.ArrayBuffer.empty[String]
      val stream = jnf.Files.newDirectoryStream(directory, "*.class").nn

      try
        stream.forEach: file =>
          val name = file.nn.getFileName.nn.toString.dropRight(6)
          if name != "module-info" && name != "package-info" then names += name
      finally stream.close()

      List.from(names.sorted.iterator.map { name => t"${`package`}.${name.tt}" })
