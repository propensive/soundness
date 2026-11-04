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

import soundness.*

import classloaders.threadContextClassloader
import denominative.dysasymptotics.linearSize
import soundness.classpathSubstantiable
import logging.silentLogging

trait TestService:
  def name: Text

class TestServiceA extends TestService:
  def name: Text = t"A"

class TestServiceB extends TestService:
  def name: Text = t"B"

object Outer:
  class Nested

class Enclosing:
  class Inner

object Tests extends Suite("classpath", m"Hellenism Tests"):
  def run(): Unit =
    test(m"check that a classpath file is accessible"):
      cp"/scala/Option.class"
    . assert()

    test(m"Decode a classpath"):
      unsafely:
        t"/scala/Option.class".as[Path on Classpath]
    . assert(_ == Classpath / "scala" / "Option.class")

    test(m"check that a classpath file is streamable"):
      unsafely(cp"/scala/Option.class".read[Data])
    . assert(_.length > 0)

    test(m"a classpath file on the classpath exists"):
      val path: Path on Classpath = Classpath / "scala" / "Option.class"
      path.existent()
    . assert(identity(_))

    test(m"a classpath file absent from the classpath does not exist"):
      val path: Path on Classpath = Classpath / "scala" / "Missing.class"
      path.existent()
    . assert(!_)

    test(m"check that a nonexistent classpath file is an error"):
      demilitarize(cp"/missing.txt").map(_.message)
    . assert(_ == List(t"hellenism: the path /missing.txt is not on the classpath"))

    test(m"check that an invalid classpath path is an error"):
      demilitarize(cp"foobar").map(_.message)
    . assert(_ == List(t"hellenism: the path foobar is not a valid classpath path"))

    test(m"load services from META-INF/services"):
      import systems.javaBaseSystem
      val classpath = LocalClasspath.of(Classloader[Tests.type])
      classpath.services[TestService].map(_.name).to[Set]
    . assert(_ == Set(t"A", t"B"))

    suite(m"Resource reads"):
      // The result of `Classloader#apply` retains nothing, so the read may sit inside a
      // `safely` block, as a host such as fume reads a suite index (#2071).
      test(m"read a resource's bytes inside safely"):
        safely(threadContextClassloader(t"scala/Option.class")).let(_.length)
      . assert(_.let(_ > 0) == true)

      test(m"a missing resource reads as Unset"):
        threadContextClassloader(t"missing/resource.txt")
      . assert(_ == Unset)

    suite(m"Building classloaders"):
      import systems.javaBaseSystem
      val own = Classloader[Tests.type]
      val classpath = LocalClasspath.of(own)
      val name = t"hellenism.TestServiceA"

      test(m"a preferential loader defines the class afresh"):
        val loader = classpath.classloader(Classloader.Delegation.Preferential, parent = own)
        loader.on(name).let: cls =>
          cls.getClassLoader == loader.java && cls != classOf[TestServiceA]
      . assert(_ == true)

      test(m"a deferential loader defers to its parent's class"):
        val loader = classpath.classloader(Classloader.Delegation.Deferential, parent = own)
        loader.on(name).let(_ == classOf[TestServiceA])
      . assert(_ == true)

      test(m"a built loader's parent is the chosen one"):
        classpath.classloader(Classloader.Delegation.Deferential, parent = own).parent.let(_.java)
      . assert(_ == own.java)

      test(m"the platform loader is the default parent"):
        classpath.classloader(Classloader.Delegation.Preferential).parent.let(_.java)
      . assert(_ == ClassLoader.getPlatformClassLoader.nn)

    suite(m"Class references"):
      test(m"a top-level class is referenced by its own class"):
        ClassRef[TestServiceA] == classOf[TestServiceA]
      . assert(_ == true)

      test(m"an object is referenced by its module class"):
        ClassRef[Outer.type].getName.nn.tt
      . assert(_ == t"hellenism.Outer$$")

      test(m"a class nested in an object is referenced by its binary name"):
        ClassRef[Outer.Nested].getName.nn.tt
      . assert(_ == t"hellenism.Outer$$Nested")

      test(m"a class nested in a class is referenced by its binary name"):
        ClassRef[Enclosing#Inner].getName.nn.tt
      . assert(_ == t"hellenism.Enclosing$$Inner")

      test(m"a type constructor is referenced by its binary name"):
        ClassRef[scala.Option].getName.nn.tt
      . assert(_ == t"scala.Option")

      test(m"a nested type's classloader is the one that loaded it"):
        Classloader[Outer.Nested].java
      . assert(_ == classOf[Outer.Nested].getClassLoader)

    suite(m"Entries classified by content"):
      import systems.javaBaseSystem
      import filesystemBackends.javaBaseFilesystem
      import filesystemOptions.dereferenceSymlinks
      import strategies.throwUnsafely
      import codepages.utf8Codepage

      val own: LocalClasspath = LocalClasspath.of(Classloader[Tests.type])
      val leaf: Text = Uuid().show
      val directory: Path on Linux = unsafely((% / "tmp" / leaf).on[Linux])
      unsafely(directory.create[Directory]())
      unsafely((directory / "notes.txt").write(t"not an archive"))

      test(m"A directory is a Directory entry"):
        unsafely(Classpath.Entry.classify(directory))
      . assert(_ == Classpath.Entry.Directory(directory.encode))

      test(m"A path that does not exist is a Directory entry which may yet be made"):
        unsafely(Classpath.Entry.classify(directory / "later"))
      . assert(_ == Classpath.Entry.Directory((directory / "later").encode))

      test(m"A jar is a Jar entry, by its content"):
        val jar: Optional[Text] = own.entries.seek(_.path.let(_.ends(t".jar")).or(false)).let(_.path)
        jar.let: path =>
          unsafely(Classpath.Entry.classify(path.as[Path on Linux])) == Classpath.Entry.Jar(path)
      . assert(_ == true)

      test(m"A file that is not an archive is an error"):
        import errorDiagnostics.emptyDiagnostics
        unsafely(capture[Io.Error](Classpath.Entry.classify(directory / "notes.txt")).reason)
      . assert(_ == Io.Error.Reason.InvalidData)

      test(m"An entry's path is its directory's or its jar's"):
        (Classpath.Entry.Jar(t"/x.jar").path, Classpath.Entry.Directory(t"/a/").path,
            Classpath.Entry.JavaRuntime.path)
      . assert(_ == (t"/x.jar", t"/a/", Unset))

      test(m"The running classpath names the entries this suite was loaded from"):
        val running: LocalClasspath = unsafely(LocalClasspath.running(Classloader[Tests.type]))
        own.entries.all(running.entrySet.has(_))
      . assert(_ == true)

    suite(m"Native-rendering coverage"):
      val classpath = LocalClasspath(Classpath.Entry.Jar(t"/x.jar"),
                                     Classpath.Entry.Directory(t"/a/b/"))

      test(m"hellenism's types inspect natively"):
        Inspectable.fallbacks(classpath.inspect, ClassRef(classOf[String]).inspect)
      . assert(_ == Nil)

      test(m"A classpath shows its entries, separated by colons"):
        classpath.inspect
      . assert(_ == t"classpath⟨/x.jar:/a/b/⟩")

      test(m"A class reference shows the source which produces it"):
        ClassRef(classOf[String]).inspect
      . assert(_ == t"classOf[java.lang.String]")
