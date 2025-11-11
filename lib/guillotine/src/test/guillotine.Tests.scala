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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package guillotine

import ambience.*, systems.virtualMachine
import anticipation.*
import contextual.*
import contingency.*, strategies.throwUnsafely
import eucalyptus.*
import fulminate.*
import galilei.*
import filesystemOptions.createNonexistent.disabled
import filesystemOptions.dereferenceSymlinks.enabled
import gossamer.*
import probably.*
import proscenium.*
import rudiments.*, workingDirectories.virtualMachine
import serpentine.*, pathHierarchies.unix
import spectacular.*
import turbulence.*

import unsafeExceptions.canThrowAny

given SimpleLogger = logging.silent

object Tests extends Suite(m"Guillotine tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      test(m"parse simple command"):
        sh"ls -la"
      .assert(_ == Command(t"ls", t"-la"))

      test(m"parse a substitution"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls $flags"
      .assert(_ == Command(t"ls", t"-la"))

      test(m"parse two substitutions"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls $flags $file"
      .assert(_ == Command(t"ls", t"-la", t"filename"))

      test(m"parse irregular spacing"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls  $flags     $file"
      .assert(_ == Command(t"ls", t"-la", t"filename"))

      test(m"parse irregular spacing 2"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls  $flags $file"
      .assert(_ == Command(t"ls", t"-la", t"filename"))

      test(m"adjacent substitutions"):
        val a = t"a"
        val b = t"b"
        sh"ls $a$b"
      .assert(_ == Command(t"ls", t"ab"))

      test(m"substitute a list"):
        val a = List(t"a", t"b")
        sh"ls $a"
      .assert(_ == Command(t"ls", t"a", t"b"))

      test(m"substitute a single-quoted list"):
        val a = List(t"a", t"b")
        sh"ls '$a'"
      .assert(_ == Command(t"ls", t"a b"))

      test(m"substitute in a double-quoted list"):
        val a = List(t"a", t"b")
        sh"""ls "$a""""
      .assert(_ == Command(t"ls", t"a b"))

      test(m"insertion after arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}x"""
      .assert(_ == Command(t"ls", t"a", t"bx"))

      test(m"insertion before arg"):
        val a = List(t"a", t"b")
        sh"""ls x${a}"""
      .assert(_ == Command(t"ls", t"xa", t"b"))

      test(m"insertion before quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}'x'"""
      .assert(_ == Command(t"ls", t"a", t"bx"))

      test(m"insertion after quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls 'x'${a}"""
      .assert(_ == Command(t"ls", t"xa", t"b"))

      test(m"empty list insertion"):
        val a = List()
        sh"""ls ${a}"""
      .assert(_ == Command(t"ls"))

      test(m"empty list insertion"):
        val a = List()
        sh"""ls '${a}'"""
      .assert(_ == Command(t"ls", t""))

      test(m"empty parameters"):
        sh"""ls '' ''"""
      .assert(_ == Command(t"ls", t"", t""))

      test(m"one empty parameter, specified twice"):
        sh"""ls ''''"""
      .assert(_ == Command(t"ls", t""))

      test(m"single quote inside double quotes"):
        sh"""ls "'" """
      .assert(_ == Command(t"ls", t"'"))

      test(m"double quote inside single quotes"):
        sh"""ls '"' """
      .assert(_ == Command(t"ls", t"\""))

      test(m"escaped double quote"):
        sh"""ls \" """
      .assert(_ == Command(t"ls", t"\""))

      test(m"escaped single quote"):
        sh"""ls \' """
      .assert(_ == Command(t"ls", t"'"))

    suite(m"rendering Debug")
      test(m"simple command"):
        sh"echo Hello World".inspect
      .check(_ == t"""sh"echo Hello World"""")

      println(sh"echo 'Hello World'".inspect)
      test(m"simple command with space"):
        sh"echo 'Hello World'".inspect
      .check(_ == t"""sh"echo 'Hello World'"""")

      test(m"simple command with quote and space"):
        Command(t"echo", t"Don't stop").inspect
      .check(_ == t"sh\"\"\"echo \"Don't stop\"\"\"\"")

      test(m"simple command with single and double quote"):
        Command(t"echo", t"single ' and double \" quotes").inspect
      .check(_ == t"sh\"\"\"echo \"single ' and double \\\" quotes\"\"\"\"")

      test(m"render pipeline of commands"):
        (sh"echo Hello" | sh"sed s/e/a/g").inspect
      .check(_ == t"""sh"echo Hello" | sh"sed s/e/a/g"""")

    suite(m"equality tests"):
      test(m"check that two commands written differently are equivalent"):
        sh"echo 'hello world'"
      .assert(_ == sh"""echo "hello world"""")

      test(m"check that two commands written with different whitespace are equivalent"):
        sh"one two   three"
      .assert(_ == sh"one   two three")

    suite(m"Execution tests"):

      given Environment = environments.virtualMachine

      test(m"Echo string"):
        sh"echo hello".exec[Text]()
      .check(_ == t"hello")

      test(m"substitute string into echo"):
        val text = t"Hello world!"
        sh"echo $text".exec[Text]()
      .check(_ == t"Hello world!")

      test(m"pipe output through two commands"):
        (sh"echo 'Hello world'" | sh"sed s/e/a/g").exec[Text]()
      .check(_ == t"Hallo world")

      test(m"read stream of strings"):
        sh"echo 'Hello world'".exec[Stream[Text]]().to(List)
      .assert(_ == List("Hello world"))

      test(m"read stream of bytes"):
        sh"echo 'Hello world'".exec[Stream[Bytes]]().read[Bytes].to(List)
      .assert(_ == Bytes(72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10).to(List))

      test(m"fork sleeping process"):
        val t0 = System.currentTimeMillis
        sh"sleep 0.2".fork[Unit]()
        System.currentTimeMillis - t0
      .assert(_ <= 100L)

      test(m"exec sleeping process"):
        val t0 = System.currentTimeMillis
        sh"sleep 0.2".exec[Unit]()
        System.currentTimeMillis - t0
      .assert(_ >= 200L)

      test(m"fork and await sleeping process"):
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.await()
        System.currentTimeMillis - t0
      .assert(_ >= 200L)

      test(m"fork and abort sleeping process"):
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.abort()
        System.currentTimeMillis - t0
      .assert(_ <= 100L)

      test(m"fork and kill sleeping process"):
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.kill()
        System.currentTimeMillis - t0
      .assert(_ <= 100L)

      test(m"successful exit status"):
        sh"echo hello".exec[Exit]()
      .assert(_ == Exit.Ok)

      test(m"failed exit status"):
        sh"false".exec[Exit]()
      .assert(_ == Exit.Fail(1))

      test(m"nested command"):
        val cmd = sh"echo 'Hello world'"
        sh"sh -c '$cmd'".exec[Text]()
      .check(_ == t"Hello world")

      test(m"implied return type"):
        sh"echo 'Hello world'"()
      .assert(_ == t"Hello world")

      test(m"implied return type for `which`"):
        sh"which cat"()
      .assert(_ == Unix / "bin" / "cat")
