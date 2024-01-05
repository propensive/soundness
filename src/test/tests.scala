/*
    Guillotine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package guillotine

import contextual.*
import rudiments.*
import gossamer.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import serpentine.*, hierarchies.unix
import anticipation.*
import turbulence.*
import ambience.*
import spectacular.*
import probably.*
import eucalyptus.*
import perforate.*, errorHandlers.throwUnsafely

import unsafeExceptions.canThrowAny

given WorkingDirectory = WorkingDirectory(Unset)

object Tests extends Suite(t"Guillotine tests"):
  def run(): Unit =
    suite(t"Parsing tests"):
      test(t"parse simple command"):
        sh"ls -la"
      .assert(_ == Command(t"ls", t"-la"))
      
      test(t"parse a substitution"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls $flags"
      .assert(_ == Command(t"ls", t"-la"))
      
      test(t"parse two substitutions"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls $flags $file"
      .assert(_ == Command(t"ls", t"-la", t"filename"))
      
      test(t"parse irregular spacing"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls  $flags     $file"
      .assert(_ == Command(t"ls", t"-la", t"filename"))
      
      test(t"parse irregular spacing 2"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls  $flags $file"
      .assert(_ == Command(t"ls", t"-la", t"filename"))
      
      test(t"adjacent substitutions"):
        val a = t"a"
        val b = t"b"
        sh"ls $a$b"
      .assert(_ == Command(t"ls", t"ab"))
      
      test(t"substitute a list"):
        val a = List(t"a", t"b")
        sh"ls $a"
      .assert(_ == Command(t"ls", t"a", t"b"))
      
      test(t"substitute a single-quoted list"):
        val a = List(t"a", t"b")
        sh"ls '$a'"
      .assert(_ == Command(t"ls", t"a b"))
      
      test(t"substitute in a double-quoted list"):
        val a = List(t"a", t"b")
        sh"""ls "$a""""
      .assert(_ == Command(t"ls", t"a b"))
      
      test(t"insertion after arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}x"""
      .assert(_ == Command(t"ls", t"a", t"bx"))
      
      test(t"insertion before arg"):
        val a = List(t"a", t"b")
        sh"""ls x${a}"""
      .assert(_ == Command(t"ls", t"xa", t"b"))
      
      test(t"insertion before quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}'x'"""
      .assert(_ == Command(t"ls", t"a", t"bx"))
      
      test(t"insertion after quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls 'x'${a}"""
      .assert(_ == Command(t"ls", t"xa", t"b"))
      
      test(t"empty list insertion"):
        val a = List()
        sh"""ls ${a}"""
      .assert(_ == Command(t"ls"))
      
      test(t"empty list insertion"):
        val a = List()
        sh"""ls '${a}'"""
      .assert(_ == Command(t"ls", t""))
      
      test(t"empty parameters"):
        sh"""ls '' ''"""
      .assert(_ == Command(t"ls", t"", t""))
      
      test(t"one empty parameter, specified twice"):
        sh"""ls ''''"""
      .assert(_ == Command(t"ls", t""))
      
      test(t"single quote inside double quotes"):
        sh"""ls "'" """
      .assert(_ == Command(t"ls", t"'"))
      
      test(t"double quote inside single quotes"):
        sh"""ls '"' """
      .assert(_ == Command(t"ls", t"\""))
      
      test(t"escaped double quote"):
        sh"""ls \" """
      .assert(_ == Command(t"ls", t"\""))
      
      test(t"escaped single quote"):
        sh"""ls \' """
      .assert(_ == Command(t"ls", t"'"))

    suite(t"rendering Debug")
      test(t"simple command"):
        sh"echo Hello World".debug
      .check(_ == t"""sh"echo Hello World"""")
      
      test(t"simple command with space"):
        sh"echo 'Hello World'".debug
      .check(_ == t"""sh"echo 'Hello World'"""")
      
      test(t"simple command with quote and space"):
        Command(t"echo", t"Don't stop").debug
      .check(_ == t"sh\"\"\"echo \"Don't stop\"\"\"\"")

      test(t"simple command with single and double quote"):
        Command(t"echo", t"single ' and double \" quotes").debug
      .check(_ == t"sh\"\"\"echo \"single ' and double \\\" quotes\"\"\"\"")

      test(t"render pipeline of commands"):
        (sh"echo Hello" | sh"sed s/e/a/g").debug
      .check(_ == t"""sh"echo Hello" | sh"sed s/e/a/g"""")

    suite(t"equality tests"):
      test(t"check that two commands written differently are equivalent"):
        sh"echo 'hello world'"
      .assert(_ == sh"""echo "hello world"""")
      
      test(t"check that two commands written with different whitespace are equivalent"):
        sh"one two   three"
      .assert(_ == sh"one   two three")

    suite(t"Execution tests"):

      given Environment = environments.virtualMachine

      test(t"Echo string"):
        sh"echo hello".exec[Text]()
      .check(_ == t"hello")

      test(t"substitute string into echo"):
        val text = t"Hello world!"
        sh"echo $text".exec[Text]()
      .check(_ == t"Hello world!")

      test(t"pipe output through two commands"):
        (sh"echo 'Hello world'" | sh"sed s/e/a/g").exec[Text]()
      .check(_ == t"Hallo world")

      test(t"read stream of strings"):
        sh"echo 'Hello world'".exec[LazyList[Text]]().to(List)
      .assert(_ == List("Hello world"))

      test(t"read stream of bytes"):
        sh"echo 'Hello world'".exec[LazyList[Bytes]]().read[Bytes].to(List)
      .assert(_ == Bytes(72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10).to(List))

      test(t"fork sleeping process"):
        val t0 = System.currentTimeMillis
        sh"sleep 0.2".fork[Unit]()
        System.currentTimeMillis - t0
      .assert(_ <= 100L)
      
      test(t"exec sleeping process"):
        val t0 = System.currentTimeMillis
        sh"sleep 0.2".exec[Unit]()
        System.currentTimeMillis - t0
      .assert(_ >= 200L)
      
      test(t"fork and await sleeping process"):
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.await()
        System.currentTimeMillis - t0
      .assert(_ >= 200L)
      
      test(t"fork and abort sleeping process"):
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.abort()
        System.currentTimeMillis - t0
      .assert(_ <= 100L)
      
      test(t"fork and kill sleeping process"):
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.kill()
        System.currentTimeMillis - t0
      .assert(_ <= 100L)

      test(t"successful exit status"):
        sh"echo hello".exec[ExitStatus]()
      .assert(_ == ExitStatus.Ok)
      
      test(t"failed exit status"):
        sh"false".exec[ExitStatus]()
      .assert(_ == ExitStatus.Fail(1))

      test(t"nested command"):
        val cmd = sh"echo 'Hello world'"
        sh"sh -c '$cmd'".exec[Text]()
      .check(_ == t"Hello world")

      test(t"implied return type"):
        sh"echo 'Hello world'"()
      .assert(_ == t"Hello world")
      
      test(t"implied return type for `which`"):
        import fileApi.galileiApi
        sh"which cat"()
      .assert(_ == Unix / p"bin" / p"cat")

