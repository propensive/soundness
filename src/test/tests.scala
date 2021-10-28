/*
    Guillotine, version 0.9.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
import escapade.*
import probably.*
import eucalyptus.*
import scala.quoted.*, staging.*

import unsafeExceptions.canThrowAny

given Log(Everything.fine |-> Stdout)

object Tests extends Suite(str"Guillotine tests"):
  def run(using Runner): Unit =
    suite(str"Parsing tests") {
      test(str"parse simple command") {
        sh"ls -la"
      }.assert(_ == Command(str"ls", str"-la"))
      
      test(str"parse a substitution") {
        val flags = str"-la"
        val file = str"filename"
        sh"ls $flags"
      }.assert(_ == Command(str"ls", str"-la"))
      
      test(str"parse two substitutions") {
        val flags = str"-la"
        val file = str"filename"
        sh"ls $flags $file"
      }.assert(_ == Command(str"ls", str"-la", str"filename"))
      
      test(str"parse irregular spacing") {
        val flags = str"-la"
        val file = str"filename"
        sh"ls  $flags     $file"
      }.assert(_ == Command(str"ls", str"-la", str"filename"))
      
      test(str"parse irregular spacing 2") {
        val flags = str"-la"
        val file = str"filename"
        sh"ls  $flags $file"
      }.assert(_ == Command(str"ls", str"-la", str"filename"))
      
      test(str"adjacent substitutions") {
        val a = str"a"
        val b = str"b"
        sh"ls $a$b"
      }.assert(_ == Command(str"ls", str"ab"))
      
      test(str"substitute a list") {
        val a = List(str"a", str"b")
        sh"ls $a"
      }.assert(_ == Command(str"ls", str"a", str"b"))
      
      test(str"substitute a single-quoted list") {
        val a = List(str"a", str"b")
        sh"ls '$a'"
      }.assert(_ == Command(str"ls", str"a b"))
      
      test(str"substitute in a double-quoted list") {
        val a = List(str"a", str"b")
        sh"""ls "$a""""
      }.assert(_ == Command(str"ls", str"a b"))
      
      test(str"insertion after arg") {
        val a = List(str"a", str"b")
        sh"""ls ${a}x"""
      }.assert(_ == Command(str"ls", str"a", str"bx"))
      
      test(str"insertion before arg") {
        val a = List(str"a", str"b")
        sh"""ls x${a}"""
      }.assert(_ == Command(str"ls", str"xa", str"b"))
      
      test(str"insertion before quoted arg") {
        val a = List(str"a", str"b")
        sh"""ls ${a}'x'"""
      }.assert(_ == Command(str"ls", str"a", str"bx"))
      
      test(str"insertion after quoted arg") {
        val a = List(str"a", str"b")
        sh"""ls 'x'${a}"""
      }.assert(_ == Command(str"ls", str"xa", str"b"))
      
      test(str"empty list insertion") {
        val a = List()
        sh"""ls ${a}"""
      }.assert(_ == Command(str"ls"))
      
      test(str"empty list insertion") {
        val a = List()
        sh"""ls '${a}'"""
      }.assert(_ == Command(str"ls", str""))
      
      test(str"empty parameters") {
        sh"""ls '' ''"""
      }.assert(_ == Command(str"ls", str"", str""))
      
      test(str"one empty parameter, specified twice") {
        sh"""ls ''''"""
      }.assert(_ == Command(str"ls", str""))
      
      test(str"single quote inside double quotes") {
        sh"""ls "'" """
      }.assert(_ == Command(str"ls", str"'"))
      
      test(str"double quote inside single quotes") {
        sh"""ls '"' """
      }.assert(_ == Command(str"ls", str"\""))
      
      test(str"escaped double quote") {
        sh"""ls \" """
      }.assert(_ == Command(str"ls", str"\""))
      
      test(str"escaped single quote") {
        sh"""ls \' """
      }.assert(_ == Command(str"ls", str"'"))
    }

    suite(str"rendering DebugString") {
      test(str"simple command") {
        sh"echo Hello World".debug
      }.check(_ == str"""sh"echo Hello World"""")
      
      test(str"simple command with space") {
        sh"echo 'Hello World'".debug
      }.check(_ == str"""sh"echo 'Hello World'"""")
      
      test(str"simple command with quote and space") {
        Command(str"echo", str"Don't stop").debug
      }.check(_ == str"sh\"\"\"echo \"Don't stop\"\"\"\"")

      test(str"simple command with single and double quote") {
        Command(str"echo", str"single ' and double \" quotes").debug
      }.check(_ == str"sh\"\"\"echo \"single ' and double \\\" quotes\"\"\"\"")

      test(str"render pipeline of commands") {
        (sh"echo Hello" | sh"sed s/e/a/g").debug
      }.check(_ == str"""sh"echo Hello" | sh"sed s/e/a/g"""")
    }

    suite(str"equality tests") {
      test(str"check that two commands written differently are equivalent") {
        sh"echo 'hello world'"
      }.assert(_ == sh"""echo "hello world"""")
      
      test(str"check that two commands written with different whitespace are equivalent") {
        sh"one two   three"
      }.assert(_ == sh"one   two three")
    }

    suite(str"Execution tests") {

      given Env = envs.enclosing

      test(str"Echo string") {
        sh"echo hello".exec[Txt]()
      }.check(_ == str"hello")

      test(str"substitute string into echo") {
        val string = "Hello world!"
        sh"echo $string".exec[Txt]()
      }.check(_ == str"Hello world!")

      test(str"pipe output through two commands") {
        (sh"echo 'Hello world'" | sh"sed s/e/a/g").exec[Txt]()
      }.check(_ == str"Hallo world")

      test(str"read stream of strings") {
        sh"echo 'Hello world'".exec[LazyList[Txt]]().to(List)
      }.assert(_ == List("Hello world"))

      test(str"read stream of bytes") {
        sh"echo 'Hello world'".exec[DataStream]().foldLeft(List[Byte]())(_ ++ _.to(List))
      }.assert(_ == List(72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10))

      test(str"fork sleeping process") {
        val t0 = System.currentTimeMillis
        sh"sleep 2".fork[Unit]()
        System.currentTimeMillis - t0
      }.assert(_ <= 1000L)
      
      test(str"exec sleeping process") {
        val t0 = System.currentTimeMillis
        sh"sleep 2".exec[Unit]()
        System.currentTimeMillis - t0
      }.assert(_ >= 2000L)
      
      test(str"fork and await sleeping process") {
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 2".fork[Unit]()
        proc.await()
        System.currentTimeMillis - t0
      }.assert(_ >= 2000L)
      
      test(str"fork and abort sleeping process") {
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 2".fork[Unit]()
        proc.abort()
        System.currentTimeMillis - t0
      }.assert(_ <= 1000L)
      
      test(str"fork and kill sleeping process") {
        val t0 = System.currentTimeMillis
        val proc = sh"sleep 2".fork[Unit]()
        proc.kill()
        System.currentTimeMillis - t0
      }.assert(_ <= 1000L)

      test(str"successful exit status") {
        sh"echo hello".exec[ExitStatus]()
      }.assert(_ == ExitStatus.Ok)
      
      test(str"failed exit status") {
        sh"false".exec[ExitStatus]()
      }.assert(_ == ExitStatus.Fail(1))

      test(str"nested command") {
        val cmd = sh"echo 'Hello world'"
        sh"sh -c '$cmd'".exec[Txt]()
      }.check(_ == str"Hello world")
    }

    // suite("Compilation tests") {
    //   given Compiler = Compiler.make(getClass.getClassLoader)

    //   test("Check final escape character is an error") {
    //     try Check('{"""hello\"""}) catch error => error
    //   }.assert(_ == InterpolationError("an escape character is not permitted at the end"))
      
    //   test("unclosed single quotes") {
    //     try Check('{"""hello ' world"""}) catch error => error
    //   }.assert(_ == InterpolationError("the single quotes have not been closed"))
      
    //   test("unclosed double quotes") {
    //     try Check('{"""hello " world"""}) catch error => error
    //   }.assert(_ == InterpolationError("the double quotes have not been closed"))
    // }
