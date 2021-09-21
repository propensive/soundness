/*
    Guillotine, version 0.5.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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
import probably.*
import scala.quoted.*, staging.*

object Tests extends Suite("Guillotine tests"):
  def run(using Runner): Unit =
    suite("Parsing tests") {
      test("parse simple command") {
        sh"ls -la"
      }.assert(_ == Command("ls", "-la"))
      
      test("parse a substitution") {
        val flags = "-la"
        val file = "filename"
        sh"ls $flags"
      }.assert(_ == Command("ls", "-la"))
      
      test("parse two substitutions") {
        val flags = "-la"
        val file = "filename"
        sh"ls $flags $file"
      }.assert(_ == Command("ls", "-la", "filename"))
      
      test("parse irregular spacing") {
        val flags = "-la"
        val file = "filename"
        sh"ls  $flags     $file"
      }.assert(_ == Command("ls", "-la", "filename"))
      
      test("parse irregular spacing") {
        val flags = "-la"
        val file = "filename"
        sh"ls  $flags $file"
      }.assert(_ == Command("ls", "-la", "filename"))
      
      test("adjacent substitutions") {
        val a = "a"
        val b = "b"
        sh"ls $a$b"
      }.assert(_ == Command("ls", "ab"))
      
      test("substitute a list") {
        val a = List("a", "b")
        sh"ls $a"
      }.assert(_ == Command("ls", "a", "b"))
      
      test("substitute a single-quoted list") {
        val a = List("a", "b")
        sh"ls '$a'"
      }.assert(_ == Command("ls", "a b"))
      
      test("substitute in a double-quoted list") {
        val a = List("a", "b")
        sh"""ls "$a""""
      }.assert(_ == Command("ls", "a b"))
      
      test("insertion after arg") {
        val a = List("a", "b")
        sh"""ls ${a}x"""
      }.assert(_ == Command("ls", "a", "bx"))
      
      test("insertion before arg") {
        val a = List("a", "b")
        sh"""ls x${a}"""
      }.assert(_ == Command("ls", "xa", "b"))
      
      test("insertion before quoted arg") {
        val a = List("a", "b")
        sh"""ls ${a}'x'"""
      }.assert(_ == Command("ls", "a", "bx"))
      
      test("insertion after quoted arg") {
        val a = List("a", "b")
        sh"""ls 'x'${a}"""
      }.assert(_ == Command("ls", "xa", "b"))
      
      test("empty list insertion") {
        val a = List()
        sh"""ls ${a}"""
      }.assert(_ == Command("ls"))
      
      test("empty list insertion") {
        val a = List()
        sh"""ls '${a}'"""
      }.assert(_ == Command("ls", ""))
      
      test("empty parameters") {
        sh"""ls '' ''"""
      }.assert(_ == Command("ls", "", ""))
      
      test("one empty parameter, specified twice") {
        sh"""ls ''''"""
      }.assert(_ == Command("ls", ""))
      
      test("single quote inside double quotes") {
        sh"""ls "'" """
      }.assert(_ == Command("ls", "'"))
      
      test("double quote inside single quotes") {
        sh"""ls '"' """
      }.assert(_ == Command("ls", "\""))
      
      test("escaped double quote") {
        sh"""ls \" """
      }.assert(_ == Command("ls", "\""))
      
      test("escaped single quote") {
        sh"""ls \' """
      }.assert(_ == Command("ls", "'"))
    }

    suite("Execution tests") {

      given Env = envs.enclosing

      test("Echo string") {
        sh"echo hello".exec[String]()
      }.assert(_ == "hello")
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