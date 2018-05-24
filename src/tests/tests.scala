/*

  Guillotine, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

 */
package guillotine.tests

import probation.{TestApp, test}
import contextual.data.scalac._
import contextual.data.fqt._

import guillotine._

object Tests extends TestApp {

  def tests(): Unit = {
    test("basic argument parsing") {
      sh"echo foo bar".args
    }.assert(_ == List("echo", "foo", "bar"))

    test("parse arguments with single quotes") {
      sh"'echo' 'foo' 'bar'".args
    }.assert(_ == List("echo", "foo", "bar"))

    test("parse arguments with double quotes") {
      sh""""echo" "foo" "bar"""".args
    }.assert(_ == List("echo", "foo", "bar"))

    test("parse single-quoted arguments with space") {
      sh"""echo 'foo bar'""".args
    }.assert(_ == List("echo", "foo bar"))

    test("parse double-quoted arguments with space") {
      sh"""echo "foo bar"""".args
    }.assert(_ == List("echo", "foo bar"))

    test("double-spacing") {
      sh"""echo  foo  bar""".args
    }.assert(_ == List("echo", "foo", "bar"))

    test("empty quoted string") {
      sh"""echo '' bar""".args
    }.assert(_ == List("echo", "", "bar"))

    test("mid-word single-quoting") {
      sh"""echo foo'b'ar""".args
    }.assert(_ == List("echo", "foobar"))

    test("mid-word double-quoting") {
      sh"""echo foo"b"ar""".args
    }.assert(_ == List("echo", "foobar"))

    test("substitution with space") {
      val sub = "Hello world"
      sh"echo $sub".args
    }.assert(_ == List("echo", "Hello world"))

    test("substitution with single quote") {
      val sub = "Hello ' world"
      sh"echo $sub".args
    }.assert(_ == List("echo", "Hello ' world"))

    test("substitution into single quotes") {
      val sub = "Hello ' \" world"
      sh"echo 'x${sub}x'".args
    }.assert(_ == List("echo", "xHello ' \" worldx"))

    test("substitution into double quotes") {
      val sub = "Hello ' \" world"
      sh"""echo "x${sub}x"""".args
    }.assert(_ == List("echo", "xHello ' \" worldx"))

    test("substitution with double quote") {
      val sub = "Hello \" world"
      sh"echo $sub".args
    }.assert(_ == List("echo", "Hello \" world"))

    test("array substitution with no quotes") {
      val sub = List("foo", "bar", "baz")
      sh"echo $sub".args
    }.assert(_ == List("echo", "foo", "bar", "baz"))

    test("double quoted array substitution") {
      val sub = List("foo", "bar", "baz")
      sh"""echo "$sub"""".args
    }.assert(_ == List("echo", "foo bar baz"))

    test("single quoted array substitution") {
      val sub = List("foo", "bar", "baz")
      sh"""echo '$sub'""".args
    }.assert(_ == List("echo", "foo bar baz"))

    test("quoted array substitution with spaces") {
      val sub = List("foo", "bar baz")
      sh"""echo '$sub'""".args
    }.assert(_ == List("echo", "foo bar baz"))

    test("unquoted array substitution with spaces") {
      val sub = List("foo", "bar baz")
      sh"""echo $sub""".args
    }.assert(_ == List("echo", "foo", "bar baz"))

    test("capture of string output") {
      sh"echo foo bar".exec[String]
    }.assert(_ == "foo bar")

    test("capture of integer output") {
      sh"echo 42".exec[Int]
    }.assert(_ == 42)

    test("capture of exit status with string") {
      sh"echo foobar".exec[Exit[String]]
    }.assert(_ == Exit(0, "foobar"))

    test("capture of exit status with int") {
      sh"echo 42".exec[Exit[Int]]
    }.assert(_ == Exit(0, 42))

    ()
  }
}
