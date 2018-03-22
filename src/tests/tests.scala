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
  }
}
