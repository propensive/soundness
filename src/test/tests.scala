package guillotine

import contextual.*
import probably.*

object Tests extends Suite("Guillotine tests"):
  def run(using Runner): Unit =
    suite("Parsing tests") {
      test("parse simple command") {
        sh"ls -la"
      }.assert(_ == Command("ls", "-la"))
      
      test("parse a substitution") {
        val flags = "-la"
        val file = "filename"
        sh"ls $flags".debug
      }.assert(_ == Command("ls", "-la"))
      
      test("parse two substitutions") {
        val flags = "-la"
        val file = "filename"
        sh"ls $flags $file".debug
      }.assert(_ == Command("ls", "-la", "filename"))
      
      test("parse irregular spacing") {
        val flags = "-la"
        val file = "filename"
        sh"ls  $flags     $file".debug
      }.assert(_ == Command("ls", "-la", "filename"))
      
      test("parse irregular spacing") {
        val flags = "-la"
        val file = "filename"
        sh"ls  $flags $file".debug
      }.assert(_ == Command("ls", "-la", "filename"))
      
      test("adjacent substitutions") {
        val a = "a"
        val b = "b"
        sh"ls $a$b".debug
      }.assert(_ == Command("ls", "ab"))
      
      test("substitute a list") {
        val a = List("a", "b")
        sh"ls $a".debug
      }.assert(_ == Command("ls", "a", "b"))
      
      test("substitute a single-quoted list") {
        val a = List("a", "b")
        sh"ls '$a'".debug
      }.assert(_ == Command("ls", "a b"))
      
      test("substitute in a double-quoted list") {
        val a = List("a", "b")
        sh"""ls "$a"""".debug
      }.assert(_ == Command("ls", "a b"))
      
      test("insertion after arg") {
        val a = List("a", "b")
        sh"""ls ${a}x""".debug
      }.assert(_ == Command("ls", "a", "bx"))
      
      test("insertion before arg") {
        val a = List("a", "b")
        sh"""ls x${a}""".debug
      }.assert(_ == Command("ls", "xa", "b"))
      
      test("insertion before quoted arg") {
        val a = List("a", "b")
        sh"""ls ${a}'x'""".debug
      }.assert(_ == Command("ls", "a", "bx"))
      
      test("insertion after quoted arg") {
        val a = List("a", "b")
        sh"""ls 'x'${a}""".debug
      }.assert(_ == Command("ls", "xa", "b"))
      
      test("empty list insertion") {
        val a = List()
        sh"""ls ${a}""".debug
      }.assert(_ == Command("ls"))
      
      test("empty list insertion") {
        val a = List()
        sh"""ls '${a}'""".debug
      }.assert(_ == Command("ls", ""))
      
      test("empty parameters") {
        sh"""ls '' ''""".debug
      }.assert(_ == Command("ls", "", ""))
      
      test("one empty parameter, specified twice") {
        sh"""ls ''''""".debug
      }.assert(_ == Command("ls", ""))
      
      test("single quote inside double quotes") {
        sh"""ls "'" """.debug
      }.assert(_ == Command("ls", "'"))
      
      test("double quote inside single quotes") {
        sh"""ls '"' """.debug
      }.assert(_ == Command("ls", "\""))
      
      test("escaped double quote") {
        sh"""ls \" """.debug
      }.assert(_ == Command("ls", "\""))
      
      test("escaped single quote") {
        sh"""ls \' """.debug
      }.assert(_ == Command("ls", "'"))
    }

    suite("Execution tests") {

      given Env = envs.enclosing

      test("Echo string") {
        val result = sh"echo hello".exec[String]()
        println(result)
        result
      }.assert(_ == "hello")
    }