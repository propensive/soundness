package quagmire

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*
import eucalyptus.*
import tetromino.*
import parasitism.*, threading.platform

import unsafeExceptions.canThrowAny

import Change.*

object Tests extends Suite(t"Quagmire tests"):
  given Realm(t"tests")

  def run(using Runner): Unit =
    import logging.stdout

    suite(t"Diff tests"):
//       test(t"Empty lists"):
//         Diff.diff(IArray[Char](), IArray[Char]())
//       .assert(_ == List())
      
//       test(t"One element, equal"):
//         Diff.diff(IArray('a'), IArray('a'))
//       .assert(_ == List(Keep('a')))
      
      test(t"Two elements, equal"):
        Diff.diff(t"ABCABBA".chars, t"CBABAC".chars)
      .assert(_ == Diff(Keep(0, 0, 'a'), Ins(1, 'b'), Keep(1, 2, 'c'), Keep(2, 3, 'd')))
