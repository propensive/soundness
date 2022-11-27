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
    import logging.silent

    suite(t"Diff tests"):
      test(t"Empty lists"):
        Diff.diff(IArray[Char](), IArray[Char]())
      .assert(_ == Diff())
      
      test(t"One element, equal"):
        Diff.diff(IArray('a'), IArray('a'))
      .assert(_ == Diff(Keep(0, 0, 'a')))
      
      test(t"Two elements, equal"):
        Diff.diff(IArray('a', 'b'), IArray('a', 'b'))
      .assert(_ == Diff(Keep(0, 0, 'a'), Keep(1, 1, 'b')))
      
      test(t"Insertion to empty list"):
        Diff.diff(IArray[Char](), IArray('a'))
      .assert(_ == Diff(Ins(0, 'a')))
      
      test(t"Deletion to become empty list"):
        Diff.diff(t"a".chars, t"".chars)
      .assert(_ == Diff(Del(0, 'a')))
      
      test(t"Prefix to short list"):
        Diff.diff(t"BC".chars, t"ABC".chars)
      .assert(_ == Diff(Ins(0, 'A'), Keep(0, 1, 'B'), Keep(1, 2, 'C')))
      
      test(t"Suffix to short list"):
        Diff.diff(t"AB".chars, t"ABC".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Keep(1, 1, 'B'), Ins(2, 'C')))
      
      test(t"Insertion in middle of short list"):
        Diff.diff(t"AC".chars, t"ABC".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Ins(1, 'B'), Keep(1, 2, 'C')))
      
      test(t"Deletion from middle of short list"):
        Diff.diff(t"ABC".chars, t"AC".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Del(1, 'B'), Keep(2, 1, 'C')))
      
      test(t"Deletion from start of short list"):
        Diff.diff(t"ABC".chars, t"BC".chars)
      .assert(_ == Diff(Del(0, 'A'), Keep(1, 0, 'B'), Keep(2, 1, 'C')))
      
      test(t"Deletion from end of short list"):
        Diff.diff(t"ABC".chars, t"AB".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Keep(1, 1, 'B'), Del(2, 'C')))
      
      test(t"Multiple inner keeps"):
        Diff.diff(t"BCD".chars, t"ABC".chars)
      .assert(_ == Diff(Ins(0, 'A'), Keep(0, 1, 'B'), Keep(1, 2, 'C'), Del(2, 'D')))
      
      test(t"Example from blog"):
        Diff.diff(t"ABCABBA".chars, t"CBABAC".chars)
      .assert(_ == Diff(Del(0, 'A'), Del(1, 'B'), Keep(2, 0, 'C'), Ins(1, 'B'), Keep(3, 2, 'A'), Keep(4, 3, 'B'), Del(5, 'B'), Keep(6, 4, 'A'), Ins(5, 'C')))
      
      test(t"Reduced example from blog"):
        Diff.diff(t"A".chars, t"C".chars)
      .assert(_ == Diff(Del(0, 'A'), Ins(0, 'C')))

