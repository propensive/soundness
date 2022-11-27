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
        import logging.stdout
        Diff.diff(t"BCD".chars, t"ABC".chars)
      .assert(_ == Diff(Ins(0, 'A'), Keep(0, 1, 'B'), Keep(1, 2, 'C'), Del(2, 'D')))
      
      test(t"Example from blog"):
        Diff.diff(t"ABCABBA".chars, t"CBABAC".chars)
      .assert(_ == Diff(Del(0, 'A'), Ins(0, 'C'), Keep(1, 1, 'B'), Del(2, 'C'), Keep(3, 2, 'A'), Keep(4, 3, 'B'), Del(5, 'B'), Keep(6, 4, 'A'), Ins(5, 'C')))
      
      test(t"Item swap"):
        Diff.diff(t"AB".chars, t"BA".chars)
      .assert(_ == Diff(Del(0, 'A'), Keep(1, 0, 'B'), Ins(1, 'A')))
      
      test(t"Item change"):
        Diff.diff(t"A".chars, t"C".chars)
      .assert(_ == Diff(Del(0, 'A'), Ins(0, 'C')))
      
      test(t"Item change between values"):
        Diff.diff(t"NAN".chars, t"NCN".chars)
      .assert(_ == Diff(Keep(0, 0, 'N'), Del(1, 'A'), Ins(1, 'C'), Keep(2, 2, 'N')))
      
      test(t"Item swap between values"):
        Diff.diff(t"NABN".chars, t"NBAN".chars)
      .assert(_ == Diff(Keep(0, 0, 'N'), Del(1, 'A'), Keep(2, 1, 'B'), Ins(2, 'A'), Keep(3, 3, 'N')))
      
      test(t"Item swap interspersed with values"):
        Diff.diff(t"AZB".chars, t"BZA".chars)
      .assert(_ == Diff(Del(0, 'A'), Ins(0, 'B'), Keep(1, 1, 'Z'), Del(2, 'B'), Ins(2, 'A')))

