package estrapade

object Diff {
  implicit def seqDiff[T](implicit show: Show[T]): Diff[Seq[T]] = (s1, s2) =>
    if(s1.length != s2.length) "the sequences differ in length: ${s1.size} vs ${s2.size}" else {
      val idx = s1.zip(s2).indexWhere { case (l, r) => l != r }
      s"the sequences differ at index $idx: ${show.show(s1(idx))} vs ${show.show(s2(idx))}"
    }
  
  implicit def arrayDiff[T: Show]: Diff[Array[T]] = { (s1, s2) =>
    val (l1, l2) = (s1.to[List], s2.to[List])
    if(l1 == l2) "the arrays are not equal but have identical elements" else seqDiff.diff(s1, s2)
  }
}

trait Diff[-T] { def diff(t1: T, t2: T): String }

object Show extends Show_1 {
  implicit def showString: Show[String] =
    s => if(s.contains("\n")) "\"\"\""+s+"\"\"\"" else s""""$s""""
  
  implicit def showArray[T](implicit show: Show[T]): Show[Array[T]] =
    arr => arr.to[List].map(show.show).mkString("Array(", ", ", ")")
}

trait Show_1 { implicit def showAnything[T]: Show[T] = _.toString }

trait Show[-T] { def show(t: T): String }
