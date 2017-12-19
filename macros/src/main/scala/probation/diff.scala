package probation

object Diff extends Diff_1 {
  implicit def seqDiff[T](implicit show: Show[T]): Diff[Seq[T]] = (s1, s2) =>
    if(s1.length != s2.length) s"the sequences differ in length: ${s1.size} vs ${s2.size}" else {
      val idx = s1.zip(s2).indexWhere { case (l, r) => l != r }
      s"the sequences differ at index $idx: ${show.show(s1(idx))} vs ${show.show(s2(idx))}"
    }
  
  implicit def arrayDiff[T: Show]: Diff[Array[T]] = { (s1, s2) =>
    val (l1, l2) = (s1.to[List], s2.to[List])
    if(l1 == l2) "the arrays are not equal but have identical elements" else seqDiff.diff(s1, s2)
  }
}

trait Diff_1 {
  implicit def diffAnything[T](implicit show: Show[T]): Diff[T] =
    (t1, t2) => s"${show.show(t1)} did not equal ${show.show(t2)}"
}

/** typeclass for describing, in words, the differences between two vales of the same type */
trait Diff[-T] { def diff(t1: T, t2: T): String }

object Show extends Show_1 {
  implicit def showString: Show[String] =
    s => if(s.contains("\n")) "\"\"\""+s+"\"\"\"" else s""""$s""""
  
  implicit def showArray[T](implicit show: Show[T]): Show[Array[T]] =
    arr => arr.to[List].map(show.show).mkString("Array(", ", ", ")")

  implicit def showMap[T: Show]: Show[Map[String, T]] = map =>
    map.map { case (label, value) => s"$label=${implicitly[Show[T]].show(value)}" }.mkString(",")
}

/** low-priority [[Show]] typeclass instance */
trait Show_1 { implicit def showAnything[T]: Show[T] = _.toString }

/** typeclass for showing a value as a string */
trait Show[-T] { def show(t: T): String }
