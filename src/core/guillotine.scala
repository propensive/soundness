package guillotine

object Terminal {
  def columns(implicit env: Environment): Option[Int] = scala.util.Try(sh"sh -c 'tput cols 2> /dev/tty'".exec[String].toInt).toOption
  def lines(implicit env: Environment): Option[Int] = scala.util.Try(sh"sh -c 'tput lines 2> /dev/tty'".exec[String].toInt).toOption
}
