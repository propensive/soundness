package eucalyptus

import java.text.SimpleDateFormat
import scala.reflect._, macros._
import language.experimental.macros

object output {
  implicit final val stdout: Destination[String] = System.out.println(_)
  implicit final val stderr: Destination[String] = System.err.println(_)
}

object formats {
  implicit final val Raw: Format[String, String] = (msg, _, _, _, _, _) => msg
  implicit final val Default: Format[String, String] = StandardFormat

  private object StandardFormat extends Format[String, String] {
    private def pad(str: String, length: Int) =
      if(str.length < length) str+(" "*(length - str.length)) else str.take(length)
   
    private val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS ")
    
    def entry(msg: String, tag: Tag, timestamp: Long, level: Level, lineNo: Int, sourceFile: String): String = {
      val sb = new StringBuilder()
      sb.append(dateFormat.format(timestamp))
      sb.append(level.name)
      sb.append(' ')
      sb.append(pad(tag.name, 10))
      sb.append(' ')
      sb.append(pad(s"$sourceFile:$lineNo", 15))
      sb.append(' ')
      sb.append(msg)
      sb.toString
    }
  }
}

case class Engine[Msg, Out](loggers: Logger[Msg, Out]*) {
  def trace(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def debug(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def audit(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def issue(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def error(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def fault(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
}

object Level {
  final val Trace = Level(0)
  final val Debug = Level(1)
  final val Audit = Level(2)
  final val Issue = Level(3)
  final val Error = Level(4)
  final val Fault = Level(5)
  final val names = Array("TRACE", "DEBUG", "AUDIT", "ISSUE", "ERROR", "FAULT")
}

final case class Level(value: Int) extends AnyVal { final def name: String = Level.names(value) }

final case class Logger[Msg, Out](min: Level, max: Level, tagSet: Set[Tag], format: Format[Msg, Out], destination: Destination[Out]) {
  final def matches(tag: Tag, level: Int): Boolean = level >= min.value && level <= max.value && tagSet.contains(tag)
  final def above(level: Level): Logger[Msg, Out] = copy(level, max, tagSet, format, destination)
  final def below(level: Level): Logger[Msg, Out] = copy(min, level, tagSet, format, destination)
}

trait Destination[T] { def consume(msg: T): Unit }

trait Format[-Msg, +Out] { def entry(msg: Msg, tag: Tag, timestamp: Long, level: Level, lineNo: Int, sourceFile: String): Out }

object Tag { implicit final val Default = Tag("default") }

object Log {
  def apply(tags: Tag*): LogTags = LogTags(tags.to[Set])

  case class LogTags(tagSet: Set[Tag]) {
    def as[Msg, Out](format: Format[Msg, Out]): LogTagsFormat[Msg, Out] = LogTagsFormat(tagSet, format)
  }

  case class LogTagsFormat[Msg, Out](tagSet: Set[Tag], format: Format[Msg, Out]) {
    def to(destination: Destination[Out]): Logger[Msg, Out] = Logger(Level.Trace, Level.Fault, tagSet, format, destination)
  }
}

final case class Tag(name: String) extends AnyVal

object Macros {
  def doLog(c: blackbox.Context)(msg: c.Tree)(tag: c.Tree): c.Tree = {
    import c._, universe._
    val lineNo = enclosingPosition.line
    val sourceFile = enclosingPosition.source.toString
    val q"$base.$method($_)($_)" = macroApplication // "
   
    val level = Level.names.indexOf(method.toString.toUpperCase)

    q"""$base.loggers.foreach { log =>
          if(log.matches($tag, $level)) log.destination.consume(log.format.entry($msg, $tag, _root_.java.lang.System.currentTimeMillis, _root_.eucalyptus.Level($level), $lineNo, $sourceFile))
        }"""
  }
}
