package eucalyptus

import annotation.implicitNotFound
import java.text.SimpleDateFormat

import log.{Tag, Level, Format, Logger}

object output {
  implicit final val stdout: Logger[String] = (s: String) => System.out.println(s)
  implicit final val stderr: Logger[String] = (s: String) => System.err.println(s)
}

object levels {
  // Helper method for creating singleton-literal-typed Level instances
  private def level[Name <: String](severity: Int, name: Name): Level[name.type] =
    Level(severity, name)

  implicit final val trace = level(0, "trace")
  implicit final val debug = level(1, "debug")
  implicit final val check = level(2, "check")
  implicit final val issue = level(3, "issue")
  implicit final val error = level(4, "error")
  implicit final val fault = level(5, "fault")
}

object formats {

  implicit final val standard: Format[String, String] = StandardFormat
  implicit final val raw: Format[String, String] = RawFormat

  private object RawFormat extends Format[String, String] {
    def entry(tag: Tag, value: String, timestamp: Long, level: Level[_]): String = value
  }

  private object StandardFormat extends Format[String, String] {
    private def pad(str: String, length: Int) =
      if(str.length < length) str+(" "*(length - str.length)) else str.take(length)
   
    private val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS ")
    
    def entry(tag: Tag, value: String, timestamp: Long, level: Level[_]): String = {
      val sb = new StringBuilder()
      sb.append(dateFormat.format(timestamp))
      sb.append(level.upperName)
      sb.append(' ')
      sb.append(pad(tag.name, 10))
      sb.append(' ')
      sb.append(value)
      sb.toString
    }
  }
}

object log {
  def trace[Value, Out, T <: Tag](value: Value)(implicit tagVisibility: TagVisibility[T], format: Format[Value, Out], logger: Logger[Out]): Unit =
    if(tagVisibility.visibility.severity <= levels.trace.severity) doLog(value, tagVisibility.tag, format, levels.trace, logger)

  def debug[Value, Out, T <: Tag](value: Value)(implicit tagVisibility: TagVisibility[T], format: Format[Value, Out], logger: Logger[Out]): Unit =
    if(tagVisibility.visibility.severity <= levels.debug.severity) doLog(value, tagVisibility.tag, format, levels.debug, logger)

  def check[Value, Out, T <: Tag](value: Value)(implicit tagVisibility: TagVisibility[T], format: Format[Value, Out], logger: Logger[Out]): Unit =
    if(tagVisibility.visibility.severity <= levels.check.severity) doLog(value, tagVisibility.tag, format, levels.check, logger)

  def issue[Value, Out, T <: Tag](value: Value)(implicit tagVisibility: TagVisibility[T], format: Format[Value, Out], logger: Logger[Out]): Unit =
    if(tagVisibility.visibility.severity <= levels.issue.severity) doLog(value, tagVisibility.tag, format, levels.issue, logger)

  def error[Value, Out, T <: Tag](value: Value)(implicit tagVisibility: TagVisibility[T], format: Format[Value, Out], logger: Logger[Out]): Unit =
    if(tagVisibility.visibility.severity <= levels.error.severity) doLog(value, tagVisibility.tag, format, levels.error, logger)

  def fault[Value, Out, T <: Tag](value: Value)(implicit tagVisibility: TagVisibility[T], format: Format[Value, Out], logger: Logger[Out]): Unit =
    if(tagVisibility.visibility.severity <= levels.fault.severity) doLog(value, tagVisibility.tag, format, levels.fault, logger)

  private def doLog[Value, T](value: Value, tag: Tag, format: Format[Value, T], level: Level[_],
      logger: Logger[T]): Unit =
    logger.log(format.entry(tag, value, System.currentTimeMillis, level))
  
  @implicitNotFound("eucalyptus: could not find an implicit Logger; try importing eucalyptus.output.stdout")
  trait Logger[-T] { def log(entry: T): Unit }

  @implicitNotFound("eucalyptus: could not find an implicit Format; try importing eucalyptus.formats.standard")
  trait Format[-L, +T] { def entry(tag: Tag, value: L, timestamp: Long, level: Level[_]): T }
 
  object Tag { implicit val defaultTag: Tag = Tag("") }
 
  case class Tag(name: String) {
    implicit val traceLevel: Visibility[this.type] = Visibility(0)
    implicit val debugLevel: Visibility[this.type] = Visibility(1)
    implicit val checkLevel: Visibility[this.type] = Visibility(2)
    implicit val issueLevel: Visibility[this.type] = Visibility(3)
    implicit val errorLevel: Visibility[this.type] = Visibility(4)
    implicit val fatalLevel: Visibility[this.type] = Visibility(5)
  }

  object Level { implicit final val defaultLevel = levels.check }

  case class Level[Name <: String] private[eucalyptus] (severity: Int, name: String) {
    val upperName: String = name.toUpperCase
  }

  object Visibility {
    implicit def defaultVisibility(implicit level: Level[_]): Visibility[Tag] = Visibility(level.severity)
  }

  case class Visibility[T <: Tag](severity: Int)


  object TagVisibility {
    implicit def join[T <: Tag](implicit visibility: Visibility[T], tag: T): TagVisibility[T] =
      new TagVisibility(tag)(visibility)
  }
  
  class TagVisibility[T <: Tag](val tag: T)(val visibility: Visibility[T])

}
