package rudiments

import java.util as ju

import language.experimental.captureChecking

object Uuid:
  def unapply(text: Text): Option[Uuid] =
    try Some:
      val uuid = ju.UUID.fromString(text.s).nn
      Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)
    catch case err: Exception => None

  def apply(): Uuid =
    val uuid = ju.UUID.randomUUID().nn
    Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

case class Uuid(msb: Long, lsb: Long):
  def javaUuid: ju.UUID = ju.UUID(msb, lsb)
  def bytes: Bytes = Bytes(msb) ++ Bytes(lsb)
  def text: Text = Text(javaUuid.toString)
