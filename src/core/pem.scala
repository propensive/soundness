package gastronomy

import rudiments.*

case class Pem(kind: String, data: Bytes):
  def serialize: String = Seq(
    Seq(s"-----BEGIN $kind-----"),
    data.grouped(48).to(Seq).map(_.encode[Base64]),
    Seq(s"-----END $kind-----")
  ).flatten.join("\n")

object Pem:
  def parse(string: String): Pem exposes PemParseError =
    val lines = string.trim.cut("\n")
    
    val label = lines.head match
      case s"-----BEGIN $label-----" => label
      case _                         => throw PemParseError("the BEGIN line could not be found")
    
    lines.tail.indexWhere {
      case s"-----END $label-----" => true
      case _                       => false
    } match
      case -1  =>
        throw PemParseError("the message's END line could not be found")
      case idx =>
        try Pem(label, lines.tail.take(idx).join.decode[Base64])
        catch Exception => throw PemParseError("could not parse Base64 PEM message")