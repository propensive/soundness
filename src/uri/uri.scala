package clairvoyant

trait UriConverter[T]:
  def apply(value: T): String
  def unapply(string: String): Option[T]