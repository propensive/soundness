package simplistic

trait HttpResponse[T]:
  def mimeType: String
  def content(value: T): String