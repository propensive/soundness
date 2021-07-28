package simplistic

trait HtmlAttribute[-L <: String & Singleton, -T]:
  def name: String
  def serialize(value: T): String