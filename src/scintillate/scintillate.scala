package honeycomb

import scintillate.*
import rudiments.*

package attributes.scintillate:
  given action[T]: Attribute["action", Uri, T] = _.toString
  given cite[T]: Attribute["cite", Uri, T] = _.toString
  given data[T]: Attribute["data", Uri, T] = _.toString
  given formaction[T]: Attribute["formaction", Uri, T] = _.toString
  given formenctype[T]: Attribute["formenctype", MediaType, T] = _.toString
  given formmethod[T]: Attribute["formmethod", Method, T] = _.toString
  given href[T]: Attribute["href", Uri, T] = _.toString
  given enctype[T]: Attribute["enctype", MediaType, T] = _.toString
  given manifest[T]: Attribute["manifest", Uri, T] = _.toString
  given media[T]: Attribute["media", MediaType, T] = _.toString
  given poster[T]: Attribute["poster", Uri, T] = _.toString
  given src[T]: Attribute["src", Uri, T] = _.toString

  given typeName[T]: Attribute["typeName", MediaType, T] with
    override def rename: Option[String] = Some("type")
    def convert(value: MediaType): String = value.toString

given SimpleHandler[HtmlDoc] = SimpleHandler("text/html", html => LazyList(HtmlDoc.serialize(html).bytes))