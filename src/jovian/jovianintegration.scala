package honeycomb

import jovian.*

package attributes.jovian:
  given acceptCharset[T]: Attribute["acceptCharset", Encoding, T] with
    def convert(enc: Encoding): String = enc.name
    override def rename: Option[String] = Some("accept-charset")
  
  given charset[T]: Attribute["charset", Encoding, T] = _.name