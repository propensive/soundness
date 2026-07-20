                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package crossparse

import anticipation.*
import contingency.*, strategies.throwUnsafely
import proscenium.*
import rudiments.*
import vacuous.*

// ── Third-party baselines for the document matrix ───────────────────────
// The best-in-class library for each format decodes the same corpus bytes
// to `String`-field mirror records: Jsoniter (JSON, macro-generated
// codecs), borer (CBOR, derived map-based codecs), protobuf-java (a
// hand-written `CodedInputStream` walk — the fastest possible use of the
// reference library), Aalto (XML, a hand-written StAX pull walk), and
// snakeyaml-engine (YAML, composed to native collections and mapped by
// hand). TEL and BinTEL have no third-party implementations.

case class MParam(key: String, value: String)
case class MServlet(servletName: String, servletClass: String, params: List[MParam])
case class MMapping(servletName: String, url: String)
case class MTaglib(uri: String, location: String)
case class MWebApp(servlets: List[MServlet], mappings: List[MMapping], taglib: MTaglib)
case class MConfig(webApp: MWebApp)
case class MMenuItem(value: String, onclick: String)
case class MPopup(menuitems: List[MMenuItem])
case class MMenu(id: String, value: String, popup: MPopup)
case class MMenuDoc(menu: MMenu)
case class MUser(id: Int, username: String, email: String, active: Boolean, role: String)
case class MUsers(users: List[MUser])

case class MLogEntry
  ( timestamp: Long, level: String, service: String, requestId: String, userId: Int,
    message: String )

case class MLogs(logs: List[MLogEntry])

case class MTransaction
  ( from: String, to: String, valueWei: String, valueEth: String, gasPriceWei: String,
    gasUsed: Int, blockNumber: Long, nonce: Int, temperatureDelta: Double )

case class MTransactions(transactions: List[MTransaction])
case class MInts(values: List[Int])
case class MDecimals(values: List[Double])

// The expected mirror of each corpus, for the correctness gates.
object mirrors:
  def config(value: Config): MConfig =
    MConfig
      ( MWebApp
          ( value.webApp.servlets.map: servlet =>
              MServlet
                ( servlet.servletName.s, servlet.servletClass.s,
                  servlet.params.map { param => MParam(param.key.s, param.value.s) } ),
            value.webApp.mappings.map { mapping => MMapping(mapping.servletName.s, mapping.url.s) },
            MTaglib(value.webApp.taglib.uri.s, value.webApp.taglib.location.s) ) )

  def menu(value: MenuDoc): MMenuDoc =
    MMenuDoc
      ( MMenu
          ( value.menu.id.s, value.menu.value.s,
            MPopup
              ( value.menu.popup.menuitems.map: item =>
                  MMenuItem(item.value.s, item.onclick.s) ) ) )

  def users(value: Users): MUsers =
    MUsers
      ( value.users.map: user =>
          MUser(user.id, user.username.s, user.email.s, user.active, user.role.s) )

  def logs(value: Logs): MLogs =
    MLogs
      ( value.logs.map: entry =>
          MLogEntry
            ( entry.timestamp, entry.level.s, entry.service.s, entry.requestId.s,
              entry.userId, entry.message.s ) )

  def transactions(value: Transactions): MTransactions =
    MTransactions
      ( value.transactions.map: transaction =>
          MTransaction
            ( transaction.from.s, transaction.to.s, transaction.valueWei.s,
              transaction.valueEth.s, transaction.gasPriceWei.s, transaction.gasUsed,
              transaction.blockNumber, transaction.nonce, transaction.temperatureDelta ) )

  def ints(value: Ints): MInts = MInts(value.values)
  def decimals(value: Decimals): MDecimals = MDecimals(value.values)

// The Jsoniter codecs: one macro-generated codec per document root.
object jsoniterCodecs:
  import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
  import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

  val config: JsonValueCodec[MConfig] = JsonCodecMaker.make
  val menu: JsonValueCodec[MMenuDoc] = JsonCodecMaker.make
  val users: JsonValueCodec[MUsers] = JsonCodecMaker.make
  val logs: JsonValueCodec[MLogs] = JsonCodecMaker.make
  val transactions: JsonValueCodec[MTransactions] = JsonCodecMaker.make
  val ints: JsonValueCodec[MInts] = JsonCodecMaker.make
  val decimals: JsonValueCodec[MDecimals] = JsonCodecMaker.make

// The borer decoders: map-based derived codecs, transitively.
object borerCodecs:
  import io.bullet.borer.Decoder
  import io.bullet.borer.derivation.MapBasedCodecs.*

  given Decoder[MParam] = deriveDecoder
  given Decoder[MServlet] = deriveDecoder
  given Decoder[MMapping] = deriveDecoder
  given Decoder[MTaglib] = deriveDecoder
  given Decoder[MWebApp] = deriveDecoder
  val config: Decoder[MConfig] = deriveDecoder

  given Decoder[MMenuItem] = deriveDecoder
  given Decoder[MPopup] = deriveDecoder
  given Decoder[MMenu] = deriveDecoder
  val menu: Decoder[MMenuDoc] = deriveDecoder

  given Decoder[MUser] = deriveDecoder
  val users: Decoder[MUsers] = deriveDecoder

  given Decoder[MLogEntry] = deriveDecoder
  val logs: Decoder[MLogs] = deriveDecoder

  given Decoder[MTransaction] = deriveDecoder
  val transactions: Decoder[MTransactions] = deriveDecoder

  val ints: Decoder[MInts] = deriveDecoder
  val decimals: Decoder[MDecimals] = deriveDecoder

// The protobuf-java walks: `CodedInputStream` reads over the exact wire
// bytes locomotion emits (declaration-order field numbers; strings and
// messages length-delimited; numeric repeats packed).
object protobufWalks:
  import com.google.protobuf.CodedInputStream

  private def stream(data: Data): CodedInputStream =
    CodedInputStream.newInstance(data.mutable(using Unsafe)).nn

  private def walkParam(in: CodedInputStream): MParam =
    var key = ""
    var value = ""

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => key = in.readStringRequireUtf8.nn
        case 2 => value = in.readStringRequireUtf8.nn
        case _ => sys.error("unexpected field")

    MParam(key, value)

  private def delimited[element](in: CodedInputStream)(read: CodedInputStream => element)
  :   element =

    val length = in.readRawVarint32()
    val limit = in.pushLimit(length)
    val result = read(in)
    in.popLimit(limit)
    result

  private def walkServlet(in: CodedInputStream): MServlet =
    var name = ""
    var servletClass = ""
    val params = List.newBuilder[MParam]

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => name = in.readStringRequireUtf8.nn
        case 2 => servletClass = in.readStringRequireUtf8.nn
        case 3 => params += delimited(in)(walkParam)
        case _ => sys.error("unexpected field")

    MServlet(name, servletClass, params.result())

  private def walkMapping(in: CodedInputStream): MMapping =
    var name = ""
    var url = ""

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => name = in.readStringRequireUtf8.nn
        case 2 => url = in.readStringRequireUtf8.nn
        case _ => sys.error("unexpected field")

    MMapping(name, url)

  private def walkTaglib(in: CodedInputStream): MTaglib =
    var uri = ""
    var location = ""

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => uri = in.readStringRequireUtf8.nn
        case 2 => location = in.readStringRequireUtf8.nn
        case _ => sys.error("unexpected field")

    MTaglib(uri, location)

  private def walkWebApp(in: CodedInputStream): MWebApp =
    val servlets = List.newBuilder[MServlet]
    val mappings = List.newBuilder[MMapping]
    var taglib = MTaglib("", "")

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => servlets += delimited(in)(walkServlet)
        case 2 => mappings += delimited(in)(walkMapping)
        case 3 => taglib = delimited(in)(walkTaglib)
        case _ => sys.error("unexpected field")

    MWebApp(servlets.result(), mappings.result(), taglib)

  def config(data: Data): MConfig =
    val in = stream(data)
    var webApp = MWebApp(Nil, Nil, MTaglib("", ""))

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => webApp = delimited(in)(walkWebApp)
        case _ => sys.error("unexpected field")

    MConfig(webApp)

  private def walkMenuItem(in: CodedInputStream): MMenuItem =
    var value = ""
    var onclick = ""

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => value = in.readStringRequireUtf8.nn
        case 2 => onclick = in.readStringRequireUtf8.nn
        case _ => sys.error("unexpected field")

    MMenuItem(value, onclick)

  private def walkPopup(in: CodedInputStream): MPopup =
    val items = List.newBuilder[MMenuItem]

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => items += delimited(in)(walkMenuItem)
        case _ => sys.error("unexpected field")

    MPopup(items.result())

  private def walkMenu(in: CodedInputStream): MMenu =
    var id = ""
    var value = ""
    var popup = MPopup(Nil)

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => id = in.readStringRequireUtf8.nn
        case 2 => value = in.readStringRequireUtf8.nn
        case 3 => popup = delimited(in)(walkPopup)
        case _ => sys.error("unexpected field")

    MMenu(id, value, popup)

  def menu(data: Data): MMenuDoc =
    val in = stream(data)
    var menu = MMenu("", "", MPopup(Nil))

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => menu = delimited(in)(walkMenu)
        case _ => sys.error("unexpected field")

    MMenuDoc(menu)

  private def walkUser(in: CodedInputStream): MUser =
    var id = 0
    var username = ""
    var email = ""
    var active = false
    var role = ""

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => id = in.readInt32()
        case 2 => username = in.readStringRequireUtf8.nn
        case 3 => email = in.readStringRequireUtf8.nn
        case 4 => active = in.readBool()
        case 5 => role = in.readStringRequireUtf8.nn
        case _ => sys.error("unexpected field")

    MUser(id, username, email, active, role)

  def users(data: Data): MUsers =
    val in = stream(data)
    val users = List.newBuilder[MUser]

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => users += delimited(in)(walkUser)
        case _ => sys.error("unexpected field")

    MUsers(users.result())

  private def walkLogEntry(in: CodedInputStream): MLogEntry =
    var timestamp = 0L
    var level = ""
    var service = ""
    var requestId = ""
    var userId = 0
    var message = ""

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => timestamp = in.readInt64()
        case 2 => level = in.readStringRequireUtf8.nn
        case 3 => service = in.readStringRequireUtf8.nn
        case 4 => requestId = in.readStringRequireUtf8.nn
        case 5 => userId = in.readInt32()
        case 6 => message = in.readStringRequireUtf8.nn
        case _ => sys.error("unexpected field")

    MLogEntry(timestamp, level, service, requestId, userId, message)

  def logs(data: Data): MLogs =
    val in = stream(data)
    val logs = List.newBuilder[MLogEntry]

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => logs += delimited(in)(walkLogEntry)
        case _ => sys.error("unexpected field")

    MLogs(logs.result())

  private def walkTransaction(in: CodedInputStream): MTransaction =
    var from = ""
    var to = ""
    var valueWei = ""
    var valueEth = ""
    var gasPriceWei = ""
    var gasUsed = 0
    var blockNumber = 0L
    var nonce = 0
    var temperatureDelta = 0.0

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => from = in.readStringRequireUtf8.nn
        case 2 => to = in.readStringRequireUtf8.nn
        case 3 => valueWei = in.readStringRequireUtf8.nn
        case 4 => valueEth = in.readStringRequireUtf8.nn
        case 5 => gasPriceWei = in.readStringRequireUtf8.nn
        case 6 => gasUsed = in.readInt32()
        case 7 => blockNumber = in.readInt64()
        case 8 => nonce = in.readInt32()
        case 9 => temperatureDelta = in.readDouble()
        case _ => sys.error("unexpected field")

    MTransaction
      ( from, to, valueWei, valueEth, gasPriceWei, gasUsed, blockNumber, nonce,
        temperatureDelta )

  def transactions(data: Data): MTransactions =
    val in = stream(data)
    val transactions = List.newBuilder[MTransaction]

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 => transactions += delimited(in)(walkTransaction)
        case _ => sys.error("unexpected field")

    MTransactions(transactions.result())

  def ints(data: Data): MInts =
    val in = stream(data)
    val values = List.newBuilder[Int]

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 =>
          val length = in.readRawVarint32()
          val limit = in.pushLimit(length)
          while in.getBytesUntilLimit > 0 do values += in.readInt32()
          in.popLimit(limit)

        case _ =>
          sys.error("unexpected field")

    MInts(values.result())

  def decimals(data: Data): MDecimals =
    val in = stream(data)
    val values = List.newBuilder[Double]

    while !in.isAtEnd do
      in.readTag() >>> 3 match
        case 1 =>
          val length = in.readRawVarint32()
          val limit = in.pushLimit(length)
          while in.getBytesUntilLimit > 0 do values += in.readDouble()
          in.popLimit(limit)

        case _ =>
          sys.error("unexpected field")

    MDecimals(values.result())

// The snakeyaml-engine mappers: the reference YAML implementation composes
// to native collections, then each document maps by hand.
object snakeyamlWalks:
  import org.snakeyaml.engine.v2.api.{Load, LoadSettings}

  private def load(text: String): AnyRef =
    Load(LoadSettings.builder.nn.build.nn).loadFromString(text).nn.asInstanceOf[AnyRef]

  private def mapping(node: AnyRef): java.util.Map[String, AnyRef] =
    node.asInstanceOf[java.util.Map[String, AnyRef]]

  private def sequence(node: AnyRef): List[AnyRef] =
    val list = node.asInstanceOf[java.util.List[AnyRef]]
    val builder = List.newBuilder[AnyRef]
    val iterator = list.iterator.nn
    while iterator.hasNext do builder += iterator.next.nn
    builder.result()

  private def string(node: AnyRef): String = node.asInstanceOf[String]
  private def int(node: AnyRef): Int = node.asInstanceOf[Number].intValue
  private def long(node: AnyRef): Long = node.asInstanceOf[Number].longValue
  private def double(node: AnyRef): Double = node.asInstanceOf[Number].doubleValue
  private def boolean(node: AnyRef): Boolean = node.asInstanceOf[java.lang.Boolean].booleanValue

  private def field(node: java.util.Map[String, AnyRef], name: String): AnyRef =
    node.get(name).nn

  def config(text: String): MConfig =
    val root = mapping(load(text))
    val webApp = mapping(field(root, "webApp"))

    MConfig
      ( MWebApp
          ( sequence(field(webApp, "servlets")).map: node =>
              val servlet = mapping(node)

              MServlet
                ( string(field(servlet, "servletName")),
                  string(field(servlet, "servletClass")),
                  sequence(field(servlet, "params")).map: paramNode =>
                    val param = mapping(paramNode)
                    MParam(string(field(param, "key")), string(field(param, "value"))) ),
            sequence(field(webApp, "mappings")).map: node =>
              val mapped = mapping(node)
              MMapping(string(field(mapped, "servletName")), string(field(mapped, "url"))),
            {
              val taglib = mapping(field(webApp, "taglib"))
              MTaglib(string(field(taglib, "uri")), string(field(taglib, "location")))
            } ) )

  def menu(text: String): MMenuDoc =
    val root = mapping(load(text))
    val menu = mapping(field(root, "menu"))
    val popup = mapping(field(menu, "popup"))

    MMenuDoc
      ( MMenu
          ( string(field(menu, "id")), string(field(menu, "value")),
            MPopup
              ( sequence(field(popup, "menuitems")).map: node =>
                  val item = mapping(node)
                  MMenuItem(string(field(item, "value")), string(field(item, "onclick"))) ) ) )

  def users(text: String): MUsers =
    val root = mapping(load(text))

    MUsers
      ( sequence(field(root, "users")).map: node =>
          val user = mapping(node)

          MUser
            ( int(field(user, "id")), string(field(user, "username")),
              string(field(user, "email")), boolean(field(user, "active")),
              string(field(user, "role")) ) )

  def logs(text: String): MLogs =
    val root = mapping(load(text))

    MLogs
      ( sequence(field(root, "logs")).map: node =>
          val entry = mapping(node)

          MLogEntry
            ( long(field(entry, "timestamp")), string(field(entry, "level")),
              string(field(entry, "service")), string(field(entry, "requestId")),
              int(field(entry, "userId")), string(field(entry, "message")) ) )

  def transactions(text: String): MTransactions =
    val root = mapping(load(text))

    MTransactions
      ( sequence(field(root, "transactions")).map: node =>
          val transaction = mapping(node)

          MTransaction
            ( string(field(transaction, "from")), string(field(transaction, "to")),
              string(field(transaction, "valueWei")), string(field(transaction, "valueEth")),
              string(field(transaction, "gasPriceWei")), int(field(transaction, "gasUsed")),
              long(field(transaction, "blockNumber")), int(field(transaction, "nonce")),
              double(field(transaction, "temperatureDelta")) ) )

  def ints(text: String): MInts =
    val root = mapping(load(text))
    MInts(sequence(field(root, "values")).map(int))

  def decimals(text: String): MDecimals =
    val root = mapping(load(text))
    MDecimals(sequence(field(root, "values")).map(double))

// The Aalto walks: a hand-written StAX pull loop per document — the
// fastest available use of the reference-quality XML parser. Xylophone's
// encoding names the root element after the type and repeats field-named
// elements for collections, so each record reader dispatches on local
// names until its end tag.
object aaltoWalks:
  import javax.xml.stream.XMLStreamConstants.START_ELEMENT
  import org.codehaus.stax2.XMLStreamReader2

  private val factory = com.fasterxml.aalto.stax.InputFactoryImpl()

  private def reader(text: String): XMLStreamReader2 =
    factory.createXMLStreamReader(java.io.StringReader(text)).nn
    . asInstanceOf[XMLStreamReader2]

  private def text(in: XMLStreamReader2): String = in.getElementText.nn

  private def walkParam(in: XMLStreamReader2): MParam =
    var key = ""
    var value = ""

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "key"   => key = text(in)
        case "value" => value = text(in)
        case other   => sys.error("unexpected element: "+other)

    MParam(key, value)

  private def walkServlet(in: XMLStreamReader2): MServlet =
    var name = ""
    var servletClass = ""
    val params = List.newBuilder[MParam]

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "servletName"  => name = text(in)
        case "servletClass" => servletClass = text(in)
        case "params"       => params += walkParam(in)
        case other          => sys.error("unexpected element: "+other)

    MServlet(name, servletClass, params.result())

  private def walkMapping(in: XMLStreamReader2): MMapping =
    var name = ""
    var url = ""

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "servletName" => name = text(in)
        case "url"         => url = text(in)
        case other         => sys.error("unexpected element: "+other)

    MMapping(name, url)

  private def walkTaglib(in: XMLStreamReader2): MTaglib =
    var uri = ""
    var location = ""

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "uri"      => uri = text(in)
        case "location" => location = text(in)
        case other      => sys.error("unexpected element: "+other)

    MTaglib(uri, location)

  private def walkWebApp(in: XMLStreamReader2): MWebApp =
    val servlets = List.newBuilder[MServlet]
    val mappings = List.newBuilder[MMapping]
    var taglib = MTaglib("", "")

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "servlets" => servlets += walkServlet(in)
        case "mappings" => mappings += walkMapping(in)
        case "taglib"   => taglib = walkTaglib(in)
        case other      => sys.error("unexpected element: "+other)

    MWebApp(servlets.result(), mappings.result(), taglib)

  def config(input: String): MConfig =
    val in = reader(input)
    in.nextTag()
    var webApp = MWebApp(Nil, Nil, MTaglib("", ""))

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "webApp" => webApp = walkWebApp(in)
        case other    => sys.error("unexpected element: "+other)

    MConfig(webApp)

  private def walkMenuItem(in: XMLStreamReader2): MMenuItem =
    var value = ""
    var onclick = ""

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "value"   => value = text(in)
        case "onclick" => onclick = text(in)
        case other     => sys.error("unexpected element: "+other)

    MMenuItem(value, onclick)

  private def walkPopup(in: XMLStreamReader2): MPopup =
    val items = List.newBuilder[MMenuItem]

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "menuitems" => items += walkMenuItem(in)
        case other       => sys.error("unexpected element: "+other)

    MPopup(items.result())

  private def walkMenu(in: XMLStreamReader2): MMenu =
    var id = ""
    var value = ""
    var popup = MPopup(Nil)

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "id"    => id = text(in)
        case "value" => value = text(in)
        case "popup" => popup = walkPopup(in)
        case other   => sys.error("unexpected element: "+other)

    MMenu(id, value, popup)

  def menu(input: String): MMenuDoc =
    val in = reader(input)
    in.nextTag()
    var menu = MMenu("", "", MPopup(Nil))

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "menu" => menu = walkMenu(in)
        case other  => sys.error("unexpected element: "+other)

    MMenuDoc(menu)

  private def walkUser(in: XMLStreamReader2): MUser =
    var id = 0
    var username = ""
    var email = ""
    var active = false
    var role = ""

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "id"       => id = text(in).toInt
        case "username" => username = text(in)
        case "email"    => email = text(in)
        case "active"   => active = text(in).toBoolean
        case "role"     => role = text(in)
        case other      => sys.error("unexpected element: "+other)

    MUser(id, username, email, active, role)

  def users(input: String): MUsers =
    val in = reader(input)
    in.nextTag()
    val users = List.newBuilder[MUser]

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "users" => users += walkUser(in)
        case other   => sys.error("unexpected element: "+other)

    MUsers(users.result())

  private def walkLogEntry(in: XMLStreamReader2): MLogEntry =
    var timestamp = 0L
    var level = ""
    var service = ""
    var requestId = ""
    var userId = 0
    var message = ""

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "timestamp" => timestamp = text(in).toLong
        case "level"     => level = text(in)
        case "service"   => service = text(in)
        case "requestId" => requestId = text(in)
        case "userId"    => userId = text(in).toInt
        case "message"   => message = text(in)
        case other       => sys.error("unexpected element: "+other)

    MLogEntry(timestamp, level, service, requestId, userId, message)

  def logs(input: String): MLogs =
    val in = reader(input)
    in.nextTag()
    val logs = List.newBuilder[MLogEntry]

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "logs" => logs += walkLogEntry(in)
        case other  => sys.error("unexpected element: "+other)

    MLogs(logs.result())

  private def walkTransaction(in: XMLStreamReader2): MTransaction =
    var from = ""
    var to = ""
    var valueWei = ""
    var valueEth = ""
    var gasPriceWei = ""
    var gasUsed = 0
    var blockNumber = 0L
    var nonce = 0
    var temperatureDelta = 0.0

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "from"             => from = text(in)
        case "to"               => to = text(in)
        case "valueWei"         => valueWei = text(in)
        case "valueEth"         => valueEth = text(in)
        case "gasPriceWei"      => gasPriceWei = text(in)
        case "gasUsed"          => gasUsed = text(in).toInt
        case "blockNumber"      => blockNumber = text(in).toLong
        case "nonce"            => nonce = text(in).toInt
        case "temperatureDelta" => temperatureDelta = text(in).toDouble
        case other              => sys.error("unexpected element: "+other)

    MTransaction
      ( from, to, valueWei, valueEth, gasPriceWei, gasUsed, blockNumber, nonce,
        temperatureDelta )

  def transactions(input: String): MTransactions =
    val in = reader(input)
    in.nextTag()
    val transactions = List.newBuilder[MTransaction]

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "transactions" => transactions += walkTransaction(in)
        case other          => sys.error("unexpected element: "+other)

    MTransactions(transactions.result())

  def ints(input: String): MInts =
    val in = reader(input)
    in.nextTag()
    val values = List.newBuilder[Int]

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "values" => values += text(in).toInt
        case other    => sys.error("unexpected element: "+other)

    MInts(values.result())

  def decimals(input: String): MDecimals =
    val in = reader(input)
    in.nextTag()
    val values = List.newBuilder[Double]

    while in.nextTag() == START_ELEMENT do
      in.getLocalName match
        case "values" => values += text(in).toDouble
        case other    => sys.error("unexpected element: "+other)

    MDecimals(values.result())
