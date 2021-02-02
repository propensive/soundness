package example

import cosmopolite._, languages.{En, Fr, De}

// We define a global type listing the languages we need to support
type AudienceLang = En | Fr | De

object Server:
  def handle(request: Request): Response =
    // We can parse a string to get one of our target languages
    val lang: Option[Language[AudienceLang]] = Language.parse[AudienceLang](request.language.toLowerCase)

    // We can define a multilingual string for our languages
    // Providing more languages than required is not a problem, but omitting one is a compile error
    // If we included the same language *twice* in the same `Messages`, it would also be a compile error
    val title: Messages[AudienceLang] = en"Welcome!" & fr"Bienvenue!" & de"Willkommen!" & es"Bienvenidas!"


    // We can get a message in a specific language by applying that language as a type parameter
    log.info(s"The ${title[En]} page was requested")

    // We can make a language given in the rest of the scope
    given Language[AudienceLang] = lang.getOrElse(Language[En])

    // We just apply empty parentheses to the `title` to get a string in the given language
    Response(title())
end Server


case class Request(language: String, user: String)
case class Response(content: String)

object log:
  def info(msg: String): Unit = println(msg)

