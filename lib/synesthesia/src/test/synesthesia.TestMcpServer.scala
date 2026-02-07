package synesthesia

import anticipation.*
import contingency.*
import honeycomb.*
import gossamer.*
import revolution.*
import turbulence.*
import vacuous.*
import zephyrine.*

object TestMcpServer extends McpServer():
  class Session() extends McpSession
  import Mcp.*

  def initialize(): Session = Session()

  def name: Text = "Pyrus"
  def description: Text = "A simple server"
  def version: Semver = v"1.0.0"
  def prompts: List[Prompt] = Nil

  @tool
  def color(name: Text): Text = "purple"

  @tool
  // @ui("ui://html/content")
  def encodeMagic(text: Text)(using client: McpClient): Text =
    Thread.sleep(1500)
    client.log(t"Searching for $text in the magic book")
    Thread.sleep(1500)

    case class WebsiteUrl(url: Text)
    client.elicit[WebsiteUrl](t"Please provide the URL for your favourite website.")

    client.log(t"Found it in the magic book")
    Thread.sleep(1500)
    client.log(t"Cross-referencing magic recipe")
    Thread.sleep(1500)
    client.log(t"Studying the recipe")
    Thread.sleep(1500)
    client.log(t"Applying the recipe to $text")
    Thread.sleep(1500)
    text.reverse

  @resource("ui://html/content")
  @about("Displays the app user interface")
  @title("User interface")
  def content: Document[Html] =
    import doms.html.whatwg
    import doms.html.whatwg.*
    val html = Html(Head(Title("MCP App")), Body(H1("Hello world")))
    Document(html, doms.html.whatwg)
