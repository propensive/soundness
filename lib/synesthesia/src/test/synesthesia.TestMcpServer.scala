package synesthesia

import anticipation.*
import gossamer.*
import revolution.*

object TestMcpServer extends McpServer():
  class Session() extends McpSession
  import Mcp.*

  def initialize(): Session =
    println("MCP initialize()")
    Session()

  def name: Text = "Pyrus"
  def description: Text = "A simple server"
  def version: Semver = v"1.0.0"
  def prompts: List[Prompt] = Nil

  @tool
  def color(name: Text): Text = "purple"

  @tool
  def encodeMagic(text: Text)(using client: McpClient): Text =
    Thread.sleep(1500)
    client.log(t"Searching for $text in the magic book")
    Thread.sleep(1500)
    client.log(t"Found it in the magic book")
    Thread.sleep(1500)
    client.log(t"Cross-referencing magic recipe")
    Thread.sleep(1500)
    client.log(t"Studying the recipe")
    Thread.sleep(1500)
    client.log(t"Applying the recipe to $text")
    Thread.sleep(1500)
    text.reverse
