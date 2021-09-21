package punctuation

import honeycomb.*

import scala.annotation.targetName

extension (value: Markdown[Markdown.Ast.Node])
  def html: Seq[Content[Flow]] = HtmlConverter().convert(value.nodes)

extension (value: Markdown.Ast.Inline)
  @targetName("html2")
  def html: Seq[Content[Phrasing]] = HtmlConverter().phrasing(value)
