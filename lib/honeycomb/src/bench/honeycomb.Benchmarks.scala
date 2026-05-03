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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package honeycomb

import scala.quoted.*

import ambience.*, environments.java, systems.java
import anticipation.*
import contingency.*, strategies.throwUnsafely
import doms.html.whatwg
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContext
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.system
import turbulence.*
import vacuous.*

object Benchmarks extends Suite(m"Honeycomb benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice

  // Auto-scale byte sizes (B → kB → MB → GB → TB) and byte rates so the
  // table prints "1.3 MB·s¯¹" instead of "1.3×10⁶ B·s¯¹".
  given prefixes: Prefixes = Prefixes(List(Kilo, Mega, Giga, Tera))

  def parseHoneycomb(text: Text): Document[Html] = unsafely(text.load[Html])

  def parseJsoup(text: String): org.jsoup.nodes.Document = org.jsoup.Jsoup.parse(text).nn

  def run(): Unit =
    val bench = Bench()

    val size1 = htmlText1.getBytes("UTF-8").nn.length*Byte
    val size2 = htmlText2.getBytes("UTF-8").nn.length*Byte
    val size3 = htmlText3.getBytes("UTF-8").nn.length*Byte
    val size4 = htmlText4.getBytes("UTF-8").nn.length*Byte
    val size5 = htmlText5.getBytes("UTF-8").nn.length*Byte

    suite(m"Parse example 1 (small page)"):
      bench(m"Parse file with Honeycomb")
        (target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min)):
        '{ honeycomb.Benchmarks.parseHoneycomb(honeycomb.Benchmarks.html1) }

      bench(m"Parse file with jsoup")(target = 1*Second, operationSize = size1):
        '{ honeycomb.Benchmarks.parseJsoup(honeycomb.Benchmarks.htmlText1) }

    suite(m"Parse example 2 (typical layout)"):
      bench(m"Parse file with Honeycomb")
        (target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min)):
        '{ honeycomb.Benchmarks.parseHoneycomb(honeycomb.Benchmarks.html2) }

      bench(m"Parse file with jsoup")(target = 1*Second, operationSize = size2):
        '{ honeycomb.Benchmarks.parseJsoup(honeycomb.Benchmarks.htmlText2) }

    suite(m"Parse example 3 (rich article)"):
      bench(m"Parse file with Honeycomb")
        (target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min)):
        '{ honeycomb.Benchmarks.parseHoneycomb(honeycomb.Benchmarks.html3) }

      bench(m"Parse file with jsoup")(target = 1*Second, operationSize = size3):
        '{ honeycomb.Benchmarks.parseJsoup(honeycomb.Benchmarks.htmlText3) }

    suite(m"Parse example 4 (100 product cards)"):
      bench(m"Parse file with Honeycomb")
        (target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min)):
        '{ honeycomb.Benchmarks.parseHoneycomb(honeycomb.Benchmarks.html4) }

      bench(m"Parse file with jsoup")(target = 1*Second, operationSize = size4):
        '{ honeycomb.Benchmarks.parseJsoup(honeycomb.Benchmarks.htmlText4) }

    suite(m"Parse example 5 (500 table rows)"):
      bench(m"Parse file with Honeycomb")
        (target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min)):
        '{ honeycomb.Benchmarks.parseHoneycomb(honeycomb.Benchmarks.html5) }

      bench(m"Parse file with jsoup")(target = 1*Second, operationSize = size5):
        '{ honeycomb.Benchmarks.parseJsoup(honeycomb.Benchmarks.htmlText5) }

  lazy val html1: Text = htmlExample1.trim
  lazy val html2: Text = htmlExample2.trim
  lazy val html3: Text = htmlExample3.trim
  lazy val htmlText1: String = html1.s
  lazy val htmlText2: String = html2.s
  lazy val htmlText3: String = html3.s

  // Example 4: 100 product cards — a typical "list of records" layout with a
  // small fixed structure repeated across all elements.
  lazy val htmlText4: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("<!doctype html><html><head><title>Catalog</title></head><body>")
    sb.append("<main class=\"catalog\"><h1>Products</h1><ul class=\"products\">")
    var i = 0
    while i < 100 do
      sb.append("<li class=\"product\" id=\"p")
      sb.append(i)
      sb.append("\"><img src=\"/img/p")
      sb.append(i)
      sb.append(".jpg\" alt=\"Product ")
      sb.append(i)
      sb.append("\"><h2><a href=\"/products/")
      sb.append(i)
      sb.append("\">Product ")
      sb.append(i)
      sb.append("</a></h2><p class=\"desc\">A high-quality item, number ")
      sb.append(i)
      sb.append(" in the range, suitable for everyday use.</p>")
      sb.append("<span class=\"price\">$")
      sb.append(10 + (i % 90))
      sb.append(".99</span></li>")
      i += 1
    sb.append("</ul></main></body></html>")
    sb.toString

  lazy val html4: Text = htmlText4.tt

  // Example 5: a longer table with 500 rows; 6 cells per row, exercising the
  // table foster-parenting and per-cell tokenization paths.
  lazy val htmlText5: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("<!doctype html><html><head><title>Logs</title></head><body>")
    sb.append("<table><thead><tr><th>Timestamp</th><th>Level</th>")
    sb.append("<th>Service</th><th>Request</th><th>User</th><th>Message</th>")
    sb.append("</tr></thead><tbody>")
    val levels = Array("info", "debug", "warn", "error")
    val services = Array("auth", "api", "db", "cache", "worker")
    var i = 0
    while i < 500 do
      val ts = 1700000000L + i
      val level = levels(i & 3)
      val service = services(i % 5)
      val userId = 1000 + (i % 50)
      sb.append("<tr class=\"row-")
      sb.append(level)
      sb.append("\"><td>")
      sb.append(ts)
      sb.append("</td><td><span class=\"level\">")
      sb.append(level)
      sb.append("</span></td><td>")
      sb.append(service)
      sb.append("</td><td><code>req-")
      sb.append(i)
      sb.append("</code></td><td>")
      sb.append(userId)
      sb.append("</td><td>event ")
      sb.append(i)
      sb.append(" processed</td></tr>")
      i += 1
    sb.append("</tbody></table></body></html>")
    sb.toString

  lazy val html5: Text = htmlText5.tt

  // Example 1: a small, self-contained page — the "hello world" of HTML with
  // a title, a heading and a short paragraph.
  val htmlExample1: Text = t"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Hello, world</title>
</head>
<body>
<h1>Hello, world</h1>
<p>A simple HTML document used to exercise the small-input path of the
parser. It contains a single heading and one short paragraph.</p>
</body>
</html>
"""

  // Example 2: a typical structured page with a nav, header, sidebar and
  // footer — broadly the shape of a marketing or documentation page.
  val htmlExample2: Text = t"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Acme Industries — Widgets, Sprockets &amp; More</title>
<link rel="stylesheet" href="/static/site.css">
<link rel="icon" href="/favicon.ico">
<meta name="description" content="Acme Industries is the original supplier of widgets, sprockets and assorted novelties.">
</head>
<body class="home">
<header class="site-header">
  <div class="brand"><a href="/"><img src="/img/logo.svg" alt="Acme"></a></div>
  <nav class="primary">
    <ul>
      <li><a href="/">Home</a></li>
      <li><a href="/products">Products</a></li>
      <li><a href="/about">About</a></li>
      <li><a href="/contact">Contact</a></li>
    </ul>
  </nav>
</header>
<main class="content">
  <section class="hero">
    <h1>Welcome to Acme</h1>
    <p>For over a century, we&rsquo;ve been the world&rsquo;s leading supplier of
    widgets, sprockets and other essentials of modern life.</p>
    <a class="cta" href="/products">Browse our catalog &rarr;</a>
  </section>
  <section class="features">
    <h2>Why Acme?</h2>
    <ul>
      <li><strong>Quality</strong> &mdash; tested rigorously since 1903.</li>
      <li><strong>Variety</strong> &mdash; over 5,000 products in stock.</li>
      <li><strong>Service</strong> &mdash; same-day shipping on every order.</li>
    </ul>
  </section>
  <aside class="sidebar">
    <h3>Latest news</h3>
    <ol>
      <li><a href="/news/1">New widget line announced</a></li>
      <li><a href="/news/2">Sprocket factory expansion</a></li>
      <li><a href="/news/3">Acme wins industry award</a></li>
    </ol>
  </aside>
</main>
<footer class="site-footer">
  <p>&copy; 1903&ndash;2025 Acme Industries. All rights reserved.</p>
  <p><a href="/privacy">Privacy</a> &middot; <a href="/terms">Terms</a></p>
</footer>
</body>
</html>
"""

  // Example 3: a longer article with a richer mix of inline elements, lists,
  // blockquotes, code blocks and a small table. Exercises text and entity
  // handling across many small text nodes.
  val htmlExample3: Text = t"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>On Parsing HTML — A Field Guide</title>
<link rel="stylesheet" href="/article.css">
</head>
<body>
<article>
<header>
<h1>On Parsing HTML</h1>
<p class="byline">by <a rel="author" href="/authors/jane">Jane Doe</a>,
<time datetime="2024-09-12">12 September 2024</time></p>
</header>
<section>
<h2>Introduction</h2>
<p>HTML is, famously, <em>not</em> a regular language. Its syntax is forgiving
to a fault: browsers will happily render documents that no formal grammar
would accept, and indeed they are <strong>required</strong> to do so by the
WHATWG <a href="https://html.spec.whatwg.org/">specification</a>.</p>
<p>This article gives a brief tour of how an HTML parser works in practice,
covering tokenization, tree construction and the curious phenomenon of
<dfn>foster parenting</dfn>.</p>
</section>
<section>
<h2>Tokenization</h2>
<p>The first stage of parsing turns a stream of characters into a stream of
tokens: <em>start tags</em>, <em>end tags</em>, <em>character data</em>,
<em>comments</em> and <em>doctypes</em>. Consider the following input:</p>
<pre><code>&lt;p class="intro"&gt;Hello, &lt;b&gt;world&lt;/b&gt;!&lt;/p&gt;</code></pre>
<p>This produces six tokens: a <code>&lt;p&gt;</code> start tag (with one
attribute), three character-data tokens, a <code>&lt;b&gt;</code> start tag,
a <code>&lt;/b&gt;</code> end tag, more character data, and a <code>&lt;/p&gt;</code>
end tag.</p>
<blockquote>
<p>&ldquo;Parsing HTML is one of those problems that is easy in 90% of cases
and surprisingly hard in the remaining 10%.&rdquo;</p>
<footer>&mdash; <cite>Anonymous browser engineer</cite></footer>
</blockquote>
</section>
<section>
<h2>Tree construction</h2>
<p>Tokens are then assembled into a tree. The tree-construction algorithm is
state-driven and famously intricate; it has at least 23 named insertion
modes, including <em>in body</em>, <em>in table</em>, <em>in select</em>
and <em>after after frameset</em>.</p>
<table>
<caption>Some insertion modes</caption>
<thead>
<tr><th>Mode</th><th>Trigger</th><th>Notes</th></tr>
</thead>
<tbody>
<tr><td>initial</td><td>doctype</td><td>switches to <em>before html</em></td></tr>
<tr><td>in body</td><td>most content</td><td>the workhorse mode</td></tr>
<tr><td>in table</td><td><code>&lt;table&gt;</code></td><td>fosters stray content</td></tr>
<tr><td>in select</td><td><code>&lt;select&gt;</code></td><td>very restricted</td></tr>
</tbody>
</table>
</section>
<section>
<h2>Foster parenting</h2>
<p>Perhaps the most colourful trick in the parser&rsquo;s repertoire is
<dfn>foster parenting</dfn>: when content appears inside a <code>&lt;table&gt;</code>
where it doesn&rsquo;t belong, it is moved out and inserted just before the
table, as though adopted by the table&rsquo;s parent.</p>
<p>This behaviour exists because real-world HTML is full of malformed tables,
and browsers must produce <em>some</em> sensible rendering. As a famous
example:</p>
<pre><code>&lt;table&gt;hello&lt;tr&gt;&lt;td&gt;world&lt;/td&gt;&lt;/tr&gt;&lt;/table&gt;</code></pre>
<p>parses as if the bare text <q>hello</q> appeared <em>before</em> the table,
not inside it.</p>
</section>
<section>
<h2>Conclusion</h2>
<p>HTML parsing is a worthy subject of study not because it is elegant
&mdash; it is decidedly <em>not</em> &mdash; but because it is a model of
engineering pragmatism. The specification reflects two decades of accumulated
real-world experience and is, in its strange way, beautiful.</p>
<p>Further reading:</p>
<ul>
<li><a href="https://html.spec.whatwg.org/">The WHATWG HTML Standard</a></li>
<li><a href="https://github.com/html5lib/html5lib-tests">html5lib-tests</a></li>
<li><a href="/articles/css-parsing">On Parsing CSS</a> &mdash; a companion piece</li>
</ul>
</section>
<footer>
<p>Comments? <a href="mailto:jane@example.com">Email the author</a>.</p>
</footer>
</article>
</body>
</html>
"""
