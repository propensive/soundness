/*
    Honeycomb, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

import probably.*
import gossamer.*
import rudiments.*
import spectacular.*

object Tests extends Suite(t"Honeycomb Tests"):
  def run(): Unit =
    suite(t"Showing HTML"):
      test(t"empty normal tag"):
        Div.show
      .check(_ == t"<div/>")
    
      test(t"empty unclosed tag"):
        Br.show
      .check(_ == t"<br>")
  
      test(t"tag with one attribute"):
        P(id = t"abc").show
      .check(_ == t"""<p id="abc"/>""")
      
      test(t"tag with two attributes"):
        P(id = t"abc", style = t"def").show
      .check(_ == t"""<p id="abc" style="def"/>""")
  
      test(t"unclosed tag with one attribute"):
        Hr(id = t"foo").show
      .check(_ == t"""<hr id="foo">""")
      
      test(t"unclosed tag with two attributes"):
        Hr(id = t"foo", style = t"bar").show
      .check(_ == t"""<hr id="foo" style="bar">""")
      
      test(t"non-self-closing tag"):
        Script.show
      .check(_ == t"""<script></script>""")
  
      test(t"tag with no attributes and children"):
        Div(Hr, Br).show
      .check(_ == t"""<div><hr><br></div>""")
  
      test(t"tag with text child"):
        P(t"hello world").show
      .check(_ == t"<p>hello world</p>")
  
      test(t"tag with mixed children"):
        P(t"hello ", Em(t"world"), t"!").show
      .check(_ == t"<p>hello <em>world</em>!</p>")
  
      test(t"deeper-nested elements"):
        Table(Tbody(Tr(Td(t"A")))).show
      .check(_ == t"<table><tbody><tr><td>A</td></tr></tbody></table>")
