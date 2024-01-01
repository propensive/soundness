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

type Metadata = "base" | "link" | "meta" | "noscript" | "script" | "style" | "template" | "title"

type Phrasing = "a" | "abbr" | "area" | "audio" | "b" | "bdi" | "bdo" | "br" | "button" | "canvas" |
    "cite" | "code" | "data" | "datalist" | "del" | "dfn" | "em" | "embed" | "i" | "iframe" |
    "img" | "input" | "ins" | "kbd" | "label" | "link" | "hmap" | "mark" | "meta" | "meter" |
    "noscript" | "object" | "output" | "picture" | "progress" | "q" | "ruby" | "s" | "samp" |
    "script" | "select" | "slot" | "small" | "span" | "strong" | "sub" | "sup" | "template" |
    "textarea" | "time" | "u" | "var" | "video" | "wbr"

type Flow = "a" | "abbr" | "address" | "area" | "article" | "aside" | "audio" | "b" | "bdi" |
    "bdo" | "blockquote" | "br" | "button" | "canvas" | "cite" | "code" | "data" | "datalist" |
    "del" | "details" | "dfn" | "dialog" | "div" | "dl" | "em" | "embed" | "fieldset" | "figure" |
    "footer" | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "header" | "hgroup" | "hr" | "i" |
    "iframe" | "img" | "input" | "ins" | "kbd" | "label" | "link" | "main" | "hmap" | "mark" |
    "menu" | "meta" | "meter" | "nav" | "noscript" | "object" | "ol" | "output" | "p" | "picture" |
    "pre" | "progress" | "q" | "ruby" | "s" | "samp" | "script" | "section" | "select" | "slot" |
    "small" | "span" | "strong" | "sub" | "sup" | "table" | "template" | "textarea" | "time" | "u" |
    "ul" | "var" | "video" | "wbr"

type Sectioning = "article" | "aside" | "nav" | "section"

type Heading = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hgroup"

type Embedded = "audio" | "canvas" | "embed" | "iframe" | "img" | "object" | "picture" | "video"

type Interactive = "a" | "audio" | "button" | "details" | "embed" | "iframe" | "img" | "input" |
    "label" | "select" | "textarea" | "video"

type Palpable = "a" | "abbr" | "address" | "article" | "aside" | "audio" | "b" | "bdi" | "bdo" |
    "blockquote" | "button" | "canvas" | "cite" | "code" | "data" | "details" | "dfn" | "div" |
    "dl" | "em" | "embed" | "fieldset" | "figure" | "footer" | "form" | "h1" | "h2" | "h3" | "h4" |
    "h5" | "h6" | "header" | "hgroup" | "i" | "iframe" | "img" | "input" | "ins" | "kbd" | "label" |
    "main" | "hmap" | "mark" | "menu" | "meter" | "nav" | "object" | "ol" | "output" | "p" | "pre" |
    "progress" | "q" | "ruby" | "s" | "samp" | "section" | "select" | "small" | "span" | "strong" |
    "sub" | "sup" | "table" | "textarea" | "time" | "u" | "ul" | "var" | "video"

type ScriptSupporting = "script" | "template"

type NonInteractive = "abbr" | "address" | "area" | "article" | "aside" | "audio" | "b" | "base" |
    "bdi" | "bdo" | "blockquote" | "br" | "canvas" | "cite" | "code" | "data" | "datalist" | "del" |
    "dfn" | "dialog" | "div" | "dl" | "em" | "fieldset" | "figure" | "footer" | "form" | "h1" |
    "h2" | "h3" | "h4" | "h5" | "h6" | "header" | "hgroup" | "hr" | "i" | "img" | "input" | "ins" |
    "kbd" | "link" | "main" | "hmap" | "mark" | "menu" | "meta" | "meter" | "nav" | "noscript" |
    "object" | "ol" | "output" | "p" | "picture" | "pre" | "progress" | "q" | "ruby" | "s" |
    "samp" | "script" | "section" | "slot" | "small" | "span" | "strong" | "style" | "sub" | "sup" |
    "table" | "template" | "time" | "title" | "u" | "ul" | "var" | "wbr"

type Global = "accesskey" | "autocapitalize" | "autofocus" | "contenteditable" | "dir" |
    "draggable" | "enterkeyhint" | "hclass" | "hidden" | "id" | "inputmode" | "is" | "itemid" |
    "itemprop" | "itemref" | "itemscope" | "itemtype" | "lang" | "nonce" | "spellcheck" | "style" |
    "tabindex" | "title" | "translate" | EventHandlers

type EventHandlers = "onabort" | "onauxclick" | "oncancel" | "oncanplay" | "oncanplaythrough" |
    "onchange" | "onclick" | "onclos" | "oncontextmenu" | "oncuechange" | "ondblclick" | "ondrag" |
    "ondragend" | "ondragenter" | "ondragleave" | "ondragover" | "ondragstart" | "ondrop" |
    "ondurationchange" | "onemptied" | "onended" | "onformdata" | "oninput" | "oninvalid" |
    "onkeydown" | "onkeypress" | "onkeyup" | "onloadeddata" | "onloadedmetadata" | "onloadstart" |
    "onmousedown" | "onmouseenter" | "onmouseleave" | "onmousemove" | "onmouseout" | "onmouseover" |
    "onmouseup" | "onpause" | "onplay" | "onplaying" | "onprogress" | "onratechange" | "onreset" |
    "onsecuritypolicyviolation" | "onseeked" | "onseeking" | "onselect" | "onslotchange" |
    "onstalled" | "onsubmit" | "onsuspend" | "ontimeupdate" | "ontoggle" | "onvolumechange" |
    "onwaiting" | "onwebkitanimationend" | "onwebkitanimationiteration" | "onwebkitanimationstart" |
    "onwebkittransitionend" | "onwheel" | "onblur" | "onerror" | "onfocus" | "onload" | "onresize" |
    "onscroll"
