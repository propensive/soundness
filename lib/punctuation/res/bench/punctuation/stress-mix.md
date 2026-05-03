# Stress mix

A synthetic Markdown document exercising every CommonMark feature in roughly
representative proportions. The body of this file is a base section of ~4 KB,
repeated several times below with minor variations so the parser does not
benefit from caching effects on identical input.

[upstream]: https://example.com/upstream "Upstream reference"
[mirror]: https://example.org/mirror
[license]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0"
[ref-image]: /images/diagram.png "A diagram"

## Section 1: prose with mixed inline content

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com "Example") and an
![inline image](/img/example.png "Caption"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license] reference.

Autolinks like <https://commonmark.org> and emails like <mail@example.com> are
also exercised. So is escaping: \*not emphasis\* and \[not a link\] and \\.
Hard line breaks come from a backslash\
or two trailing spaces.  
Soft breaks just wrap
to the next line.

Some HTML inlines: <span>raw</span> and <kbd>Ctrl</kbd>+<kbd>C</kbd>. An entity
reference like &amp; or &#42; or &#x2A; should decode.

## Section 2: headings of every level

# H1 ATX
## H2 ATX
### H3 ATX
#### H4 ATX
##### H5 ATX
###### H6 ATX

Setext H1
=========

Setext H2
---------

## Section 3: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org)

Loose bullet list:

* one paragraph item

  with a continuation paragraph.

* another item with

  > a nested blockquote inside.

* third item with

  ```scala
  val x = 42
  println(x)
  ```

Ordered list with nesting:

1. First level
   1. Second level
      - mixed bullet at third level
      - another bullet
   2. Second level item two
2. First level item two
   1. Nested again
3. First level item three with `inline code` and *emphasis*.

Loose ordered list:

1) item one with paragraph

   continuing here.

2) item two
3) item three

## Section 4: block quotes

> A simple block quote.
> It spans two lines.

> Outer block quote.
>
> > Inner block quote.
> > > Triple-nested block quote with **strong** content.
>
> Back to outer level with a [link][upstream].

## Section 5: code blocks

Fenced code block with info string:

```scala
def parse(text: Text): Markdown of Layout =
  // walk the cursor line by line
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Fenced code block with no info:

```
plain   text   with   spaces
preserved exactly as written
```

Indented code block:

    indented line one
    indented line two
    val x = "with quotes \" and backslashes \\"

Tilde fenced block:

~~~rust
fn main() {
    println!("Hello, world!");
}
~~~

## Section 6: thematic breaks

Above:

---

Star form:

***

Underscore form:

___

## Section 7: emphasis edge cases

Triple emphasis: ***foo bar baz***
Mixed: *foo **bar** baz*
Underscore mid-word: foo_bar_baz (not emphasis)
Asterisk mid-word: foo*bar*baz (is emphasis)
Adjacent runs: ****foo****
Within strong: **foo *bar* baz**
Nested deeply: *a *b *c* d* e*

## Section 8: more references

See also [upstream link][upstream] and the [mirror][] for backups, and consult
the [license] before redistribution. The diagram is shown ![below][ref-image].

> Quoted reference: pull from [upstream] daily.

End of section.

---

# Section repeat 2

[upstream]: https://example.com/upstream "Upstream reference"
[mirror]: https://example.org/mirror

## Section 1 (repeat 2): prose with mixed inline content

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/2 "Example two") and an
![inline image](/img/example2.png "Caption two"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference.

Autolinks like <https://example.org/2> are also exercised. Backslash escapes
\*here\* and \[there\] and \\.

Hard break\
and another  
soft break wrapping
to the next line.

## Section 2 (repeat 2): tight nested lists

- root one
  - child one
    - grand-child one
    - grand-child two with `code`
  - child two with [link](https://example.com)
- root two with **bold**
- root three
  - inner

## Section 3 (repeat 2): block quotes and code

> Outer.
> > Middle.
> > > Inner with *emphasis* and a [link](https://x).

```scala
final class Cursor[data]:
  inline def advance(): Unit = pos += 1
  inline def more: Boolean = pos < writeEnd || moreSlow()
```

    plain indented block
    second line
    third line

## Section 4 (repeat 2): emphasis edge cases

*a*b*c*d*e*
**a**b**c**d**
***a*b*c***
*foo _bar_ baz*
_foo *bar* baz_
**foo *bar baz***

End of repeat.

---

# Section repeat 3

[upstream]: https://example.net/upstream
[mirror]: https://example.net/mirror

Long paragraph one with *italic*, **bold**, ***both***, `code`,
[a link](https://example.com/three), an ![image](/img/three.png),
[ref][upstream], [mirror][], and a final shortcut [upstream]. The paragraph
continues for several lines to give the inline parser a real workout, with
plenty of soft line breaks and the occasional hard break  
right here. Backslash escapes \* \_ \[ \] \( \) \\ \` \! all show.

> Quote with [inline](https://x) and **bold** and `code`.
>
> > Nested quote with *emphasis*.
>
> Back to outer.

## Subsection

1. Numbered with **strong**
2. Another with [link](https://x)
3. Third with `code`

- Bulleted with *italic*
- Another with [reference][upstream]
- Third with autolink <https://example.org>

```python
def fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)
```

    indented again
    second line again

End of section repeat 3.

---

# Section repeat 4

A dense paragraph: *a* *b* *c* **d** **e** **f** ***g*** ***h*** `i` `j` `k`
[a](https://x) [b](https://y) [c](https://z) ![d](/d.png) ![e](/e.png) <https://f>
\* \_ \[ \] \( \) \\ \` \! &amp; &copy; &#x2A; <span>x</span>.

> Quote line one.
> Quote line two with `inline` and *em* and **strong**.
>
> > Inner quote.

- list one
  - nested
    - deep
      - very deep with *emphasis* and `code` and [link](https://x)
- list two
- list three with very long paragraph content that wraps across multiple lines
  to exercise paragraph continuation handling within list items, including
  some `inline code` and [links](https://example.org) along the way.

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

End of section repeat 4.

---

# Section repeat 5

Triple-nested blockquote with mixed content:

> outer
> > middle with *emphasis*
> > > inner with [link](https://x) and `code` and **bold**
> > > and a continuation line.
> > middle continuation.
> outer continuation.

Loose ordered list with code:

1. Step one.

   Continuation paragraph.

   ```scala
   def step1(): Unit = ()
   ```

2. Step two.

   > A blockquote inside.

3. Step three.

Reference resolution stress:

[a]: /url-a
[b]: /url-b "title b"
[c]: /url-c (paren title c)

See [a], [b][], [c][a], and ![image][b].

## Final emphasis stress

*foo*bar*baz*
**foo**bar**baz**
***foo***bar***baz***
_foo_bar_baz_ (no emphasis since underscore mid-word)
__foo__bar__baz__
*a **b** c*
**a *b* c**
***a **b** c***
*a *b *c *d* e* f* g*

End of stress-mix.md.

---

# Section repeat 6

[upstream]: https://example.com/r6
[mirror]: https://example.org/r6

A section to add bulk and ensure the file crosses 50 KB. The body deliberately
mixes features so the parser cannot exploit a narrow distribution: *italic*,
**bold**, ***both***, `code`, [link](https://x), [ref][upstream],
![image](/i.png), <https://example.org>, and HTML <span>inline</span>.

> Quoted bulk content with **strong**, *emphasis*, `code`, and a [link][upstream]
> and a hard break\
> here.

- bulk list item one with *emphasis*
- bulk list item two with `code`
- bulk list item three with [link](https://x)
  - nested with **bold**
  - nested with ![image](/i.png)
- bulk list item four

```scala
object Bench:
  def run(): Unit =
    println("running")
```

    indented bulk
    second line
    third line

# Section repeat 7

[upstream]: https://example.com/r7

Another bulk section with full feature mix.

1. Ordered bulk one with *italic*
2. Ordered bulk two with **bold**
3. Ordered bulk three with `code`
4. Ordered bulk four with [link](https://x)
5. Ordered bulk five with [ref][upstream]

> Bulk quote one.
> Bulk quote two with `inline`.
>
> > Nested bulk quote.

```rust
fn bulk() -> i32 {
    42
}
```

End of bulk content.

# Section repeat 8

Ten paragraphs of mixed inline content follow.

Paragraph 1: *a* **b** ***c*** `d` [e](https://e) ![f](/f.png) <https://g>.
Paragraph 2: *foo* and **bar** and ***baz*** with `code` and [link](https://x).
Paragraph 3: backslash escapes \* \_ \[ \] \\ and entities &amp; &#42;.
Paragraph 4: HTML inlines <em>e</em> <strong>s</strong> <code>c</code>.
Paragraph 5: hard break\
on next line and soft  
break here.
Paragraph 6: long mixed prose with *italic*, **bold**, `code`, and a
[link with title](https://example.com "title"), all on one paragraph.
Paragraph 7: emphasis edge ***foo*bar*baz***.
Paragraph 8: emphasis edge *foo **bar** baz*.
Paragraph 9: emphasis edge **foo *bar* baz**.
Paragraph 10: final paragraph with [ref-link][upstream].

[upstream]: https://example.com/r8

End of stress-mix.md.

---

# Section repeat 9

[upstream]: https://example.com/r9
[mirror]: https://example.org/r9
[license-r9]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r9"

## Subsection 9.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/9 "Example 9") and an
![inline image](/img/example9.png "Caption 9"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r9] reference.

Autolinks like <https://commonmark.org/9> and emails like <mail9@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 9</span> <kbd>Ctrl</kbd>+<kbd>9</kbd>.

## Subsection 9.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/9)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x9 = 9
  println(x9)
  ```

Nested ordered list:

1. First level 9
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep9)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 9.3: block quotes

> A simple block quote 9.
> It spans two lines.

> Outer block quote 9.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 9.4: code blocks

Fenced code block:

```scala
def parse9(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (9)
    indented line two
    val y9 = "with quotes \" and backslashes \\"

## Subsection 9.5: emphasis edge cases

Triple emphasis: ***foo 9 bar baz***
Mixed: *foo **bar 9** baz*
Adjacent: ****foo 9****
Nested: *a *b *c 9* d* e*

## Subsection 9.6: more references

See [upstream link][upstream], [mirror][], and [license-r9].

End of section repeat 9.

---

# Section repeat 10

[upstream]: https://example.com/r10
[mirror]: https://example.org/r10
[license-r10]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r10"

## Subsection 10.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/10 "Example 10") and an
![inline image](/img/example10.png "Caption 10"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r10] reference.

Autolinks like <https://commonmark.org/10> and emails like <mail10@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 10</span> <kbd>Ctrl</kbd>+<kbd>10</kbd>.

## Subsection 10.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/10)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x10 = 10
  println(x10)
  ```

Nested ordered list:

1. First level 10
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep10)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 10.3: block quotes

> A simple block quote 10.
> It spans two lines.

> Outer block quote 10.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 10.4: code blocks

Fenced code block:

```scala
def parse10(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (10)
    indented line two
    val y10 = "with quotes \" and backslashes \\"

## Subsection 10.5: emphasis edge cases

Triple emphasis: ***foo 10 bar baz***
Mixed: *foo **bar 10** baz*
Adjacent: ****foo 10****
Nested: *a *b *c 10* d* e*

## Subsection 10.6: more references

See [upstream link][upstream], [mirror][], and [license-r10].

End of section repeat 10.

---

# Section repeat 11

[upstream]: https://example.com/r11
[mirror]: https://example.org/r11
[license-r11]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r11"

## Subsection 11.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/11 "Example 11") and an
![inline image](/img/example11.png "Caption 11"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r11] reference.

Autolinks like <https://commonmark.org/11> and emails like <mail11@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 11</span> <kbd>Ctrl</kbd>+<kbd>11</kbd>.

## Subsection 11.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/11)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x11 = 11
  println(x11)
  ```

Nested ordered list:

1. First level 11
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep11)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 11.3: block quotes

> A simple block quote 11.
> It spans two lines.

> Outer block quote 11.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 11.4: code blocks

Fenced code block:

```scala
def parse11(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (11)
    indented line two
    val y11 = "with quotes \" and backslashes \\"

## Subsection 11.5: emphasis edge cases

Triple emphasis: ***foo 11 bar baz***
Mixed: *foo **bar 11** baz*
Adjacent: ****foo 11****
Nested: *a *b *c 11* d* e*

## Subsection 11.6: more references

See [upstream link][upstream], [mirror][], and [license-r11].

End of section repeat 11.

---

# Section repeat 12

[upstream]: https://example.com/r12
[mirror]: https://example.org/r12
[license-r12]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r12"

## Subsection 12.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/12 "Example 12") and an
![inline image](/img/example12.png "Caption 12"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r12] reference.

Autolinks like <https://commonmark.org/12> and emails like <mail12@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 12</span> <kbd>Ctrl</kbd>+<kbd>12</kbd>.

## Subsection 12.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/12)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x12 = 12
  println(x12)
  ```

Nested ordered list:

1. First level 12
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep12)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 12.3: block quotes

> A simple block quote 12.
> It spans two lines.

> Outer block quote 12.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 12.4: code blocks

Fenced code block:

```scala
def parse12(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (12)
    indented line two
    val y12 = "with quotes \" and backslashes \\"

## Subsection 12.5: emphasis edge cases

Triple emphasis: ***foo 12 bar baz***
Mixed: *foo **bar 12** baz*
Adjacent: ****foo 12****
Nested: *a *b *c 12* d* e*

## Subsection 12.6: more references

See [upstream link][upstream], [mirror][], and [license-r12].

End of section repeat 12.

---

# Section repeat 13

[upstream]: https://example.com/r13
[mirror]: https://example.org/r13
[license-r13]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r13"

## Subsection 13.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/13 "Example 13") and an
![inline image](/img/example13.png "Caption 13"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r13] reference.

Autolinks like <https://commonmark.org/13> and emails like <mail13@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 13</span> <kbd>Ctrl</kbd>+<kbd>13</kbd>.

## Subsection 13.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/13)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x13 = 13
  println(x13)
  ```

Nested ordered list:

1. First level 13
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep13)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 13.3: block quotes

> A simple block quote 13.
> It spans two lines.

> Outer block quote 13.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 13.4: code blocks

Fenced code block:

```scala
def parse13(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (13)
    indented line two
    val y13 = "with quotes \" and backslashes \\"

## Subsection 13.5: emphasis edge cases

Triple emphasis: ***foo 13 bar baz***
Mixed: *foo **bar 13** baz*
Adjacent: ****foo 13****
Nested: *a *b *c 13* d* e*

## Subsection 13.6: more references

See [upstream link][upstream], [mirror][], and [license-r13].

End of section repeat 13.

---

# Section repeat 14

[upstream]: https://example.com/r14
[mirror]: https://example.org/r14
[license-r14]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r14"

## Subsection 14.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/14 "Example 14") and an
![inline image](/img/example14.png "Caption 14"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r14] reference.

Autolinks like <https://commonmark.org/14> and emails like <mail14@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 14</span> <kbd>Ctrl</kbd>+<kbd>14</kbd>.

## Subsection 14.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/14)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x14 = 14
  println(x14)
  ```

Nested ordered list:

1. First level 14
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep14)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 14.3: block quotes

> A simple block quote 14.
> It spans two lines.

> Outer block quote 14.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 14.4: code blocks

Fenced code block:

```scala
def parse14(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (14)
    indented line two
    val y14 = "with quotes \" and backslashes \\"

## Subsection 14.5: emphasis edge cases

Triple emphasis: ***foo 14 bar baz***
Mixed: *foo **bar 14** baz*
Adjacent: ****foo 14****
Nested: *a *b *c 14* d* e*

## Subsection 14.6: more references

See [upstream link][upstream], [mirror][], and [license-r14].

End of section repeat 14.

---

# Section repeat 15

[upstream]: https://example.com/r15
[mirror]: https://example.org/r15
[license-r15]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r15"

## Subsection 15.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/15 "Example 15") and an
![inline image](/img/example15.png "Caption 15"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r15] reference.

Autolinks like <https://commonmark.org/15> and emails like <mail15@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 15</span> <kbd>Ctrl</kbd>+<kbd>15</kbd>.

## Subsection 15.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/15)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x15 = 15
  println(x15)
  ```

Nested ordered list:

1. First level 15
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep15)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 15.3: block quotes

> A simple block quote 15.
> It spans two lines.

> Outer block quote 15.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 15.4: code blocks

Fenced code block:

```scala
def parse15(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (15)
    indented line two
    val y15 = "with quotes \" and backslashes \\"

## Subsection 15.5: emphasis edge cases

Triple emphasis: ***foo 15 bar baz***
Mixed: *foo **bar 15** baz*
Adjacent: ****foo 15****
Nested: *a *b *c 15* d* e*

## Subsection 15.6: more references

See [upstream link][upstream], [mirror][], and [license-r15].

End of section repeat 15.

---

# Section repeat 16

[upstream]: https://example.com/r16
[mirror]: https://example.org/r16
[license-r16]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r16"

## Subsection 16.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/16 "Example 16") and an
![inline image](/img/example16.png "Caption 16"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r16] reference.

Autolinks like <https://commonmark.org/16> and emails like <mail16@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 16</span> <kbd>Ctrl</kbd>+<kbd>16</kbd>.

## Subsection 16.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/16)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x16 = 16
  println(x16)
  ```

Nested ordered list:

1. First level 16
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep16)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 16.3: block quotes

> A simple block quote 16.
> It spans two lines.

> Outer block quote 16.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 16.4: code blocks

Fenced code block:

```scala
def parse16(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (16)
    indented line two
    val y16 = "with quotes \" and backslashes \\"

## Subsection 16.5: emphasis edge cases

Triple emphasis: ***foo 16 bar baz***
Mixed: *foo **bar 16** baz*
Adjacent: ****foo 16****
Nested: *a *b *c 16* d* e*

## Subsection 16.6: more references

See [upstream link][upstream], [mirror][], and [license-r16].

End of section repeat 16.

---

# Section repeat 17

[upstream]: https://example.com/r17
[mirror]: https://example.org/r17
[license-r17]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r17"

## Subsection 17.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/17 "Example 17") and an
![inline image](/img/example17.png "Caption 17"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r17] reference.

Autolinks like <https://commonmark.org/17> and emails like <mail17@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 17</span> <kbd>Ctrl</kbd>+<kbd>17</kbd>.

## Subsection 17.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/17)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x17 = 17
  println(x17)
  ```

Nested ordered list:

1. First level 17
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep17)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 17.3: block quotes

> A simple block quote 17.
> It spans two lines.

> Outer block quote 17.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 17.4: code blocks

Fenced code block:

```scala
def parse17(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (17)
    indented line two
    val y17 = "with quotes \" and backslashes \\"

## Subsection 17.5: emphasis edge cases

Triple emphasis: ***foo 17 bar baz***
Mixed: *foo **bar 17** baz*
Adjacent: ****foo 17****
Nested: *a *b *c 17* d* e*

## Subsection 17.6: more references

See [upstream link][upstream], [mirror][], and [license-r17].

End of section repeat 17.

---

# Section repeat 18

[upstream]: https://example.com/r18
[mirror]: https://example.org/r18
[license-r18]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r18"

## Subsection 18.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/18 "Example 18") and an
![inline image](/img/example18.png "Caption 18"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r18] reference.

Autolinks like <https://commonmark.org/18> and emails like <mail18@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 18</span> <kbd>Ctrl</kbd>+<kbd>18</kbd>.

## Subsection 18.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/18)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x18 = 18
  println(x18)
  ```

Nested ordered list:

1. First level 18
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep18)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 18.3: block quotes

> A simple block quote 18.
> It spans two lines.

> Outer block quote 18.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 18.4: code blocks

Fenced code block:

```scala
def parse18(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (18)
    indented line two
    val y18 = "with quotes \" and backslashes \\"

## Subsection 18.5: emphasis edge cases

Triple emphasis: ***foo 18 bar baz***
Mixed: *foo **bar 18** baz*
Adjacent: ****foo 18****
Nested: *a *b *c 18* d* e*

## Subsection 18.6: more references

See [upstream link][upstream], [mirror][], and [license-r18].

End of section repeat 18.

---

# Section repeat 19

[upstream]: https://example.com/r19
[mirror]: https://example.org/r19
[license-r19]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r19"

## Subsection 19.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/19 "Example 19") and an
![inline image](/img/example19.png "Caption 19"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r19] reference.

Autolinks like <https://commonmark.org/19> and emails like <mail19@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 19</span> <kbd>Ctrl</kbd>+<kbd>19</kbd>.

## Subsection 19.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/19)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x19 = 19
  println(x19)
  ```

Nested ordered list:

1. First level 19
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep19)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 19.3: block quotes

> A simple block quote 19.
> It spans two lines.

> Outer block quote 19.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 19.4: code blocks

Fenced code block:

```scala
def parse19(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (19)
    indented line two
    val y19 = "with quotes \" and backslashes \\"

## Subsection 19.5: emphasis edge cases

Triple emphasis: ***foo 19 bar baz***
Mixed: *foo **bar 19** baz*
Adjacent: ****foo 19****
Nested: *a *b *c 19* d* e*

## Subsection 19.6: more references

See [upstream link][upstream], [mirror][], and [license-r19].

End of section repeat 19.

---

# Section repeat 20

[upstream]: https://example.com/r20
[mirror]: https://example.org/r20
[license-r20]: https://www.apache.org/licenses/LICENSE-2.0 "Apache 2.0 r20"

## Subsection 20.1: prose

The quick *brown* fox **jumps** over the ***lazy*** dog. This sentence contains
emphasis (`*`), strong (`**`), strong-emphasis (`***`), and `inline code` along
with [an inline link](https://example.com/20 "Example 20") and an
![inline image](/img/example20.png "Caption 20"). It also has a [reference link][upstream]
and a collapsed [mirror][] reference and a shortcut [license-r20] reference.

Autolinks like <https://commonmark.org/20> and emails like <mail20@example.com>
are also exercised. Backslash escapes \\* \\_ \\[ \\] \\ and entities
&amp; &#42; &#x2A; &copy; appear here too. Hard line break\
right here, soft break  
on the next line.

HTML inlines: <span>raw 20</span> <kbd>Ctrl</kbd>+<kbd>20</kbd>.

## Subsection 20.2: lists

Tight bullet list:

- alpha *italic*
- beta **bold**
- gamma `code`
- delta [link](https://example.org/20)

Loose bullet list:

* one paragraph item

  with continuation paragraph and `inline code`.

* another item with

  > nested blockquote with **bold**.

* third item with fenced code:

  ```scala
  val x20 = 20
  println(x20)
  ```

Nested ordered list:

1. First level 20
   1. Second level
      - mixed bullet at third level
      - with [link](https://example.com/deep20)
   2. Second level item two
2. First level item two
3. First level item three with `code` and *emphasis*.

## Subsection 20.3: block quotes

> A simple block quote 20.
> It spans two lines.

> Outer block quote 20.
>
> > Inner block quote with *emphasis*.
> > > Triple-nested block quote with **strong** and `code`.
>
> Back to outer with a [reference][upstream].

## Subsection 20.4: code blocks

Fenced code block:

```scala
def parse20(text: Text): Markdown of Layout =
  val cursor = Cursor(Iterator(text))
  while cursor.more do cursor.advance()
```

Indented code block:

    indented line one (20)
    indented line two
    val y20 = "with quotes \" and backslashes \\"

## Subsection 20.5: emphasis edge cases

Triple emphasis: ***foo 20 bar baz***
Mixed: *foo **bar 20** baz*
Adjacent: ****foo 20****
Nested: *a *b *c 20* d* e*

## Subsection 20.6: more references

See [upstream link][upstream], [mirror][], and [license-r20].

End of section repeat 20.
