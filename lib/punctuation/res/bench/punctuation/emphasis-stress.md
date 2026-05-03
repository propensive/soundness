# Emphasis stress

A fixture of pure inline density, designed to isolate inline-parser cost from
block-parser cost. Almost no block structure: just paragraphs packed with
emphasis runs, links, code spans, autolinks, escapes, and entities. The
delimiter-stack algorithm and the rule-of-3 length pairing are exercised
heavily.

[a]: https://example.com/a "title a"
[b]: https://example.org/b 'title b'
[c]: https://example.net/c (paren title c)
[deep]: https://example.com/deep

Paragraph one: *single em* and **double strong** and ***triple ems*** and
****quad runs**** and *****five***** and ******six****** in a row, with `code`
and [a link](https://example.com) and ![an image](/i.png "alt") and an
autolink <https://example.org> and a [reference][a] and a [collapsed][] and a
shortcut [c] reference. Backslash escapes \* \_ \[ \] \\ and entities &amp;
&#42; &#x2A; &copy; appear too.

Paragraph two — the gnarly cases for the rule of 3:
*foo***bar
**foo***bar
***foo*bar**
**foo*bar***
*foo *bar* baz*
**foo **bar** baz**
***foo ***bar*** baz***
*a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*p*q*r*s*t*u*v*w*x*y*z*

Paragraph three — alternating asterisk and underscore (different delimiters
do not pair with each other):
*a_b_c*
_a*b*c_
**a__b__c**
__a**b**c__
***a___b___c***

Paragraph four — Unicode flanking (left/right flanking rules with punctuation
and whitespace boundaries):
".*foo*."
"(*foo*)"
"a*b*c"
"a *b* c"
"a* b *c"
"_foo_."
"(_foo_)"
"a_b_c"
"a _b_ c"

Paragraph five — long runs and adjacent runs:
**********foo**********
__________foo__________
*****foo*****bar*****baz*****
*foo**bar***baz****qux*****quux*

Paragraph six — links and images mixed with emphasis:
[*italic link*](https://x)
[**bold link**](https://x)
[***both link***](https://x)
*[italic around link](https://x)*
**[bold around link](https://x)**
*outer **inner [link](https://x) inner** outer*
![*italic image*](/i.png)
*[image](/i.png) and ![embedded](/e.png)*

Paragraph seven — code spans inside emphasis (code spans take precedence):
*foo `code` bar*
**foo `code` bar**
*`code` foo*
*foo `co*de` bar*
*foo \`code\` bar*

Paragraph eight — references inside emphasis:
*[a]*
**[b]**
***[c]***
*[a][a] [b][] [c]*
**[deep][]**

Paragraph nine — backslash escapes inside delimiter runs:
*foo\*bar*
**foo\**bar**
*foo\_bar*
*foo\\bar*
\*not emphasis\*
\**not strong\**
\*\*emphasis around escapes\*\*

Paragraph ten — autolinks and HTML inlines mixed with emphasis:
*<https://example.com>*
**<mail@example.com>**
*<span>html</span>*
**<em>nested em</em>**
*foo <span>span</span> bar*

Paragraph eleven — adjacent emphasis runs must not merge:
*foo* *bar* *baz*
**foo** **bar** **baz**
*foo***bar***baz*
**foo*bar*baz**

Paragraph twelve — pathological-ish nested cases (still well-formed):
*a *b *c *d *e *f *g *h *i *j* k* l* m* n* o* p* q* r*
**a **b **c **d **e **f **g** h** i** j** k** l** m** n**

Paragraph thirteen — link reference resolution stress:
[a][a] [a][b] [a][c] [b][a] [b][b] [b][c] [c][a] [c][b] [c][c]
[a] [b] [c] [deep] [a][] [b][] [c][] [deep][]
![a][a] ![b][b] ![c][c] ![deep][deep]
[**bold ref**][a] [*italic ref*][b] [`code ref`][c]

Paragraph fourteen — entities packed densely:
&amp;&amp;&amp;&amp;&amp;&amp;&amp;&amp;&amp;&amp;&amp;&amp;
&#42;&#42;&#42;&#42;&#42;&#42;&#42;&#42;&#42;&#42;
&#x2A;&#x2A;&#x2A;&#x2A;&#x2A;&#x2A;&#x2A;&#x2A;
&copy;&reg;&trade;&hellip;&mdash;&ndash;
&lt;&gt;&quot;&apos;&nbsp;

Paragraph fifteen — code spans of varied lengths:
`a` ``b`` ```c``` ````d```` `e``f``g`
`with backtick \` inside`
`` `interior` ``
``` ``two backticks`` ```
`spaces   preserved`
`  trim leading and trailing  `

Paragraph sixteen — final mixed paragraph hitting everything:
The *quick* **brown** ***fox*** [jumps](https://x) over the [lazy][a] dog with
`inline code` and an ![image](/i.png) and an autolink <https://example.org>
plus an entity &amp; and a backslash escape \* and a hard break\
on the next line and a soft
break wrapping here, all in one *italic **strong** italic* and one **strong
*italic* strong** and one ***strong-italic [link](https://x) strong-italic***.

[collapsed]: https://example.com/collapsed
