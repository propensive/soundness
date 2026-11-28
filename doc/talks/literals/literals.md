# Checked literals

Soundness is a set of libraries for Scala. It is built so that code which compiles does not fail
by surprise. One of the ways it does this is with literals: values which are written directly in
the source code, and checked by the compiler.

[Here is what follows](@overview). First, the reason that literals matter. Then a quick tour of
the literals in Soundness. Then what happens when a literal is wrong. Then how other values are
put inside a literal, safely. Then what all of this costs when the program runs. And last, the
same literals used the other way round, as patterns.

## Why literals matter

Soundness has [two golden rules](@golden-rules).

The first rule: impossible states should be unrepresentable. If a value would make no sense,
there should be no way to construct it.

The second rule: transitions between states should be total. An operation must give an answer
for every input it accepts.

Together, the two rules mean that a program starts in a correct state, and never leaves one.

To keep these rules, every failure which can happen must be handled. Soundness enforces this.

Here is [a method which reads a media type from text](@runtime-parse). The text comes from
outside the program, so it may be wrong. The method says so, in its type: it raises a media type
error. Code which calls this method cannot ignore that.

[It must handle the failure](@must-handle), or pass it on. Here the failure is handled, and the
result is a value which may be absent.

How errors are handled in Soundness is the subject of a later video. For now, one fact is
enough. A failure which is possible must be handled, and the compiler insists on it.

But [look at the last line](@literal-cannot-fail). It is the same media type, written as a
literal. It is the same parser, and it checks the same rules. The difference is that this text
is fixed. The compiler can read it, and the compiler can see that it is valid. This line cannot
fail when the program runs. So there is nothing to handle, and Soundness does not ask for
anything to be handled.

This idea has a name: [exact handling](@exact-handling).

Handle every failure which is possible. Never handle a failure which is impossible.

A type which hides a possible failure is dishonest. But a type which declares an impossible
failure is dishonest too. It describes a phantom error: an error which can never happen. And it
makes the programmer write code which can never run.

Exact handling means that the types tell the truth, in both directions.

## A tour

Soundness has many literals. Each one is a short prefix, followed by a string. Here are some of
them, quickly.

[Text](@tour-text). [A URL](@tour-url). [An email address](@tour-email).
[A hostname](@tour-host). [An IP address](@tour-ip). [A MAC address](@tour-mac).
[A TCP port](@tour-port).

[A UUID](@tour-uuid). [A version number](@tour-version). [An ISIN](@tour-isin), which
identifies a share. [A media type](@tour-media). [A colour](@tour-colour).
[Bytes, in hexadecimal](@tour-hex). [A byte, in binary](@tour-binary).
[A time zone](@tour-timezone). [A date](@tour-date). [A length of time](@tour-duration).

[A regular expression](@tour-regex). [A path](@tour-path) to a file.
[A command](@tour-command) for the shell. [A JSON document](@tour-json).
[A YAML document](@tour-yaml). [An XML document](@tour-xml). [An HTML document](@tour-html).
[And a CSS stylesheet](@tour-css).

That is twenty-five literals, and there are more. Every one of them is checked as the code
compiles. Not one of them needs any error handling.

## A literal which is wrong

A check is only useful if it can fail. So here is what happens when a literal is wrong.

[This is not a UUID](@uuid-wrong). The code does not compile, and the compiler says why.
[Correct it](@uuid-right), and it compiles.

[The thirtieth of February](@date-wrong) is not a date. The compiler knows the calendar.
[The twenty-ninth](@date-right), in a leap year, is a date.

[This ISIN](@isin-wrong) looks right. But its last digit is a check digit, and the check fails.
[Change one digit](@isin-right), and it passes.

[Here is a media type with a spelling mistake](@media-wrong). The compiler knows the registered
media types, and it suggests the right one. [With the right spelling](@media-right), it compiles.

[And a port number](@port-wrong) of seventy thousand. Port numbers stop at sixty-five thousand,
five hundred and thirty-five. [Seven thousand](@port-right) is allowed.

In every case, the mistake never reaches a running program. It is found by the compiler, a few
seconds after it is typed.

In a longer literal, it matters where the mistake is. So the error says where.

[One character here is not hexadecimal](@hex-wrong), and the compiler says which one.
[Now it is correct](@hex-right).

[This regular expression has a group which is never closed](@regex-wrong). The compiler gives
the position where the group should have ended. [Close it](@regex-right), and the error goes.

[This shell command has a quote which is never closed](@shell-wrong). The compiler says so.
[Close the quote](@shell-right).

[In this XML, the closing tag does not match the opening tag](@xml-wrong). The compiler names
both tags, and it gives the line and the column. [Now the two match](@xml-right).

[And this is HTML](@html-wrong). It is well formed, but it is not valid: a paragraph cannot be a
child of a list. Soundness knows the rules of HTML, so this is a compile error as well.
[A list item](@html-right) is allowed.

## Substitutions

So far, every literal has been fixed text. But most real literals have gaps in them, and the
gaps are filled by other values. This is called a substitution. In Scala, it is written with a
dollar sign.

In many languages, a substitution turns the value into a string, and pastes that string into
place. That is how injection attacks happen. Soundness never does this.

[A substitution is a value](@typed-substitutions). It is never turned into a string first. The
`toString` method is never called. The literal looks at the type of the value, and at the place
where the value is used, and decides what the value means there. If the value means nothing
there, the code does not compile.

Start with [text](@text-shown). A name and a number are substituted into some text. Each has a
type which says how it should be shown, so each is accepted.

[Now here is a new type](@text-lute): a lute. Nothing says how a lute should be shown. Most
languages would print something useless. [Soundness refuses](@text-rejected). A lute is not
showable, and so this is a compile error.

[Next, JSON](@json-values). The name is text, so it becomes a JSON string: it is given quotes,
and any special characters in it are escaped. The number becomes a JSON number. Nothing was
pasted in. Each value was encoded as JSON, according to its type.

[Here is the same name, inside a JSON string](@json-in-string). In this place it is part of a
string, so it is encoded as text.

[A list, followed by a star](@json-spread), adds each of its elements to an array.

[A case class](@json-record) becomes a JSON object. Its encoder is derived by the compiler, from
the fields of the class.

[And the lute](@json-rejected). There is no way to encode a lute as JSON, so this does not
compile. The message says what is missing.

[HTML](@html-child) is stricter, because HTML has more rules. Text inside a list item becomes a
text node, and it is escaped. [An element inside a list](@html-nested) becomes a child of that
list.

Attributes are typed, too. [A link takes a URL](@html-url), and a URL is what it is given.
[A table cell can span a number of columns](@html-number), and a number is what it is given.

[The same attribute on a paragraph](@html-wrong-tag) is an error, because a paragraph has no
such attribute. [And a lute inside a paragraph](@html-rejected) is an error, because there is no
way to show it.

The other document formats follow the same rules. [In XML](@xml-values), a value may be used as
an attribute, or as a node. [In YAML](@yaml-values), a number becomes a YAML number.

[CSS](@css-colour) checks each value against its property. A colour is accepted where a colour
belongs. [But a colour is not a width](@css-wrong-type), so this is rejected.
[And this spelling of colour](@css-unknown) is not a CSS property at all. CSS spells it the
American way, and the compiler knows that.

[A shell command](@shell-text) is a classic source of mistakes. Here, a file name has a space in
it. In a shell script, that would become two arguments. Here it is one value, so it is one
argument, always.

[A list of two files](@shell-list) becomes two arguments.
[The same list, inside quotes](@shell-quoted), becomes one argument. The quotes decide, as they
do in a shell. But nothing is ever split by accident.

[Last, a URL](@url-text). The title has a space in it, and a space is not allowed in a URL. So
the value is encoded for the place where it is used. [And a number](@url-port) can be used as a
port.

In all of these examples, the rule is the same. A substitution is typed. The same value may mean
different things in different places. And a value which means nothing is rejected, as the code
compiles.

## Precise types

A literal can also give back more than a general type.

[This URL](@type-url) does not only have the type of a URL. It is an HTTPS URL, and its type
says so. [This port](@type-port) has its number in its type.

[A date literal](@type-date) is a date. [Add a time](@type-moment), and the same prefix gives a
timestamp.

[An address written in version four](@type-ip) is a version four address.
[Written in version six](@type-ip6), it is a version six address.

[And an XML literal](@type-xml) knows the name of its root element.

The compiler worked out each of these types from the text of the literal. Types which depend on
values are a large subject, and they have a later video of their own.

## What it costs

There is one more question. What does all of this cost when the program runs?

Take [this HTML literal](@cost-literal), with one substitution. There is a simple way to
implement it. Check the literal as the code compiles. And then, when the program runs,
[build a string, and parse it again](@cost-reparsed).

That would be safe. But it would do the same work twice, and it would do it every time the line
runs.

Soundness does not do this. The compiler has already parsed the literal, so it already knows the
structure. It replaces the literal with
[the code which constructs the value directly](@cost-constructed). An element. Its name. Its
child, with the substituted value in its place. Nothing is parsed when the program runs, because
there is nothing left to parse.

[The same is true of JSON](@cost-json).
[The literal becomes the code which builds the object](@cost-json-constructed): the keys, which
are fixed, and the values, with the substitution encoded in its place.

So a literal is as fast as the code which a careful programmer would write by hand. It is only
much easier to read. And it has been checked.

## Patterns

A literal constructs a value from its parts. The opposite is to take a value apart. In Scala,
that is pattern matching, and the same literals can be used as patterns. Putting values into a
literal is called interpolation. Taking values out of one can be called extrapolation.

[Here is some JSON, and a pattern](@match-json). The pattern is a JSON literal, with a gap where
the title should be. If the value has that shape, the title is given a name. The last gap takes
all the other fields.

[A pattern can also contain fixed values](@match-json-exact). This one matches only if the year
is sixteen hundred and two.

[And a pattern can look deeper](@match-json-array). Here it looks into an array. It names the
first element, and then all the others.

[XML works in the same way](@match-xml). The value of an attribute is given as text, and the
child is given as a node. [A pattern can take every attribute at once](@match-xml-element), as a
map, and it can take a whole element. [HTML has patterns too](@match-html), and there the type
of each value comes from the rules of HTML.

[Regular expressions](@match-regex) are the oldest kind of pattern. In Soundness, a name before
a group takes whatever that group matches, and the type follows from the pattern. A group which
matches once gives text. [A group which may be absent](@match-regex-optional) gives an optional
value. [And a match which repeats](@match-regex-repeated) gives a list.

Patterns are checked like every other literal: a pattern which is wrong does not compile. And
patterns are fast for the same reason as before. The compiler reads the pattern, and it writes
the code which does the matching.

## Bringing it together

[A checked literal is three things](@wrap-up).

It is checked, as the code compiles. A mistake in a literal is never seen by a user.

It is typed, in what it accepts and in what it gives back. Nothing is ever pasted in as a
string.

And it is fast, because the work has been done already, by the compiler.

Behind all three is one principle: exact handling. Handle every failure which is possible. Never
handle a failure which is impossible. A literal is the clearest case. The compiler has proved
that it cannot fail. So the code says nothing about failure, and it is right to say nothing.

## Next

The next video applies the same principle to collections. In most languages, asking for the
first element of an empty list is a runtime error. In Soundness,
[a list which is known to have elements](@next-populated) gives its first element, and nothing
can go wrong. [A list which may be empty](@next-optional) gives an optional value. The types are
exact, again. Lists, maps and sets, made total and safe: that is next.

## Links

[Soundness is free, and it is open source](@links). The documentation is at soundness dot dev.
The code is on GitHub, at propensive slash soundness. There is a community on Discord, at
propensive dot com slash discord. And for more videos like this one, follow propensive on
YouTube, on Instagram and on TikTok.

For a team which wants to use Soundness in its own work, Propensive offers consultancy and
training. The details are at propensive dot com.
