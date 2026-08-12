### Keypresses

A `Keypress` is one of:

- `CharKey(char)` — an ordinary character
- `FunctionKey(number)` — `F1` to `F12` and beyond
- a named editing key: `Tab`, `Enter`, `Backspace`, `Delete`, `Escape`, `Insert`, `Home`, `End`,
  `PageUp`, `PageDown`, `Up`, `Down`, `Left`, `Right`
- `EscapeSeq(id, content*)` — a terminal escape sequence with no other name

and each of those may be wrapped in the modifiers held down with it: `Shift`, `Alt`, `Ctrl` and
`Meta`, nesting outwards.

```scala
Keypress.Ctrl('A')
Keypress.Ctrl(Keypress.Shift(Keypress.Enter))
```

The wrappers are typed to accept only what can meaningfully be modified, so `Shift(CharKey('a'))`
is rejected — a shifted `a` is `CharKey('A')`.

A `Keypress` renders with a Unicode symbol in square brackets for each special key, joining a
modifier to what it modifies with `+`: `[⇧]+[↵]`, `[⌃]+C`, `[⌥]+[→]`.

### Who uses it

Nothing here encodes or decodes anything. [Profanity](https://github.com/propensive/profanity)
decodes terminal escape sequences — CSI-u, kitty, VT — into `Keypress` values, and
[Tarantula](https://github.com/propensive/tarantula) renders them as the WebDriver actions a
browser understands. Neither needs to depend on the other to say what a keypress is.
