### Browser Sessions

_Tarantula_ makes it possible to control a web browser programmatically from Scala. Currently [Firefox](https://www.mozilla.org/en-GB/firefox/new/) and [Chrome](https://www.google.com/chrome/) are supported.

All browser operations take place in a _session_, which may be started by calling the `session` method, specifying a port number, on
a `Browser` object; either `Chrome` or `Firefox`. For example:
```scala
Firefox.session(8120):
  // Browser actions are carried out in this scope
```

### Simple navigation

Within the session body, the `browser` object, may be accessed and used to control the newly-launched browser.

The `browser` object is an instance of `WebDriver#Session`, and includes several navigational methods:
- `navigateTo(url)` - to send the browser to a particular URL
- `refresh()` - to refresh the page
- `back()` - to go back to the previous page
- `forward()` - to go forward (assuming we have already gone back at least once)

The `title()` method will also return the page title, and `url()` the current URL, as `Text` instances.

### Acessing Elements

Within a particular page, it's possible to access an element with the `element` method, which takes, as a parameter, a
way of locating that element, of which several different types are valid:
- `Text`: finds an element by its link text
- `Selector`: type from [Cataclysm](https://github.com/propensive/cataclysm), which find a value by CSS selection
- `TagType`, `DomId`, `Cls`: types from [Honeycomb](https://github.com/propensive/honeycomb), which finds a value by an HTML tag, DOM ID or CSS class

For example, the link containing the text `"here"` could be selected with, `browser.element(t"here")` or the element which is an
instance of an `<img>` HTML tag could be found with `browser.element(Img)`, where the `Img` value is defined in Honeycomb. In both
cases an `Element` instance will be returned, or an exception will be thrown if no matching element exists on the page.

HTML has a tree-based structure, so it's possible to select one element within another with repeated applications of the
`Element#element` method, for example,
```scala
val link = browser.element(Nav).element(id"menu").element(t"About")
```
would find the link containing the text `About` in the element with ID `menu` which is inside a `<nav>` HTML element.

### Accessing multiple elements

Often it's useful to find all elements on a page, which is served by the `/` method of `browser`, `Element` and as an
extension on `List[Element]`. Although the method is defined on three types, it always returns a `List[Element]`, and since
this is one of the types defining `/`, it is easy to progressively filter a selection of elements to a single one, with
repeated applications. The infix syntax is particularly intuitive. For example,
```scala
for elem <- browser / id"menu" / Li / cls"checkbox" do elem.click()
```
would simulate a click on every element with the `checkbox` CSS class inside an `<li>` tag in the element with ID `menu`.



