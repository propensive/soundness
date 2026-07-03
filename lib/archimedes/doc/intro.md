__Archimedes__ provides a typed representation of [Presentation
MathML](https://www.w3.org/TR/MathML/), the markup language for mathematical
notation. A MathML document is modelled as an immutable tree of case classes,
one per element (`Mfrac`, `Msup`, `Mrow`, `Mtable`, …), which can be rendered to
and parsed from XML (through [Xylophone](https://github.com/propensive/xylophone))
and embedded in or extracted from HTML (through
[Honeycomb](https://github.com/propensive/honeycomb)).
