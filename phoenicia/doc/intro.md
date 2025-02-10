TrueType and OpenType fonts have become a _de facto_ standard for describing
how character data, or _text_, should be visually rendered. As binary data,
font files are not as straightforward to read as many other files. This is
particularly true of `TrueType` and `OpenType` fonts, which can include a
variety of font-related data, not all of which is needed for all applications:
_most_ of the data in a font file is exists to describe the shapes of each
glyph, which is not easy to work with. So usually, reading font files is left
to graphics software, web browsers or the operating system.

But there remains useful data (or _metadata_) in a font file which many
applications can take advantage of. In particular this includes font metrics
data, essentially the widths of glyphs, and information about the font's weight
or slant. _Phoenicia_ provides read-only access to this data.

