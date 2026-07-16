_Facsimile_ reads PDF files in Scala, from the low-level object syntax of ISO 32000-2 up
through the document structure, with random access through the file's cross-reference tables
and lazy, on-demand loading of objects. A PDF is opened within a capture-checked scope, so
the file handle can never outlive it, while every value extracted from the document is pure
data which may be freely retained.
