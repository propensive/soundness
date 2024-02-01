Scala source code is nontrivial to parse, and while many syntax-highlighting scripts or
configurations exist (for example, for editors), the complexity of the Scala language means that
they can make mistakes when encountering unusual combinations of tokens. _Harlequin_ uses the
actual Scala compiler to ensure that code is parsed exactly as it would be during compilation.

The result of parsing code contains only three different types of token—spaces, newlines and
content—which lend themselves to conversion to HTML or another format.

