# Examples

This is a store of short examples. Each shows one thing a reader may want to do with
Soundness, in the light of one idea which runs through all of it, in two to ten lines of code.
They are meant to be read one at a time, chosen from two lists.

## The two aspects

A *topic* is what the example does: reading JSON, making an HTTP request, writing a date. The
topics are the tutorials of [the documentation](../soundness/modules/overview.md), and are
listed in `topics.tel`.

A *theme* is the idea the example shows: that a literal is checked by the compiler, that an
error is named in a type, that a capability cannot leave its scope. The themes are listed in
`themes.tel`, each with a sentence saying what it means, and most have an essay of their own
under [the philosophy](../philosophy/correctness.md).

An example of the theme `checked-literals` on the topic `json` is the file
`json/checked-literals.md`. A second example of the same pair has a word after the theme,
as `json/checked-literals.pointer.md`. Most pairs have no example, because most topics show
only some of the themes well: an example is written only where the pair teaches something
that neither aspect does alone.

`python3 etc/check-examples.py --matrix` shows which pairs are filled.

## Writing an example

An example is a title, a sentence or two, one block of code, and a sentence which leads to the
tutorial or the essay behind it. The standard for prose and code is that of
[the tutorials](../standards/style.md), and these rules are added to it:

- The title says what the example shows, in the words of the topic, and names neither aspect.
- The code is the least which shows the idea: two to ten lines in each frame. Example data
  comes from Shakespeare, as [everywhere else](../standards/examples.md).
- The code has no comments. What a comment would say is said by a `highlight` or a `focus`,
  by a frame's `label`, or by the sentences around the code.
- Where the idea is that something wrong does not compile, the example is an animation: a
  frame marked `errors show` which holds the mistake, with the compiler's own message on it,
  then a frame which puts it right. An error is never written by hand.
- Where the idea is a choice, such as a strategy, a calendar or a backend, the versions are
  `alternative`s, under tabs.
- Every frame is compiled. An example which shows a result prints it from a `run` fence after
  the animation, and one which cannot run where the documentation is built, because it needs
  the network or a terminal, says so.
- A mark names its range without a space in it, since what follows the first space is its
  caption: a range of several words is written from its first characters to its last, as
  `raises..Error`. Every mark has a caption.
- Where every frame would repeat the same several imports, they go in a plain fence before the
  animation, which the frames then follow.
- An example stands alone. It imports what it needs, beyond the `soundness` package itself,
  and does not depend on another example.

If the code an example needs is not short, or is not safe, the fault may be in the API and not
in the example. Such a case is recorded on the issue which tracks them, and the example is left
out until the API is fit to be shown.
