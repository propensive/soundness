# Contributing to Soundness

Firstly, thank you for taking an interesting in contributing! Soundness is an
open-source project, and welcomes contributions in the form of feature code,
bug reports and fixes, tests, feature suggestions and anything else which may
help to make it better software.


## Before Starting

It&rsquo;s a good idea to [discuss](https://discord.gg/MBUrkTgMnA) potential
changes with one of the maintainers before starting work. Although efforts are
made to document future development work using the [issue tracker](/issues), it
will not always be up-to-date, and the maintainers may have useful information
to share on plans.

A bad scenario would be for a contributor to spend a lot of time producing a
pull request, only for it to be rejected by the maintainers for being
inconsistent with their plans. A quick conversation before starting work can
save a lot of time.

If a response is not forthcoming in the [Discord
chatroom](https://discord.gg/MBUrkTgMnA), open a [GitHub
issue](https://github.com/propensive/soundness/issues) or contact the project
maintainer directly _but publicly_. Please __do not__ contact the maintainer
about technical issues privately, as it misses a good opportunity to share
knowledge with a wider audience, unless there is a good reason to do so. Jon
Pretty can usually be contacted [on X](https://x.com/propensive).

All development work&mdash;whether bugfixing or implementing new
features&mdash;should have a corresponding issue before work starts. If you
have commit rights to the `propensive/soundness` repository, push to a branch named
after the issue number, prefixed with `issue/`, for example, `issue/23`.


## Contribution standards

Pull requests should try to follow the coding style of existing code in the
repository. They are unlikely to be rejected on grounds of formatting, except
in extreme cases. Soundness does not use automatic code-formatting because it
has proven to produce unreliable and unsatisfactory results (and furthermore,
hand-formatting is not particularly laborious).

Unfortunately an official coding style guide does not yet exist.

Any code that is inconsistently formatted will be tidied up, if necessary, by
the project maintainers, though well-formatted code is appreciated.


## Code reviews

Pull requests should have at least one review before being merged. When opening
a pull request, contributors are welcome to suggest a reviewer. Pull requests
should be left in _draft_ mode until they are believed to be ready for review.

The preferred method of reviewing a pull request is to schedule a video call
with the reviewer and talk through it. It is much faster to share understanding
between the contributor and reviewer this way.

For code contributions, we prefer pull requests with corresponding tests, if
that's appropriate. Changes which break existing tests, however, are likely to
be rejected during review.


## Reporting issues

New issues are welcome, both as bug reports and feature suggestions. More
precision is preferable, and the clearest and most detailed reports will most
likely be addressed sooner, but a short report from a busy developer is still
preferred over a bug we never hear about. We will ask for more detail in triage
if it&rsquo;s needed.
