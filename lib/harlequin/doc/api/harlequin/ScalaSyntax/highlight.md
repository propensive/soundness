tries to parse the `:Text` value as Scala syntax, returning a sequence of `:Token`s

This method will always return a sequence of tokens containing the entirety of the original text, even if it
does not represent valid Scala code. Some tokens will, however, be marked as erroneous in this case.