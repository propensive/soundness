# Expressive Errors

When an error does occur, Soundness makes it as informative as it can be. An error is
an immutable value carrying the full detail of what went wrong — what was expected, what
was found, and where — rather than a bare message or an opaque code. Because that detail
is structured data, it can be inspected, rendered legibly for a person, or matched on by
handling code, and the same richness that helps a developer diagnose a fault lets a
program respond to it precisely.
