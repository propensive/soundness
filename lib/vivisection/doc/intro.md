_Vivisection_ is a session-based debugger for the JVM, designed for Scala rather than adapted from
a Java debugger. It speaks the JDWP wire protocol directly over a socket, launches or attaches to a
debuggee, and drives breakpoints, stepping, variable inspection and expression evaluation through a
capability-scoped session. Frames, variables and values are translated back into the terms the
programmer wrote — decoding compiled names through TASTy, stepping through `inline` expansions, and
rendering values through their static types rather than their erased runtime representation.
