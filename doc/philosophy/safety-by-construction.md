# Safety by Construction

Soundness makes invalid values impossible to construct in the first place, checking
them as the code compiles. A guiding conviction is that any static analysis a
programmer can do, the compiler can do better — and any analysis the compiler can do,
it should. Interpolators express this most clearly: a path, a URL, a date, or a
regular expression written as a literal is parsed and validated at compiletime, so a
malformed one is a compile error rather than a runtime surprise. A value that exists is
therefore known to be well-formed, and the code that receives it need not check again.
