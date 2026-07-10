# Correctness

Correctness is the end that Soundness's other principles serve: a program should be
constrained to behave correctly, and the language should help ensure that it does.
Where most libraries make correctness merely possible, Soundness tries to make
incorrectness impossible — moving checks to compiletime, ruling out invalid states, and
making failure visible in the types — so that a program which compiles is already known
to be right about a great deal. This is what "sound by default" means: safety is the
starting point, not an extra to be bolted on later.
