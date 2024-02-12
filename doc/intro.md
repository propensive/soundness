When we write a program in Scala, _usually_ all the code that is compiled
together will be run together (along with dependencies), in the _same
environment_: a single instance of the JVM, or in a browser with Scala.JS.
(Macros are a notable exception, since they are run during a later compilation,
but they generally appear in _library_ rather than _application_ code.)
Conversely, in distributed applications, code which is intended to run in
_different environments_ would be compiled separately, and would remain
separate from source code to its execution in separate JVMs, web browsers, and
maybe multiple machines or docker instances.

Examples include client-server communications between an HTTP server and a web
browser, and microservice-based architectures. For a distributed application,
there is an inherent contract between any two distinct environments in the
system.

However, our usual approach to development means that this contract is not
enforced by the compiler. Even though other tools may be employed to ensure
contractual consistency, this must happen externally to the compiler, and
rarely offers the same strong guarantees that Scala can. So contractual
consistency can be compromised, and lead to runtime failures.

_Superlunary_'s model compiles source code to be run in different environments
_together_, using _quotes and splices_ to precisely and safely delimit local
from remote code. This allows code which runs in a remote environment to be
written alongside local code; to be fully checked by the compiler; to be
marshalled and unmarshalled transparently; and to be maintained in lockstep.

_Superlunary_ makes it possible to develop a distributed application with the
versatility, simplicity and self-consistency as an application which runs
within a single runtime environment.
