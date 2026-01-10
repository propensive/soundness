### Decoupled Integration

Soundness is a vast ecosystem of small libraries, for many diverse applications. For specific needs, individual libraries can be selected à la carte, without introducing a complex graph of dependencies. Care has been taken to decouple libraries which aren't directly related, without compromising their integration. Soundness's typeclass-based approach has made it possible to avoid unnecessary dependencies through the careful specification of small interfaces.

But Soundness also provides six bundles of libraries targetting different domains. These are web for web applications; data for data processing; sci for scientific applications; cli for command-line applications; test for testing; and, tool for tooling development. The base bundle provides a common set of fundamental libraries, and all includes everything.
