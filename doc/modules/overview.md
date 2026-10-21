## Overview

Soundness is one library with many parts, and these tutorials take one topic each. They are
grouped here by what a reader is likely to be doing; each builds its samples up in order, and
most link to the philosophy behind their design — [safety by construction](../philosophy/safety-by-construction.md), [direct style](../philosophy/direct-style.md), [honest signatures](../philosophy/honest-signatures.md) and the rest.

### Foundations

- [Text](text.md) — writing, slicing, searching and rendering text, with the types that are textual
- [Characters](characters.md) — encodings, Unicode properties, widths and grapheme clusters
- [Numbers](numbers.md) — checked arithmetic, bit manipulation, precision, ordinals and bounded numbers
- [Mathematics](mathematics.md) — complex numbers, vectors, matrices and permutations
- [Optional Values](optional-values.md) — absence without `Option`, and what it costs
- [Errors](errors.md) — declaring that code can fail, choosing a strategy, and recovering
- [Collections](collections.md) — the collection types, and how they cross to the standard library
- [Patterns](patterns.md) — regular expressions and globs, checked as the code compiles
- [Interpolation](interpolation.md) — writing an interpolator of your own
- [Names](names.md) — identifiers validated against a plane of rules
- [Decoding](decoding.md) — turning text into typed values, and extracting them in patterns
- [Display](display.md) — showing and inspecting values
- [Derivation](derivation.md) — deriving typeclass instances for products and sums
- [Annotations](annotations.md) — reading annotations at compiletime
- [Generic Data](generic-data.md) — a representation every format can be converted through
- [Records](records.md) — typed records from a specification
- [Catalogs](catalogs.md) — a value for every field of a type, addressed by path
- [Optics](optics.md) — updating nested immutable values
- [Bit Flags](flags.md) — sets of flags as bits
- [Bloom Filters](bloom-filters.md) — probabilistic membership
- [Caching](caching.md) — an LRU cache and expiring values
- [UUIDs](uuid.md) — generating, parsing and combining UUIDs
- [Randomness](randomness.md) — random values, distributions and reproducible runs
- [Versioning](versioning.md) — versions and manifests
- [Diffing](diffing.md) — diffs, patches and evolutions
- [Trees](trees.md) — trees and graphs drawn as diagrams
- [Graphs](graphs.md) — building, ordering, folding and drawing graphs

### Data formats

- [JSON](json.md) — parsing, typed access, building, schemas and validation
- [YAML](yaml.md) — parsing, tags, anchors and encoding
- [XML](xml.md) — nodes, namespaces, XPath and schemas
- [CBOR](cbor.md) — binary JSON-like encoding, with tags and diagnostic notation
- [CSV](csv.md) — rows, headers and formats, decoded to case classes
- [TEL](tel.md) — the indentation-based data language, its schemas and editing
- [Protocol Buffers](protobuf.md) — messages, fields and compatibility
- [Markdown](markdown.md) — parsing, rendering and writing back
- [Base Encoding](base-encoding.md) — Base64, hexadecimal and other alphabets
- [Binary Data](binary-data.md) — describing and reading a binary layout
- [Compression](compression.md) — compressing values and streams
- [Archives](archives.md) — ZIP, tar and ar
- [Hashing](hashing.md) — digests, checksums and fast hashing
- [Cryptography](cryptography.md) — encryption, signing, keys and certificates
- [Media Types](media-types.md) — media types, and types that know theirs

### Time, quantity and locale

- [Time](time.md) — dates, times, instants, zones, calendars, recurrences and formatting
- [Quantities](quantities.md) — units, dimensions and conversions, checked at compiletime
- [Money](money.md) — amounts, splitting, prices and tax
- [Geography](geography.md) — angles, compass points and locations
- [Chemistry](chemistry.md) — elements, molecules, formulas and equations
- [Colors](colors.md) — color spaces, mixing, distance and palettes
- [Internationalization](internationalization.md) — translations and locales
- [Hyphenation](hyphenation.md) — hyphenating and syllabifying words

### Filesystem, processes and the environment

- [Paths](paths.md) — building, relating and deconstructing paths
- [Filesystem](filesystem.md) — files, directories, access and watching
- [Environment](environment.md) — variables, properties, directories and the machine
- [Streams](streams.md) — reading, writing and composing streams of data
- [Processes](processes.md) — defining, running and piping commands
- [Concurrency](concurrency.md) — supervised tasks, cancellation and promises
- [Logging](logging.md) — loggers, levels and routing
- [Classpaths](classpath.md) — resources, classloaders and services
- [Daemons](daemons.md) — long-running applications with fast clients
- [Containers](docker.md) — building and reading images, and talking to a daemon
- [Git](git.md) — repositories, commits, branches and remotes
- [SSH](ssh.md) — host keys, transports and remote commands
- [Databases](database.md) — declaring, storing and querying

### Networking and the web

- [Network Addresses](network-addresses.md) — URLs, hostnames, IP addresses and ports
- [Sockets](sockets.md) — serving, clients, TLS and conversations
- [HTTP Client](http-client.md) — requests, headers, bodies and sessions
- [HTTP Server](http-server.md) — servers, responses, WebSockets and HTTP/2
- [RPC](rpc.md) — interfaces over JSON-RPC, gRPC and server-sent events
- [Email](email.md) — composing and sending
- [OAuth 2](oauth.md) — tokens and credentials
- [OpenAPI](openapi.md) — typed clients from a document
- [Service Discovery](service-discovery.md) — advertising and browsing services
- [Web Automation](web-automation.md) — driving a browser
- [HTML](html.md) — constructing, checking, rendering and parsing HTML
- [Web Pages](pages.md) — whole pages from archetypes
- [The Browser DOM](dom.md) — typed scripting of the page
- [CSS](css.md) — writing, validating and rendering stylesheets
- [Forms](forms.md) — rendering and handling forms
- [SVG](svg.md) — shapes, paths, transforms and documents
- [Charts](charts.md) — series, chart kinds and rendering
- [Label placement](label-placement.md) — captions that avoid their obstacles
- [Images](images.md) — pixels, canvases and conversion
- [Audio](audio.md) — reading, writing, recording and playback
- [Fonts](fonts.md) — loading, measuring and subsetting
- [PDF](pdf.md) — creating, reading and editing documents
- [Mathematical Markup](mathematical-markup.md) — MathML trees

### Command line and terminal

- [Command-Line Applications](cli.md) — entry points, arguments, flags, completions and help
- [Terminal](terminal.md) — styled text, input, menus and layout
- [Terminal Emulation](terminal-emulator.md) — feeding and inspecting a terminal
- [Tables](tables.md) — tabulating values at any width
- [Gauges](gauges.md) — statuses as gauges
- [Syntax Highlighting](syntax-highlighting.md) — highlighting, types and completions

### Compiler, tooling and agents

- [Compilation](compiler.md) — configuring and running compilers, and producing artifacts
- [Metaprogramming](metaprogramming.md) — givens, type-level values and expansions
- [Staging](staging.md) — quotes, splices and rigs
- [Testing](testing.md) — suites, tests, contrasts and benchmarks
- [Debugging](debugging.md) — sessions, breakpoints and stepping
- [Stack Traces](stack-traces.md) — capturing, demangling and resolving traces
- [Foreign Interoperability](foreign-interop.md) — loading and navigating foreign interfaces
- [Language Server Protocol](lsp.md) — defining and running a server
- [ACP](acp.md) — the agent client protocol
- [MCP](mcp.md) — the model context protocol
- [Language Models](llm.md) — conversations, tools and structured answers
