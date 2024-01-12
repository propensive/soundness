To use Umbrageous, `scalac` must be invoked with two additional parameters:
```sh
scalac -d bin -Xplugin:umbrageous.jar -P:umbrageous:com.example:shaded *.scala`
```

Firstly, `-Xplugin:umbrageous.jar` points the compiler to a JAR file containing
the packaged plugin.

Secondly, `-P:umbrageous:com.example:shaded` tells the plugin to shade
everything inside the `com.example` package behind the prefix `shaded`, i.e.
rewriting `com.example` to `shaded.com.example`.

This `-P` parameter can be provided multiple times to shade different packages.
If more than one parameter matches a package name (e.g.
`-P:umbrageous:com:shade1` and `-P:umbrageous:com.example:shade2`) then the
prefix corresponding to the longest match will be applied to that package.

A downstream project can include an additional wildcard import at the start to
include everything from its shaded dependencies, for example,
```scala
package myproject
import shade.*  // additional import
import com.example
```
will allow the shaded `com.example` package (shaded by the `shade` prefix)
resolve anywhere in the scope of the import (whether imported, or referred to
by fully-qualified classname).

Including the parameter, `-P:umbrageous:com.example:shade` will automatically
unshade the `com.example` package from subsequent compilations without the need
for the import.

Note that this parameter is the same whether applying or using shading.
However, caution should be taken to avoid including nonexistant shaded
packages: doing so will add the synthetic wildcard import, but it will not
resolve, and a compile error will result.

### Limitations

Umbrageous does not currently match package names that are defined in multiple
`package` declarations, such as `com.example` in:

```scala
package com
package example

object Main // ...
```

Additionally, code which references an absolute name, such as
`_root_.com.example.Main` will not find the entity with its new, shaded name.



