# Descriptors

One directory per released version, holding the `<artifactId>-<version>.ivy.xml` of every module
in that release: the metadata Coursier reads when [crux](https://github.com/propensive/crux)
uses the release as an Ivy-pattern repository. The jars are the release's assets. Written by the
release pipeline; not for editing.
