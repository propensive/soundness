Ideas for better management of Soundness repositories:

Highest priority:
- move this list to GitHub issues

High priority:
- finish Scala 3.6 migration and merge `next`->`main` branches
- fix merino git-submodule
- Get Larceny compiling, built, and used as a plugin
- Build Java sources in `burdock`
- Fix all the remaining tests
- Add more submodules
- Experiment with discoverability
- Build grouped `web`, `cli`, `data`, `base`, `dev`, `sci` binaries; maybe others

Medium priority:
- Compile for Scala.JS (where possible)
- Publishing to Maven Central using Mill
- Use Mill in CI
- Require passing CI for `soundness` PR merges
- Add branch protection rules to only allow changes through PRs

Low priority:
- Automate publish-on-tag from CI
- API Documentation
- Build and use Umbrageous for shading
- Start tracking code coverage
- scalafmt

Done:
- Merge `npcrus/soundness-monorepo` and `propensive/soundness`
