# tools

What works on the source from outside it: the checks, the build, the
release, the installers and the live QA. One folder per job:

```
ci/       the CI entry points, one script per runner (ci.yml, release.yml)
check/    what `just check` and CI measure: structural lint, the bloat
          ledger, the proofs gate, test slices, footprint, feed and exec
          stress
build/    what turns a checkout into a binary: the cells pull and compile,
          the SBCL runtime and its patches, the launchers
release/  packing and publishing, and their rehearsals: pack*, publish*,
          release*, the macOS and Windows halves, the installer mirror
install/  the installers a release carries beside its binary, and their test
qa/       live checks against real chat platforms and real upgrades
probes/   measurements run by hand: the turn probe and the body relay
```

A script finds the repository from its own place (`../../..` from its
folder), so it runs from any working directory.
