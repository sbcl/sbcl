Code to reproduce the results from the "Adaptive Hashing" paper.

There are two high-level scripts `run-microbenchmarks.sh` and
`run-macrobenchmarks.sh`. Both assume that the SBCL versions to
compare are checked out from
<https://github.com/melisgl/sbcl/tree/adaptive-hash> and compiled in
the following directories:

- `~/src/sbcl.0`: (commit "!!! hack generate-version.sh") This is
  stock SBCL 2.4.2 plus a hacked `generate-version.sh` to reduce the
  chance of heap layout changes due to longer or shorter source
  directories. Numbers for this version are not reported in the paper.

- `~/src/sbcl.1`: (commit "!!! baseline") This is the stock SBCL with
  the Prefuzz hash (Pr) plus a lot of performance optimizations
  unrelated to Adaptive Hashing. Most of the optimizations are to hash
  tables. They are intended to strenghten the baseline as much as
  possible.

- `~/src/sbcl.u`: Murmur hash (Mu). Patch the baseline with
  `murmur.diff`.

- `~/src/sbcl.f`: (commit "Adaptive hashing: flat hash tables") This
  is the Constant hash (Co).

- `~/src/sbcl.m`: Constant then Murmur hash (Co+Mu). Patch the
  previous with `murmur.diff`.

- `~/src/sbcl.s`: (commit "Adaptive hashing: add safe fallback for EQ
  hash tables") Constant then Prefuzz with fallback to Murmur hash
  (Co+Pr>Mu).

- `~/src/sbcl.2`: (commit "Adaptive hashing: EQ hash tables") This is
  the adapative `eq` hash (Co+PS>Pr>Mu) in the paper.

- `~/src/sbcl.3`: (commit "Adaptive hashing: EQUAL hash tables") The
  previous plus adaptive `equal` hash for strings and conses.
