# ghc-prof-aeson-flamegraph

Turn GHC's JSON profiling output into
[FlameGraph](https://github.com/brendangregg/FlameGraph) compatible
format.

This is similar to
[ghc-prof-flamegraph](https://hackage.haskell.org/package/ghc-prof-flamegraph)
but only supports JSON format. If you want to process profiling output
from regular `-p`, `-P` &c., please try using that instead.
