# ghc-prof-aeson-flamegraph [![Build Status][travisbadge]][travisurl] [![Build Status][appbadge]][appurl]

Turn GHC's JSON profiling output into
[FlameGraph](https://github.com/brendangregg/FlameGraph) compatible
format.

This is similar to
[ghc-prof-flamegraph](https://hackage.haskell.org/package/ghc-prof-flamegraph)
but only supports JSON format. If you want to process profiling output
from regular `-p`, `-P` &c., please try using that instead.

[travisbadge]: https://travis-ci.org/Fuuzetsu/ghc-prof-aeson-flamegraph.svg?branch=master
[travisurl]: https://travis-ci.org/Fuuzetsu/ghc-prof-aeson-flamegraph
[appbadge]: https://ci.appveyor.com/api/projects/status/github/Fuuzetsu/ghc-prof-aeson-flamegraph?branch=master&svg=true
[appurl]: https://ci.appveyor.com/project/Fuuzetsu/ghc-prof-aeson-flamegraph
