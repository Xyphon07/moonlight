module SiteBench
  ( siteBenchmarks,
  )
where

import SiteManifest (siteManifestBenchmarks)
import SitePathQuotient (sitePathQuotientBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup)

siteBenchmarks :: Benchmark
siteBenchmarks =
  bgroup
    "site"
    [ bgroup
        "Site API"
        [ siteManifestBenchmarks,
          sitePathQuotientBenchmarks
        ]
    ]
