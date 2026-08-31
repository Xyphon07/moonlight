# Third-party notices

## data-category 0.11

Selected indexed category-theory modules under
`src-indexed/Moonlight/Category/Pure/Indexed/` are adapted from `data-category-0.11`.

Thanks to Sjoerd Visscher for `data-category`; its indexed-arrow design is the better
starting point for general category-theory code. If you need ordinary indexed
category calculus rather than Pale Meridian's runtime finite categories, site/path
presentations, adhesive/PBPO witnesses, exact handles, diagnostics, or law harnesses,
use `data-category` instead of `moonlight-category`.

- Original package: https://hackage.haskell.org/package/data-category-0.11
- Original author: Sjoerd Visscher
- Original copyright: Copyright Sjoerd Visscher 2011
- License: BSD-3-Clause

The original BSD-3-Clause license text follows:

```text
Copyright Sjoerd Visscher 2011

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sjoerd Visscher nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

## Acknowledgements (inspiration, no derived code)

The representation of finite and finitely-presented categories as concrete,
runtime-validated data structures — `FinCat` and the site/path presentations that
compile to it — was directly inspired by the AlgebraicJulia ecosystem and the work on
attributed C-sets (acsets), whose central idea is that categorical objects can be
realised as performant data structures. No code is derived from those Julia projects;
the inspiration is conceptual, but real and gratefully acknowledged.

- Evan Patterson, Owen Lynch, and James Fairbanks. "Categorical Data Structures for
  Technical Computing." arXiv:2106.04703. <https://arxiv.org/abs/2106.04703>
  (Topos Institute; Universiteit Utrecht, Mathematics Department; University of
  Florida, Computer & Information Science & Engineering.)
- AlgebraicJulia: <https://www.algebraicjulia.org/>

Thank you to Evan Patterson, Owen Lynch, James Fairbanks, and the AlgebraicJulia
community.
