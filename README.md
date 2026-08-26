# semanticprimeR

This package supports the SPAML (Semantic Priming  Across Many Languages) project and associated papers that are connected to that project. The package currently includes information on how to calculate sample size by using AIPE,  bootstrapping, and simulation. More coming soon!

Cite this package:

[![DOI](https://zenodo.org/badge/280236106.svg)](https://zenodo.org/doi/10.5281/zenodo.10697999)

Buchanan, E. M. (2024). _semanticprimeR: Semantic Priming Across Many Languages and Related Projects_. R package version v0.0.2. doi: 10.5281/zenodo.10698000

## Installation

```
library(devtools)
install_github("SemanticPriming/semanticprimeR")
```

## Package Website

https://semanticpriming.github.io/semanticprimeR/

## Landing Page

Check out our [OSF landing page](https://osf.io/peyqh/) that links all these projects together. 

## Data releases

Datasets are too large to ship inside the R package, so `import_lab()` downloads them from this repo's GitHub releases at runtime instead. Package releases (code, functions, bug fixes) and data releases (new/updated datasets) are independent and tagged separately:

- Data releases are tagged `data-vX.Y.Z` (e.g. `data-v0.0.2`). Each one is an immutable snapshot of `datasets/completed/`, DOI-linked via Zenodo, that `import_lab()` can download from.
- Package releases keep their normal `vX.Y.Z` tags for code changes.
- By default, `import_lab()` downloads from the most recent `data-*` release (`release = "latest"`, the default). Pass a specific tag, e.g. `import_lab(bibtexID = "Birchenough2017", release = "data-v0.0.1")`, to pin to an exact snapshot for reproducibility (recommended when citing data in a paper).
- The very first data release predates this convention and is tagged plainly `v0.0.1` — `import_lab()` falls back to it automatically if no `data-*` release is found or the GitHub API lookup fails (e.g. offline).

**Maintainers:** adding or updating a dataset does *not* require a new package version — cut a new `data-vX.Y.Z` GitHub release (with a matching Zenodo DOI) whenever `datasets/completed/` changes, independent of the package's own release cycle. Since `import_lab()` resolves "latest" dynamically, no code change is needed after a new data release — the pin only needs touching if you want a *default* other than "most recent."

## Related projects

- SPAML: 
    - Preprint: https://osf.io/q4fjy/
    - Published manuscript: https://www.nature.com/articles/s41562-025-02254-x 
    - GitHub: https://github.com/SemanticPriming/SPAML
    - Official Dataset: https://zenodo.org/records/15021515 

- Powering Stimuli: coming soon 
