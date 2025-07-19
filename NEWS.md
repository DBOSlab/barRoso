---
title: "News"
format:
  html:
    toc: true
    toc-depth: 3
---

<style>
  body {
    position: relative;
  }

  .watermark {
    position: fixed;
    bottom: 50px;
    right: 50px;
    opacity: 0.15;
    z-index: -1;
    width: 400px;
  }
</style>

<img src="/figures/barroso_hex_sticker.png" class="watermark" alt="barroso watermark">


# barroso 1.0.0

## Initial Release

The first official release of the `barroso` R package — a comprehensive toolkit for standardizing, harmonizing, and preparing plant specimen records for research and reconciliation.

### Highlights

- `barroso_std()`: Unified function to clean and standardize herbarium records across multiple fields (collector, geography, taxonomy, etc.).
- `barroso_flag_duplicates()`: Flag potential duplicate specimens across herbaria using metadata patterns.
- `barroso_cat()`: Combine and reconcile specimen records from multiple virtual herbaria (e.g., GBIF, JABOT, speciesLink).
- `barroso_labels()`: Generate printable herbarium labels from cleaned fieldbook data, with embedded maps and taxonomic authority retrieval.
- Flexible support for multilingual column names and Darwin Core standards.
- Optimized for datasets from [REFLORA](https://floradobrasil.jbrj.gov.br/reflora/herbarioVirtual/), [speciesLink](https://specieslink.net), and [JABOT](https://jabot.jbrj.gov.br/v3/consulta.php).
- Supports integration with tidyverse workflows for downstream analyses.
- Test coverage >95%, continuous integration via GitHub Actions.

### Philosophy

Unlike other tools that **aggressively clean (and discard)** records, `barroso` focuses on **standardization** first — ensuring that all specimens, even misidentified or ambiguous ones, remain usable and discoverable. Standardization also enables better duplicate detection and data reconciliation **without losing valuable information**.

### Infrastructure

- MIT license.
- GitHub Actions: R-CMD-check, test coverage, continuous integration.
- Website: [barroso documentation site](https://dboslab.github.io/barroso-website/)

### Feedback

Please report bugs or feature requests here:  
<https://github.com/DBOSlab/barroso/issues>
