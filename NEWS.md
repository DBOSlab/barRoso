## News

# barRoso 1.0.0

## Initial Release

The first official release of the `barroso` R package — a comprehensive toolkit for standardizing, harmonizing, and preparing plant specimen records for research and reconciliation.

### Highlights

- `barroso_std()`: Unified function to clean and standardize herbarium records across multiple fields (collector, geography, taxonomy, etc.).
- `barroso_flag_duplicates()`: Flag potential duplicate specimens across herbaria using metadata patterns.
- `barroso_cat()`: Combine and reconcile specimen records from multiple virtual herbaria (e.g., GBIF, SEINet, REFLORA, JABOT, speciesLink).
- `barroso_labels()`: Generate printable herbarium labels from cleaned fieldbook data, with embedded maps and taxonomic authority retrieval.
- Standardize collector names and collection numbers using regex-based parsing
- Harmonize taxonomic, geographic, and temporal fields
- Flag and remove potential duplicates across herbarium records
- Generate herbarium labels from fieldbook data
- Integrate with external taxonomic databases (e.g., LCVP, WFO)
- Prepare large-scale biodiversity datasets for publication and analysis
- Optimized for datasets from [REFLORA](https://floradobrasil.jbrj.gov.br/reflora/herbarioVirtual/), [speciesLink](https://specieslink.net), and [JABOT](https://jabot.jbrj.gov.br/v3/consulta.php).
- Supports integration with tidyverse workflows for downstream analyses.
- Test coverage >95%, continuous integration via GitHub Actions.

### Philosophy

Unlike other tools that **aggressively clean (and discard)** records, `barroso` focuses on **standardization** first — ensuring that all specimens, even misidentified or ambiguous ones, remain usable and discoverable. Standardization also enables better duplicate detection and data reconciliation **without losing valuable information**.

### Infrastructure

- MIT license.
- GitHub Actions: R-CMD-check, test coverage, continuous integration.
- Website: [barRoso documentation site](https://dboslab.github.io/barRoso-website/)

### Feedback

Please report bugs or feature requests here:  
<https://github.com/DBOSlab/barRoso/issues>
