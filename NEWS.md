## News

# barRoso (development version)

### New functions

- `std_identifiedBy()`: standardizes determiner names in `identifiedBy` with the same rules of `std_recordedBy()`, keeping all determiners in the same cell (e.g. `"D. Cardoso & L. P. Queiroz"`) and removing herbarium acronyms and determination dates. It is now part of `barroso_std()` (new argument `colname_identifiedBy`).
- `std_coordinates()`: fixes transposed coordinates and flags empty, out of range, country-inconsistent and low precision coordinates, as well as coordinates at capitals, centroids, institutions and zero coordinates, following the workflow of the bdc package. Flags use the bdc columns (e.g. `.coordinates_empty`), plus a readable `coordinateIssues` column.

- `dms_to_decimal()`: converts coordinates in degrees, minutes and seconds (e.g. `"22º22´55´´S"`, `"53°45'W"`, `"43º40'49''O"`) into decimal degrees, recognizing the many symbols and hemisphere letters (also in Portuguese) found in herbarium data. Invalid values (e.g. 61 seconds) become `NA`, with a warning.

### Improvements

- `barroso_cat()`: standardizes records of REFLORA and JABOT (as returned by refloraR and jabotR): the determiner `"Administrador"` of REFLORA becomes `NA`, dates become ISO 8601 (e.g. `"2/12/1940"` into `"1940-12-02"`), REFLORA type status in Latin becomes Darwin Core terms (`"ISOTYPUS"` into `"isotype"`), and the institution of RB becomes `"JBRJ"`.

- `std_recordedBy()` was refactored into small internal helpers (about half of the code), with the same results for all non-Chinese names of the Ormosia GBIF and speciesLink test datasets, and about twice as fast.
- `std_recordedBy()`: Chinese names in unicode escapes are now converted into abbreviated Latin names (e.g. `"G. Z. Li"`, as Chinese authors cite themselves), without the `tmcn` package, which is no longer a dependency.
- `std_recordedBy()`: fixed capitalization of accented names (e.g. `"DuséN"`), initials followed by particles (`"Martius, CFP von"`), three collectors like `"A. Gómez Pompa, A. J. Sharp & P. Hernández"`, remains of `"et al."`, HTML entities, and abbreviations that depended on the other names in the data.
- `barroso_flag_duplicates()`: groups duplicates into blocks (`duplicateGroup`), flags blocks with conflicting identifications (`identificationConflict`, `duplicateIdentifications`), and stores all herbaria and catalog numbers of each block (`duplicateCollectionCodes`, e.g. `"MO | JBB"`; `duplicateCatalogNumbers`, e.g. `"MO 2839102 | JBB 13548"`). With `rm_duplicates = TRUE`, the record kept is identified to species, has the most frequent identification of the block, and then has coordinates and more filled columns. Records with unknown collector are no longer grouped as duplicates.
- `barroso_cat()`: renames raw speciesLink columns into Darwin Core terms, combines columns with different types, adds a `datasource` column, and with `keep_source` removes only the herbaria also present in the preferred source.

### Bug fixes

- `std_collection()`: with custom column names, the herbarium column was renamed wrongly, so no cleaning was done, and the `genus` column was renamed.
- `std_place()`: the original municipality and locality columns were saved as `countyOriginal.1` and `countyOriginal.2`; they are now `municipalityOriginal` and `localityOriginal`.
- `std_place()`: Venezuela was renamed "Venezula", and continents were not added when the continent column was completely empty.
- `std_place()`: USA state acronyms were never expanded, and their table was misaligned (e.g. `AK` would become `Alabama`); Brazilian acronyms were applied to records of all countries (e.g. `AL` in the USA became `Alagoas`); US records got no continent; and with a custom country column, states and continents were not standardized.
- `barroso_write_taxon_descr()`: trait values `black`, and whole numbers from 8 to 16 stored as numbers (e.g. 10 stamens), were silently dropped from the descriptions. A `filename` with the `.docx` extension is now saved in the output folder.
- `barroso_write_specimens()`: failed with any dataset with coordinates; missing type status, month, number and municipality were printed as `NA`; coordinates could show `60″`; and duplicates with different type status now keep each one, e.g. `[holotype HUEFS, isotype RB]`.
- `barroso_labels()`: a specimen not found in LCVP (e.g. `Ormosia sp.`) got the name of the previous label; authorities of infraspecific names are shown correctly.
- `barroso_std()`: failed with more than 10,000 records when the last chunk had a single record.
- `std_recordedBy()`: numbers before the collector name (e.g. `8470 G.H. Turner`) are moved to `recordNumber`.
- `std_taxa()`: `sp.`, `cf.` and `aff.` at the beginning of the epithet, and genera in capital letters, are now standardized.
- `remove_authorship()`: removes all authors, e.g. `Ormosia arborea (Vell.) Harms` and `Swartzia apetala Raddi var. glabra (Vogel) R.S.Cowan`, so `species_filter` now matches these names.

### Tests

- Test coverage increased from 35% to 94%, with tests for all exported functions.

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
