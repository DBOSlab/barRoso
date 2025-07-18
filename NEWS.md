# barroso 1.0.0

## Initial Release

The first official release of the `barroso` R package, designed to streamline access to plant specimen data from the JABOT hosted by the Rio de Janeiro Botanical Garden.

### Features

- `barroso_std()`: Retrieve metadata and summary info from all or specific JABOT herbaria.
- `barroso_flag_duplicates()`: Download original specimen data in Darwin Core Archive (DwC-A) format.
- `barroso_labels()`: Parse, filter, and organize JABOT records based on taxon, herbarium, region, and year.
- Optional filters by `taxon`, `herbarium`, `state`, `recordYear`, and `level`.
- Supports integration with tidyverse workflows for downstream analyses.
- Test coverage >95%, continuous integration via GitHub Actions.

### Infrastructure

- MIT license.
- GitHub Actions: R-CMD-check and test coverage.
- Hosted documentation: [herbCur-website](https://dboslab.github.io/barroso-website/)

### Feedback

Please report bugs or issues at:  
<https://github.com/DBOSlab/barroso/issues>
