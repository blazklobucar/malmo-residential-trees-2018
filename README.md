# Trees on residential land in Malmö, Sweden (2018)

Tree inventory data from 200 randomly sampled points on residential land in Malmö, collected in September–November 2018 by the Swedish University of Agricultural Sciences (SLU) in cooperation with the City of Malmö.

| | Single-household (småhus) | Multi-dwelling |
| --- | --- | --- |
| Inventory unit | Whole lot | Circular plot, 100 m² |
| Units inventoried | 114 lots | 86 plots |
| Trees | 910 | 55 |
| Variables | Species, stem diameter | Full i-Tree Eco tree and plot variables |

The design, field methods, processing and results are described in the accompanying report: *[report title, series and DOI — to be added]*.

## Contents

| File | One row per | Rows | Content |
| --- | --- | --- | --- |
| `data/plots.csv` | Sample point | 226 | Housing type, outcome, inventory unit and area, postal area, field month, tree count, plot canopy cover |
| `data/trees.csv` | Tree | 965 | Species as recorded, taxon, genus, i-Tree code, stem count, equivalent diameter; crown variables for multi-dwelling trees |
| `data/stems.csv` | Stem | 1 633 | Diameter of each measured stem |
| `data/species_lookup.csv` | i-Tree code | 89 | Interpreted taxon; `verify = TRUE` marks codes not confirmed against the i-Tree species list |
| `data/data_dictionary.csv` | Variable | – | Definition, type and unit of every variable |
| `qc/qc_flags.csv` | Issue | 16 | Data issues found during processing and how each was handled |
| `R/build_malmo_ruf_dataset.R` | – | – | Script that produced the tables from the raw field files |

Tables join on `plot_id` (plots, trees) and `tree_id` (trees, stems). Missing values are empty cells; text is UTF-8.

## Notes for users

- **Different inventory units.** Single-household lots were inventoried in full; multi-dwelling land was sampled with 100 m² plots. Compare the two as trees per hectare (`n_trees` / `plot_area_m2`), not as trees per unit.
- **No crown data for single-household trees.** Only species and diameter were measured on single-household lots; height and crown fields are empty for those trees.
- **Missing records.** A 100 m² plot was also measured at single-household points and used in Klobucar et al. (2021b), but these plot records were not found in the archived source files and are not included.
- **Diameter.** `dbh_eq_cm` is the equivalent single-stem diameter, √(Σ dᵢ²), over up to five measured stems; individual stems are in `stems.csv`. Minimum recorded diameter was 5 cm.

## Privacy

The data are pseudonymised. Sample points carry random identifiers unrelated to the field numbering; location is given only as a three-digit postal area, with areas holding fewer than five points pooled as `other`; lot area is rounded to 50 m². Owner names, addresses, coordinates and contact details are not published. The `R/` script documents the full processing but needs the raw field files, which are held by the author at SLU. The random identifiers are a permutation fixed by a private seed (`RUF_PUBLIC_SEED`) that is deliberately not published, so a rerun with another seed gives different identifiers.

The homeowner questionnaire data (98 linked responses) are not included in this repository. [Add how to request them.]

## Publications using these data

- Klobucar, B., Östberg, J., Wiström, B., Jansson, M., 2021a. Residential urban trees – socio-ecological factors affecting tree and shrub abundance in the city of Malmö, Sweden. *Urban Forestry & Urban Greening* 62, 127118. https://doi.org/10.1016/j.ufug.2021.127118
- Klobucar, B., Sang, N., Randrup, T.B., 2021b. Comparing ground and remotely sensed measurements of urban tree canopy in private residential property. *Trees, Forests and People* 5, 100114. https://doi.org/10.1016/j.tfp.2021.100114

## Licence and citation

Data: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Code: MIT. Cite the dataset as in `CITATION.cff`.

The fieldwork was funded by Formas, the Swedish Research Council for Sustainable Development (project 2016-01278).

Contact: Blaz Klobucar, SLU, blaz.klobucar@slu.se
