# Protestantism paper

Code for "Education or Ethic? The Long-Term Impact of Protestantism on Electoral
Behaviour in France" (Amaro & Sattelmayer). Run everything from the project
root (open `Protestantism-Paper.Rproj`).

## Data

Only the INSEE COG tables (`cog/`) are in the repository. Everything else has
to sit in the project folder locally:

- `Protestants by Commune - 1839.xlsx`, `communes-france-2025.csv`
- `albertus_gay/` and `gay/` (Albertus & Gay, Jean Nicolas database)
- `cage_piketty/`: the control files used in `code.Rmd` and the legislative
  election files from https://unehistoireduconflitpolitique.fr, with the folder
  names of the download. For the historic part these are 1848-1988:
  `leg1848comm.csv`, `leg1885comm.csv`, `leg1958comm.csv` ... `leg1988comm.csv`
  directly in `cage_piketty/`, and `leg1849_csv/leg1849comm.csv`,
  `leg1876_csv/` ... `leg1956_csv/`, `leg1946Juin_csv/leg1946comm.csv`,
  `leg1946Nov_csv/leg1947comm.csv` as subfolders (exact paths in
  `build-party-dictionary.R`)
- `scraped-churches/churches_fpf_insee.csv` for `church-proximity.R`: not in the
  repository because it contains contact details of pastors. Rebuild it with
  `scrape_fpf_churches.py` and `add_insee_codes.py` or ask Luis for the file.

R packages: tidyverse, readxl, estimatr, broom, modelsummary, mgcv, Matrix,
dbscan.

## Contemporary analysis (1993-2022)

`code.Rmd`: cleaning of the 1839 survey, RN models, sequential g-estimation,
mediation, sensitivity, weighting, RDD.

## Historic elections (1848-2022)

Run in this order:

1. `build-protestant-data.R` -> `clean_protestant_1839.csv`
2. `build-party-dictionary.R` -> `tables/party_family_dictionary.csv`
3. `build-historic-panel.R` -> `historic_panel.rds`
4. analysis scripts (all source `historic-setup.R`):
   - `historic-party-groups.R`: coefficient per election for our eight party
     groups (`figures/coef_per_year_party_groups_std.png`)
   - `historic-elections-modeling.R`: right bloc and anti-republican right,
     tables per Republic and year-by-year figures
   - `historic-cp-blocs.R`: the five Cage-Piketty blocs as benchmark
   - `historic-turnout.R`, `historic-persistence.R`, `historic-generality.R`
   - `historic-spatial-spline.R`, `historic-kelly-noise.R`,
     `historic-conley.R` (spatial robustness, the Conley script is slow)

`church-proximity.R` adds the distance to today's Protestant churches
(`scraped-churches/`) to the contemporary RN models.

## Note on the 1839 data

`build-protestant-data.R` applies the cleaning rules of `code.Rmd` but fixes a
few things that `clean_protestant_cp.csv` (the `d_clean` object of `code.Rmd`)
still has: 56 surveyed communes that are dropped (among them Vialas, Vebron,
Saint-Agreve, Lafitte), 293 communes listed twice (once as 0), Tarn and
Tarn-et-Garonne as well as Savoie, Haute-Savoie and the County of Nice coded
as 0 instead of NA, and shares above 100 set to NA instead of top-coded. The
details are at the top of the script. `code.Rmd` has not been switched to the
new file yet.
