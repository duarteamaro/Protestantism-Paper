## build-language-1886.R
##
## Share of French speakers in 1886 per commune (Historical Ethnic Geography,
## Muller-Crepon, Schvitz and Cederman), averaged over the commune polygon as
## in code.Rmd. Written to language_1886.csv; used as the centre-periphery
## proxy in historic-cleavages.R.
##
## hegdata is not on CRAN: remotes::install_github("carl-mc/hegdata")

library(tidyverse)
library(sf)
library(terra)
library(hegdata)

communes <- read_sf("commune-frmetdrom/COMMUNE_FRMETDROM.shp") |>
  filter(!INSEE_DEP %in% c("971", "972", "973", "974", "976"))

french <- hegdata$new()$loadHEGGroup(group = "french", year = 1886) |>
  rast()

crs(french) <- "EPSG:4326"

language <- tibble(
  code_insee = communes$INSEE_COM,
  french_1886 = 100 * terra::extract(french, vect(communes), fun = mean, na.rm = TRUE)[, 2]
)

write_csv(language, "language_1886.csv")
