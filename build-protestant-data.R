## build-protestant-data.R
##
## Cleans the hand-transcribed 1839 survey of the Reformed churches and writes
## one row per commune (2025 frame) to clean_protestant_1839.csv.
##
## The cleaning rules are the ones documented in code.Rmd. Differences to the
## d_clean object built there (and saved as clean_protestant_cp.csv):
##   - rows carrying a transcription note are kept (code.Rmd drops 56 surveyed
##     communes this way, e.g. Vialas, Vebron, Rousses, Lafitte, Mauvezin)
##   - Ardeche codes written without the leading zero ("7204") are matched
##   - Bouches-du-Rhone joint counts read "among these communes", not "spread over"
##   - Rocroi: 60 Protestants (as in the text), not 50
##   - Saint-Frezal-de-Ventalon is 48152, not 48175
##   - La Grand-Combe / Les Salles-du-Gardon get their 1839 population
##   - one row per commune (code.Rmd lists 293 communes twice, once as a 0)
##   - NA instead of 0 for Tarn and Tarn-et-Garonne (transcribed, not yet
##     cleaned), and for Savoie, Haute-Savoie and the County of Nice (not French
##     in 1839)
##   - shares above 100 are top-coded at 100 instead of set to NA. This happens
##     in 102 strongholds where the Cage-Piketty estimate of the 1839 population
##     is below the survey's Protestant count. Collorgues stays NA: there the
##     survey itself lists more Protestants than inhabitants.

library(tidyverse)
library(readxl)

pop_cp <- read_csv("cage_piketty/popcommunes.csv", show_col_types = FALSE) |>
  dplyr::select(dep, nomdep, code_insee = codecommune, nomcommune, pop1839) |>
  distinct()

survey_file <- "Protestants by Commune - 1839.xlsx"

survey <- bind_rows(
  read_xlsx(survey_file, sheet = 1, col_types = "text") |> mutate(obs = ...16),
  read_xlsx(survey_file, sheet = 2, col_types = "text") |> mutate(obs = ...15)
) |>
  filter(!(Departement %in% c("Tarn", "Tarn et Garonne"))) |>
  transmute(Departement, Communes, obs,
            code_insee = str_pad(`Code INSEE`, width = 5, pad = "0"),
            pop_general = as.numeric(`Population générale`),
            pop_text = `Population Protestante`,
            pop_protestant = as.numeric(pop_text))

## the two parts of Les Salles: left bank = La Grand-Combe (created 1846)
survey <- survey |>
  mutate(code_insee = case_when(
    str_detect(Communes, "rive gauche du Gardon") ~ "30132",
    str_detect(Communes, "rive droite du Gardon") ~ "30307",
    .default = code_insee
  ))


# joint counts --------------------------------------------------------------

## Protestants reported for a group of communes are spread proportionally to
## the 1839 population (Cage-Piketty estimates)
spread_count <- function(codes, total) {
  pop_cp |>
    filter(code_insee %in% codes) |>
    transmute(code_insee, pop_protestant = round(total * pop1839 / sum(pop1839)))
}

rocroi <- c("08367", "08071", "08078", "08110", "08121", "08155", "08202",
            "08249", "08282", "08355", "08365", "08417", "08436", "08460")

ardennes_180 <- survey |>
  filter(Departement == "Ardennes", pop_text == "180 spread over these communes") |>
  pull(code_insee)

indre_et_loire <- pop_cp |>
  filter(dep == "37", code_insee != "37261") |>
  pull(code_insee)

joint <- bind_rows(
  spread_count(ardennes_180, 180),
  spread_count(c("08362", "08105"), 50),
  spread_count(rocroi, 60),
  spread_count(c("13084", "13024"), 335),
  spread_count(c("13100", "13035", "13004"), 90),
  spread_count(c("21038", "21054", "21464"), 100),
  spread_count(indre_et_loire, 150),
  spread_count(c("42044", "42095"), 60),
  spread_count(c("42207", "42186"), 65),
  ## Tournon and its neighbours, split with the 1836 census (see code.Rmd)
  tibble(code_insee = c("07324", "07152", "07245"), pop_protestant = c(91, 21, 18))
)


# communes listed more than once ----------------------------------------------

## double counts: keep the lower figure (Arvert, Pommiers, Orne ...), or the
## higher one for Cherbourg. Remuzat, Peronville and Ronssoy have only one
## usable figure.
dup_min <- c("17021", "26264", "30199", "50128", "61007", "61169", "61001",
             "61407", "61044", "61287", "61177", "28296", "80679")
dup_max <- "50129"

## the row to trust when two entries disagree
dup_pick <- tribble(
  ~code_insee, ~pick_pop_general,
  "17219", 4942,  # Marennes
  "26064", 4295,  # Chabeuil
  "17342", 812,   # Saint-Germain-du-Seudre
  "30176", 715,   # Montdardier
  "07184", 1775   # Pranles
)

## everything else that shares a code is summed: communes merged since 1839
## (Saint-Agreve + Le Pouzat, Niort ...) and sections of the same commune
## (Gardonne, Barr, Saint-Frezal-de-Ventalon ...)
survey_commune <- survey |>
  filter(!is.na(code_insee), !code_insee %in% joint$code_insee) |>
  left_join(dup_pick, by = "code_insee") |>
  filter(is.na(pick_pop_general) | pop_general == pick_pop_general) |>
  filter(!is.na(pop_protestant)) |>
  group_by(code_insee) |>
  summarise(n = n(),
            p_min = min(pop_protestant),
            p_max = max(pop_protestant),
            p_sum = sum(pop_protestant)) |>
  mutate(pop_protestant = case_when(
    code_insee %in% dup_min ~ p_min,
    code_insee %in% dup_max ~ p_max,
    .default = p_sum
  )) |>
  dplyr::select(code_insee, pop_protestant) |>
  bind_rows(joint)


# all communes ----------------------------------------------------------------

## departments where the survey itself says coverage is incomplete
incomplete_deps <- c("01", "02", "14", "50", "25", "45", "24")

## Seine and the metropole of Bordeaux: the survey gives no usable count
seine_deps <- c("75", "92", "93", "94")
bordeaux <- c("33063", "33003", "33004", "33013", "33032", "33039", "33056",
              "33065", "33069", "33075", "33096", "33119", "33162", "33167",
              "33192", "33200", "33249", "33273", "33281", "33312", "33318",
              "33376", "33434", "33449", "33487", "33519", "33522", "33550")

## not covered: Tarn and Tarn-et-Garonne (not yet cleaned) and territories
## that were not French in 1839 (County of Nice = arrondissement of Nice)
nice <- read_csv("cog/v_commune_2025.csv", show_col_types = FALSE) |>
  filter(TYPECOM == "COM", ARR == "062") |>
  pull(COM)
uncovered_deps <- c("81", "82", "73", "74")

d_prot <- pop_cp |>
  left_join(survey_commune, by = "code_insee") |>
  mutate(
    category = if_else(!is.na(pop_protestant), "Original", "Extrapolated"),
    pop_protestant = case_when(
      dep %in% c(seine_deps, uncovered_deps) ~ NA_real_,
      code_insee %in% c(bordeaux, nice) ~ NA_real_,
      dep %in% incomplete_deps & is.na(pop_protestant) ~ NA_real_,
      is.na(pop_protestant) ~ 0,
      .default = pop_protestant
    ),
    share_protestant = pmin(100 * pop_protestant / pop1839, 100),
    share_protestant = if_else(code_insee == "30086", NA_real_, share_protestant)
  )

write_csv(d_prot, "clean_protestant_1839.csv")
